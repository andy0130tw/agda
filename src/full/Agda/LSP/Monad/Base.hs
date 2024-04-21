{-# LANGUAGE RoleAnnotations #-}
module Agda.LSP.Monad.Base where

import Prelude hiding (null)

import Control.Concurrent.MVar
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative (liftA2, asum)
import Control.Concurrent
import Control.Monad

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.List (find)
import Data.Aeson
import Data.Maybe
import Data.IORef

import GHC.Generics

import qualified Language.LSP.Protocol.Message as Lsp
import qualified Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Types (NormalizedUri, Uri, fromNormalizedUri)
import Language.LSP.Protocol.Lens (start, end)
import Language.LSP.VFS (VirtualFile)
import Language.LSP.Server

import Agda.Syntax.Position
import Agda.Syntax.Common

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty

import qualified Agda.Utils.BiMap as BiMap

import Agda.LSP.Monad.Config
import Agda.LSP.Position
import Agda.Interaction.FindFile (SourceFile(..))
import Agda.Interaction.Options
import Agda.Utils.FileName (absolute)
import Agda.Utils.Null
import Data.String
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Functor
import Agda.Utils.Maybe (fromMaybe, mapMaybe)
import Agda.Utils.Lens


data PositionInfo = PositionInfo
  { posInfoDelta        :: PosDelta
  , posInfoInteractions :: [Lsp.Range]
  }

data Worker = Worker
  { workerUri         :: !NormalizedUri
    -- ^ URI managed by this worker.

  , workerFilePath    :: FilePath
    -- ^ File path managed by this worker.

  , workerLoadedState :: !(MVar (IORef TCState))
    -- ^ Mutable variable for the “main” TC state in this worker.
  , workerPosInfo     :: !(MVar PositionInfo)
  , workerSnapshot    :: !(MVar (Maybe StateSnapshot))

  , workerThread      :: ThreadId
    -- ^ Thread for this worker.

  , workerTasks       :: Chan (Task ())

  , workerContext     :: LanguageContextEnv LspConfig

  , workerOptions     :: CommandLineOptions
  }

-- | A snapshot of the current editing state.
data StateSnapshot = StateSnapshot
  { snapshotText     :: Text
  , snapshotTc       :: TCState
  , snapshotPosition :: PosDelta
  }

data LspState = LspState
  { lspStateConfig  :: LanguageContextEnv LspConfig
  , lspStateWorkers :: MVar (HashMap NormalizedUri Worker)
  , lspStateOptions :: CommandLineOptions
  }

newtype WorkerM a = WorkerM { unWorkerM :: ReaderT LspState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader LspState)

instance MonadLsp LspConfig WorkerM where
  getLspEnv = asks lspStateConfig

newtype Task a = Task { unTask :: ReaderT Worker TCM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTCM, HasOptions, MonadTCEnv, MonadTCState, MonadDebug, ReadTCState, HasConstInfo, MonadFail, HasBuiltins, MonadError TCErr, MonadTrace, MonadReduce, MonadFresh NameId, MonadInteractionPoints, MonadStConcreteNames, MonadAddContext, PureTCM)

instance MonadUnliftIO Task where
  withRunInIO k = Task $ ReaderT \worker -> TCM \state env -> k \task -> unTCM (runReaderT (unTask task) worker) state env

instance MonadLsp LspConfig Task where
  getLspEnv = Task (asks workerContext)

instance IsString a => IsString (Task a) where
  fromString s = pure (fromString s)

instance {-# OVERLAPPABLE #-} (Semigroup a) => Semigroup (Task a) where
  (<>) = liftA2 (<>)

-- | Strict (non-shortcut) monoid.
instance {-# OVERLAPPABLE #-} (Semigroup a, Monoid a) => Monoid (Task a) where
  mempty  = pure mempty
  mappend = (<>)
  mconcat = mconcat <.> sequence

instance {-# OVERLAPPABLE #-} Null a => Null (Task a) where
  empty = return empty
  null  = __IMPOSSIBLE__

getPositionInfo :: Task PositionInfo
getPositionInfo = liftIO . readMVar =<< Task (asks workerPosInfo)

getDelta :: Task PosDelta
getDelta = posInfoDelta <$> getPositionInfo

modifyDelta :: (PosDelta -> PosDelta) -> Task ()
modifyDelta k = do
  info <- Task (asks workerPosInfo)
  liftIO $ modifyMVar_ info (pure . \x -> x { posInfoDelta = k $ posInfoDelta x  })

setInteractionPointRanges :: [Lsp.Range] -> Task ()
setInteractionPointRanges i = do
  info <- Task (asks workerPosInfo)
  liftIO $ modifyMVar_ info (pure . \x -> x { posInfoInteractions = i })

theSourceFile :: Task SourceFile
theSourceFile = do
  file <- Task (asks workerFilePath)
  SourceFile <$> liftIO (absolute file)

theURI :: Task Uri
theURI = Task (asks (fromNormalizedUri . workerUri))

theVirtualFile :: Task (Maybe VirtualFile)
theVirtualFile = getVirtualFile . Lsp.toNormalizedUri =<< theURI

getInitialOptions :: Task CommandLineOptions
getInitialOptions = Task (asks workerOptions)

takeSnapshot :: Task (Maybe StateSnapshot)
takeSnapshot = do
  info <- Task (asks workerSnapshot)
  liftIO $ modifyMVar info (pure . (Nothing, ))

setSnapshot :: Maybe StateSnapshot -> Task ()
setSnapshot snapshot = do
  info <- Task (asks workerSnapshot)
  liftIO $ modifyMVar_ info (pure . const snapshot)

-- | Attempt to get the position of an interaction point, updated to the latest
-- position in the document.
getCurrentInteractionRange :: PositionInfo -> InteractionPoint -> Maybe Lsp.Range
getCurrentInteractionRange posInfo ip = do
  ival <- rangeToInterval (ipRange ip)

  let
    delta = posInfoDelta posInfo
    s = toUpdatedPosition delta (iStart ival)
    e = toUpdatedPosition delta (iEnd ival)

  asum
    [ pointAt (view start) =<< s
    , pointAt (view end) =<< e
    ]

  where
    pointAt f x = find ((== x) . f) (posInfoInteractions posInfo)

type InteractionPointInfo = (InteractionId, InteractionPoint, Lsp.Range)

-- | Get all interaction points in the current file, along with their range.
getActiveInteractionPoints :: Task [InteractionPointInfo]
getActiveInteractionPoints  = do
  ii <- useR stInteractionPoints
  posInfo <- getPositionInfo

  let
    go (ii, ip)
      | ipSolved ip = Nothing
      | otherwise = (ii, ip, ) <$> getCurrentInteractionRange posInfo ip

  pure . mapMaybe go $ BiMap.toList ii

-- | Find an active interaction point at the cursor.
findInteractionPoint :: Lsp.Position -> Task (Maybe InteractionPointInfo)
findInteractionPoint pos = find (\(_, _, r) -> Lsp.positionInRange pos r) <$> getActiveInteractionPoints

withPosition :: Lsp.Position -> (InteractionPoint -> Task a) -> Task (Maybe a)
withPosition pos cont = do
  containing <- findInteractionPoint pos
  case containing of
    Just (iid, ip, _) -> Just <$> withInteractionId iid (cont ip)
    Nothing -> pure Nothing

runLater :: Task () -> Task ()
runLater cont = do
  queue <- Task (asks workerTasks)
  liftIO $ writeChan queue cont
