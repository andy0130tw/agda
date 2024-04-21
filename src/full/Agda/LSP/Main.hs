{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Agda.LSP.Main (runAgdaLSP, serverDefinition) where

import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Except (MonadError(catchError))
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text.Lines as TextLines
import qualified Data.Text.Lazy as TL
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Strict.Tuple (Pair(..))
import Data.Aeson.Types as Aeson
import Data.Traversable
import Data.Foldable
import Data.Default
import Data.Coerce
import Data.Proxy
import Data.IORef
import Data.Maybe
import Data.List (find, sortOn, intercalate)
import Data.Char (isSpace)
import Data.Monoid

import GHC.Generics

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens
import Language.LSP.Server
import Language.LSP.VFS hiding (start, end, line, character)

import System.Exit
import System.IO

import qualified Text.PrettyPrint.Annotated.HughesPJ as Ppr
import qualified Text.PrettyPrint.Annotated as Ppr

import qualified Agda.Interaction.InteractionTop as ITop

import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Position as Agda

import Agda.Syntax.Translation.InternalToAbstract (Reify(reify), reifyUnblocked)
import Agda.Syntax.Translation.AbstractToConcrete
import Agda.Syntax.Abstract.Pretty hiding (prettyA)
import Agda.Syntax.Concrete.Pretty () -- Instances only
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Common (isInstance)
import Agda.Syntax.Scope.Base
import Agda.Syntax.Concrete (Module(modDecls))
import Agda.Syntax.Position
import Agda.Syntax.Internal

import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Monad as I
import Agda.TypeChecking.Opacity (saturateOpaqueBlocks)

import qualified Agda.Syntax.Parser as Pa
import qualified Agda.Syntax.Parser.Tokens as T

import Agda.LSP.Translation
import Agda.LSP.Diagnostic
import Agda.LSP.Position
import Agda.LSP.Utils
import Agda.LSP.Goal

import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.BasicOps as B
import Agda.Interaction.Highlighting.Precise
import Agda.Interaction.Highlighting.Common (toAtoms)
import Agda.Interaction.InteractionTop (highlightExpr)
import Agda.Interaction.Highlighting.Range (rangeToRange)
import Agda.Interaction.Highlighting.Generate (generateTokenInfoFromString)
import Agda.Interaction.Base (Rewrite(AsIs, Simplified), UseForce (WithoutForce))
import Agda.Interaction.Response.Base
import Agda.Interaction.FindFile (SourceFile(..))
import Agda.Interaction.BasicOps (getWarningsAndNonFatalErrors, normalForm)
import Agda.Interaction.JSONTop () -- Instances only
import Agda.Interaction.Options.Lenses
import Agda.Interaction.Options

import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import qualified Agda.Utils.Null as Null
import qualified Agda.Utils.BiMap as BiMap
import Agda.Utils.Tuple (fst3)

import Agda.Utils.Impossible
import Agda.Utils.FileName
import Agda.Utils.Either
import Agda.Utils.Monad
import Agda.Utils.Lens
import Agda.Syntax.Parser (runPMIO)
import Agda.Mimer.Mimer as Mimer
import qualified Agda.Syntax.Parser as P
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Syntax.Parser.Tokens (Token(TokSymbol), Symbol (SymQuestionMark))
import Agda.Syntax.Common (InteractionId, LensModality (getModality), LensHiding (getHiding), Relevance (Relevant), LensRelevance (getRelevance), inverseComposeRelevance, LensQuantity (getQuantity), moreQuantity)
import Agda.Syntax.Fixity (Precedence(TopCtx))

import Agda.LSP.Monad.Config
import Agda.LSP.Monad.Base
import Agda.LSP.Commands
import Agda.LSP.Output
import qualified Agda.LSP.Goal as Goal
import Agda.TypeChecking.Pretty.Warning (getAllWarnings, getAllWarningsPreserving)
import Agda.TypeChecking.Warnings (WhichWarnings(AllWarnings))
import Agda.Interaction.Options.Warnings (unsolvedWarnings)
import Agda.Interaction.MakeCase (makeCase)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder.Extra (defaultChunkSize)

syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TextDocumentSyncKind_Incremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just (InR (SaveOptions (Just False)))
  }

-- | Run the LSP server using stdin/stdout as input/output channels.
runAgdaLSP :: TCM () -> TCM ()
runAgdaLSP setup = liftIO do
  exc <- runServer $ serverDefinition setup
  case exc of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)

-- | Create an LSP server.
--
-- The supplied TCM value may be used to set up the initial command line options
-- of the TC state.
serverDefinition :: TCM () -> ServerDefinition LspConfig
serverDefinition setup = ServerDefinition
  { defaultConfig    = def
  , configSection    = "agda"
  , parseConfig      = \_ -> mapLeft Text.pack . parseEither parseJSON
  , onConfigChange   = const (pure ())
  , doInitialize     = lspInit setup
  , staticHandlers   = lspHandlers
  , interpretHandler = \state -> Iso (flip runReaderT state . unWorkerM) liftIO
  , options          = def
    { optTextDocumentSync = Just syncOptions
    , optDocumentOnTypeFormattingTriggerCharacters = Just ('?' :| [])
    , optExecuteCommandCommands = Just commandNames
    }
  }

lspOutputCallback :: Uri -> LanguageContextEnv LspConfig -> InteractionOutputCallback
lspOutputCallback uri config = liftIO . runLspT config . \case
  Resp_RunningInfo v s -> do
    lspDebug s
    when (v <= 1) $ sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/infoview/message")) $ Object $ KeyMap.fromList
      [ ("message", toJSON s)
      , ("uri", toJSON uri)
      ]
  _ -> pure ()

lspInit
  :: TCM ()
  -> LanguageContextEnv LspConfig
  -> TMessage 'Method_Initialize
  -> IO (Either ResponseError LspState)
lspInit setup config init = do
  workers  <- newMVar mempty

  (opts, _) <- runTCM initEnv initState do
    setup
    commandLineOptions

  pure $ Right LspState
    { lspStateConfig  = config
    , lspStateWorkers = workers
    , lspStateOptions = opts { optAbsoluteIncludePaths = [] }
    }

lspDebug :: MonadLsp cfg m => String -> m ()
lspDebug s = sendNotification SMethod_WindowLogMessage (LogMessageParams MessageType_Log (Text.pack s))

spawnOrGet :: Uri -> WorkerM (Maybe Worker)
spawnOrGet uri = withRunInIO \run -> case uriToFilePath uri of
  Just fp -> do
    state <- run ask
    let norm = toNormalizedUri uri
    modifyMVar (lspStateWorkers state) \workers -> case HashMap.lookup norm workers of
      Just worker -> pure (workers, Just worker)

      Nothing -> do
        contents <- run . getVirtualFile $ toNormalizedUri  uri
        let lines = maybe mempty (Rope.toTextLines . view file_text) contents

        lock     <- newMVar =<< newIORef initState
        deltas   <- newMVar (PositionInfo (createPosDelta lines) def)
        snapshot <- newMVar Nothing
        chan     <- newChan

        rec
          wthread <- forkIO . forever $ do
            task <- readChan chan

            let
              report :: TCErr -> TCM ()
              report terr = do
                it <- prettyTCM terr
                liftIO . run . lspDebug $ show it
                runT $ diagnoseTCM =<< toDiagnostic terr

              runT :: Task () -> TCM ()
              runT w = do
                setInteractionOutputCallback (lspOutputCallback uri (lspStateConfig state))
                conf <- liftIO (run getConfig)
                locallyTC eHighlightingLevel (const (lspHighlightingLevel conf)) do
                  runReaderT (unTask w) worker `catchError` report

            withMVar lock \state -> do
              unTCM (runT task) state initEnv
                `catch` \(e :: SomeException) -> run (lspDebug (show e))

          let
            worker = Worker
              { workerUri         = norm
              , workerFilePath    = fp
              , workerLoadedState = lock
              , workerThread      = wthread
              , workerTasks       = chan
              , workerContext     = lspStateConfig state
              , workerOptions     = lspStateOptions state
              , workerPosInfo     = deltas
              , workerSnapshot    = snapshot
              }

        run $ lspDebug $ "Spawned worker " <> show (workerThread worker) <> " to manage file " <> fp
        pure (HashMap.insert norm worker workers, Just worker)

  Nothing -> pure Nothing

runAtURI :: Uri -> Task a -> WorkerM ()
runAtURI uri task = spawnOrGet uri >>= \case
  Just Worker{workerTasks = chan} -> liftIO $ writeChan chan (void task)
  Nothing -> lspDebug $ "URI is not a file: " <> show uri

-- | Update the interaction ranges in the file.
updateInteractionRanges :: Text.Text -> Task ()
updateInteractionRanges text = do
  path <- theSourceFile

  (parseResult, _) <- liftIO . Pa.runPMIO $ Pa.parseFile Pa.tokensParser (RangeFile (srcFilePath path) Nothing) (Text.unpack text)
  setInteractionPointRanges . mapMaybe (fmap toLsp . findInteractionPointToken) $ either (const []) (fst . fst) parseResult

  refreshGoals

findInteractionPointToken :: T.Token -> Maybe Interval
findInteractionPointToken (T.TokSymbol T.SymQuestionMark ival) = Just ival
findInteractionPointToken _ = Nothing

onTextDocumentOpen :: Handlers WorkerM
onTextDocumentOpen = notificationHandler SMethod_TextDocumentDidOpen \notif ->
  runAtURI (notif ^. params . textDocument . uri) do
  reportSLn "lsp.lifecycle" 10 "document opened"
  updateInteractionRanges =<< maybe "" virtualFileText <$> theVirtualFile
  reloadURI

onTextDocumentSaved :: Handlers WorkerM
onTextDocumentSaved = notificationHandler SMethod_TextDocumentDidSave \notif ->
  whenM (lspReloadOnSave <$> getConfig) $
    runAtURI (notif ^. params . textDocument . uri) reloadURI

onTextDocumentChange :: Handlers WorkerM
onTextDocumentChange = notificationHandler SMethod_TextDocumentDidChange \notif ->
  runAtURI (notif ^. params . textDocument . uri) do
  reportSLn "lsp.lifecycle" 10 $ "document changed\n" <> unlines (map show (notif ^. params . contentChanges))

  snapshot <- takeSnapshot
  text <- maybe "" virtualFileText <$> theVirtualFile
  case snapshot of
    Just snapshot
      | snapshotText snapshot == text -> do
        liftTCM . putTC $ snapshotTc snapshot
        modifyDelta . const $ snapshotPosition snapshot
      | otherwise -> reloadURI
    _ -> modifyDelta $ flip posDeltaWithChangeEvents (notif ^. params . contentChanges)

  updateInteractionRanges text

onTextDocumentClosed :: Handlers WorkerM
onTextDocumentClosed = notificationHandler SMethod_TextDocumentDidClose \notif ->
  withRunInIO \run -> do
    let uri' = notif ^. params . textDocument . uri
    case uriToFilePath uri' of
      Just fp -> do
        state <- run ask
        let norm = toNormalizedUri uri'
        worker <- modifyMVar (lspStateWorkers state) \workers -> pure (HashMap.delete norm workers, HashMap.lookup norm workers)
        traverse_ (killThread . workerThread) worker

      Nothing -> pure ()

diagnoseTCM :: [Diagnostic] -> Task ()
diagnoseTCM diag = do
  uri <- theURI

  sendNotification SMethod_TextDocumentPublishDiagnostics $ PublishDiagnosticsParams
    { _uri         = uri
    , _version     = Nothing
    , _diagnostics = diag
    }

-- Reset the TC state inside a task.
resetTCState :: Task ()
resetTCState = do
  _ <- takeSnapshot

  opts <- getInitialOptions

  rootPath <- getRootPath
  workspaceFolders <- getWorkspaceFolders

  let
    root = rootPath <|> case workspaceFolders of
      Just [x] -> uriToFilePath (x ^. uri)
      _ -> Nothing

  liftTCM do
    resetState

    opts <- addTrustedExecutables opts
    case root of
      Nothing -> I.setCommandLineOptions opts
      Just path -> do
        absPath <- liftIO $ absolute path
        I.setCommandLineOptions' absPath opts

refreshClientInfo :: Task ()
refreshClientInfo = do
  void $ sendRequest SMethod_WorkspaceSemanticTokensRefresh Nothing (const (pure ()))
  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/infoview/refresh")) . toJSON =<< theURI

  refreshGoals

refreshGoals :: Task ()
refreshGoals = do
  diagnoseTCM =<< fold
    [ toDiagnostic =<< liftTCM (getAllWarnings AllWarnings)
    , toDiagnostic =<< getUnsolvedMetaVars
    ]

  goals <- fillQuery (Query_AllGoals False)
  uri <- theURI

  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/goals")) $ Object $ KeyMap.fromList
    [ ("goals", toJSON goals)
    , ("uri",   toJSON uri)
    ]

reloadURI :: Task ()
reloadURI = do
  resetTCState

  fileContents <- maybe mempty (Rope.toTextLines . view file_text) <$> theVirtualFile
  modifyDelta . const $ createPosDelta fileContents

  diagnoseTCM []

  sf <- theSourceFile
  src <- liftTCM $ Imp.parseSource' (const . pure . TL.fromStrict $ TextLines.toText fileContents) sf
  cr <- liftTCM $ Imp.typeCheckMain Imp.TypeCheck src

  reportSDoc "lsp.lifecycle" 10 $ "got warnings: " <+> prettyTCM (Imp.crWarnings cr)
  whenM (Null.null <$> useTC stSyntaxInfo) . setTCLens stSyntaxInfo . iHighlighting $ Imp.crInterface cr

  refreshClientInfo

isQuestion :: InteractionPoint -> Bool
isQuestion InteractionPoint{ipRange = r}
  | Just i <- rangeToInterval r, posPos (iEnd i) == posPos (iStart i) + 1 = True
isQuestion _ = False

requestHandlerTCM
  :: forall (m :: Method 'ClientToServer 'Request) a. SMethod m
  -> (TRequestMessage m -> Uri)
  -> (TRequestMessage m -> (Either ResponseError (MessageResult m) -> Task ()) -> Task a)
  -> Handlers WorkerM
requestHandlerTCM method uri cont = requestHandler method \req res -> withRunInIO \run -> run do
  runAtURI (uri req) $ void $ cont req \m -> liftIO (run (res m))

provideSemanticTokens :: Handlers WorkerM
provideSemanticTokens =
  requestHandlerTCM SMethod_TextDocumentSemanticTokensFull (view (params . textDocument . uri)) \req res -> do
  info <- useTC stSyntaxInfo

  delta <- getDelta
  let tokens = aspectMapToTokens delta info
  reportSLn "lsp.highlighting.semantic" 10 $ "returning " <> show (Prelude.length tokens) <> " semantic tokens"
  reportSLn "lsp.highlighting.semantic" 666 $ unlines
    [ show (range, r) <> ": " <> show (aspect asp) | (range, asp) <- RangeMap.toList info, let r = toUpdatedPosition delta range]

  case encodeTokens agdaTokenLegend (relativizeTokens (sortOn (\x -> (x ^. line, x ^. startChar)) tokens)) of
    Left  e    -> res $ Right (InR Lsp.Null)
    Right ints -> res $ Right $ InL (SemanticTokens Nothing ints)

onInitialized :: Handlers WorkerM
onInitialized = notificationHandler SMethod_Initialized \notif -> do
  sendNotification (SMethod_CustomMethod (Proxy :: Proxy "agda/highlightingInit"))
    $ Object $ KeyMap.singleton "legend" (toJSON agdaTokenLegend)

-- | Get a substring of a file based on an Lsp range.
getFileSubstring :: VirtualFile -> Lsp.Range -> Text.Text
getFileSubstring file (Lsp.Range (Lsp.Position sL sC) (Lsp.Position eL eC)) =
  let
    rope = file ^. file_text
    (_, prefix) = Rope.charSplitAtPosition (TextLines.Position (fromIntegral sL) (fromIntegral sC)) rope

    len = if sL == eL then eC - sC else eC
    (main, _) = Rope.charSplitAtPosition (TextLines.Position (fromIntegral (eL - sL)) (fromIntegral len)) prefix
  in
  Rope.toText main

-- | Get the contents of an interaction point, if still present in the document.
interactionContents :: InteractionPointInfo -> VirtualFile -> Maybe (Agda.Range, Int, Text.Text)
interactionContents (_, ip, lrange) file = do
  contents <- Text.stripPrefix "{!" <=< Text.stripSuffix "!}" $ getFileSubstring file lrange

  let
    range = toAgdaRange (file ^. file_text) (rangeFile (ipRange ip)) lrange
    (spaces, text) = Text.span isSpace contents
    offset = 2 {- "{!" -} + Text.length spaces

  offset `seq` pure
    ( modifyRangePositions (+ offset) (file ^. file_text) range
    , offset
    , Text.strip text
    )

printedTerm :: Rewrite -> Term -> TCM C.Expr
printedTerm rewr ty = do
  ty  <- normalForm rewr ty
  abstractToConcreteCtx TopCtx =<< reify ty

fillQuery :: Query a -> Task a
fillQuery (Query_GoalAt pos) = fmap fst3 <$> findInteractionPoint pos
fillQuery Query_ModuleName = fmap (printed . snd) <$> useTC stCurrentModule

fillQuery (Query_AllGoals t) = do
  iis <- getActiveInteractionPoints
  for iis \(id, ip, range) -> withInteractionId id do
    mi <- lookupInteractionId id
    ty <- if t
      then fmap (pure . printed) . liftTCM . printedTerm AsIs . unEl =<< getMetaTypeInContext mi
      else pure Nothing
    pure Goal
      { goalId    = id
      , goalType  = ty
      , goalRange = range
      }

fillQuery (Query_GoalInfo iid) = withInteractionId iid do
  mi <- lookupInteractionId iid
  ip <- lookupInteractionPoint iid
  posInfo <- getPositionInfo
  mod <- currentModality

  let
    delta = posInfoDelta posInfo

    flags s ai =
      let
        them = mconcat
          [ [ Goal.NotInScope | s /= C.InScope ]
          , [ Goal.Inaccessible r
            | let r = getRelevance mod `inverseComposeRelevance` getRelevance ai
            , r /= Relevant ]
          , [ Goal.Erased   | not (getQuantity ai `moreQuantity` getQuantity mod) ]
          , [ Goal.Instance | isInstance ai ]
          ]
      in case them of { [] -> Nothing; xs -> Just xs }

    mkVar :: ContextEntry -> Task (Maybe Local)
    mkVar Dom{ domInfo = ai, unDom = (name, t) } = do
      -- if shouldHide ai name then return Nothing else Just <$> do
      let n = nameConcrete name
      x <- abstractToConcrete_ name

      let s = C.isInScope x
      -- ty <- allocForeignCl t
      con <- abstractToConcreteCtx TopCtx =<< reify (unEl t)

      pure $ Just Local
        { localBinder      = printed (Binder (ReifiedName n x) con False)
        , localValue       = Nothing
        , localBindingSite = toUpdatedPosition delta (nameBindingSite name)
        , localFlags       = flags s ai
        , localHiding      = getHiding ai
        , localModality    = getModality ai
        }

    mkLet :: (Name, Open I.LetBinding) -> Task (Maybe Local)
    mkLet (name, lb) = do
      LetBinding _ tm !dom <- getOpen lb
      let n = nameConcrete name
      x  <- abstractToConcrete_ name
      let s = C.isInScope x
      ty <- liftTCM (abstractToConcreteCtx TopCtx =<< reifyUnblocked =<< normalForm AsIs (unDom dom))
            -- Remove let bindings from x and later, to avoid folding to x = x, or using bindings
            -- not introduced when x was defined.
      v <- abstractToConcreteCtx TopCtx =<< liftTCM (removeLetBindingsFrom name (reifyUnblocked =<< normalForm AsIs tm))

      pure $ Just Local
        { localBinder      = printed (Binder (ReifiedName n x) ty False)
        , localBindingSite = toUpdatedPosition delta (nameBindingSite name)
        , localValue       = Just (printed (Binder (ReifiedName n x) v True))
        , localFlags       = flags s (domInfo dom)
        , localHiding      = getHiding (domInfo dom)
        , localModality    = getModality (domInfo dom)
        }

  gty <- fmap printed . liftTCM . printedTerm AsIs . unEl =<< getMetaTypeInContext mi

  ctx <- getContext
  let locals = zipWith raise [1..] ctx
  locals <- catMaybes <$> for locals mkVar

  lets <- Map.toAscList <$> asksTC envLetBindings
  lets <- catMaybes <$> for lets mkLet

  boundary <- liftTCM $ B.getIPBoundary Simplified iid

  constr <- traverse printedTCM =<< liftTCM (B.getConstraintsMentioning Simplified $ fromMaybe __IMPOSSIBLE__ (ipMeta ip))

  pure GoalInfo
    { goalGoal     = Goal iid (fromMaybe (toLsp (ipRange ip)) (getCurrentInteractionRange posInfo ip)) (Just gty)
    , goalContext  = lets ++ locals
    , goalBoundary = case boundary of
        [] -> Nothing
        xs -> Just $! map printed xs
    , goalConstraints = constr
    }

goal :: Handlers WorkerM
goal = requestHandler (SMethod_CustomMethod (Proxy :: Proxy "agda/query")) \req res -> withRunInIO \run -> run do
  case fromJSON @SomeQuery (req ^. params) of
    Success q@(SomeQuery uri query) -> runAtURI uri do
      reportSLn "lsp.query" 10 $ show q
      out <- tryError (fillQuery query)
      case out of
        Left err  -> liftIO . run . res $ Left $ ResponseError (InL LSPErrorCodes_RequestFailed) "Request failed" Nothing
        Right out -> liftIO . run . res $ Right $ toJSON out
    Aeson.Error e -> liftIO . run . res $ Left $ ResponseError (InL LSPErrorCodes_RequestFailed) (Text.pack e) Nothing
    -- pure ()

namedCompletionItem :: Text.Text -> CompletionItem
namedCompletionItem !name = CompletionItem
  { _textEditText        = Nothing
  , _sortText            = Nothing
  , _preselect           = Nothing
  , _labelDetails        = Nothing
  , _insertTextFormat    = Nothing
  , _insertText          = Nothing
  , _filterText          = Nothing
  , _documentation       = Nothing
  , _deprecated          = Nothing
  , _commitCharacters    = Nothing
  , _insertTextMode      = Nothing
  , _textEdit            = Nothing
  , _additionalTextEdits = Nothing
  , _command             = Nothing
  , _detail              = Nothing
  , _kind                = Nothing
  , _label               = name
  , _data_               = Nothing
  , _tags                = Nothing
  }

localCompletionItem :: MonadTCM m => Int -> Dom' Term (Name, Type) -> m (Maybe CompletionItem)
localCompletionItem ix var@Dom{unDom = (name, ty)} = liftTCM $ runMaybeT do
  ty <- Text.pack . Ppr.render <$> lift (prettyATop =<< reify (unEl (raise (ix + 1) ty)))

  concrete <- lift (abstractToConcrete_ name)
  guard (C.InScope == C.isInScope concrete)

  name <- Text.pack . Ppr.renderStyle (Ppr.Style Ppr.OneLineMode 0 0) <$> lift (prettyTCM name)

  pure $! namedCompletionItem name
    & sortText     ?~ "00_" <> Text.pack (show ix)
    & labelDetails ?~ CompletionItemLabelDetails (Just (" : " <> ty)) Nothing
    & kind         ?~ CompletionItemKind_Variable

definedCompletionItem :: QName -> Task (Maybe CompletionItem)
definedCompletionItem qnm | isExtendedLambdaName qnm = pure Nothing
definedCompletionItem qnm | isAbsurdLambdaName qnm = pure Nothing
definedCompletionItem qnm = getConstInfo qnm >>= \def -> runMaybeT do
  name <- Text.pack . Ppr.render <$> lift (prettyTCM qnm)

  let
    defKind = case theDef def of
      Axiom{}                             -> Just CompletionItemKind_Constant
      I.Datatype{}                        -> Just CompletionItemKind_Enum
      I.Record{}                          -> Just CompletionItemKind_Struct
      I.Function{funProjection = Right _} -> Just CompletionItemKind_Field
      I.Function{}                        -> Just CompletionItemKind_Function
      I.Primitive{}                       -> Just CompletionItemKind_Function
      I.Constructor{}                     -> Just CompletionItemKind_EnumMember
      _                                   -> Nothing

  ty <- Text.pack . Ppr.render <$> lift (inTopContext (prettyATop =<< reify (defType def)))

  pure $! namedCompletionItem name
    & labelDetails ?~ CompletionItemLabelDetails (Just (" : " <> ty)) Nothing
    & kind         .~ defKind

completion :: Handlers WorkerM
completion = requestHandlerTCM SMethod_TextDocumentCompletion (view (params . textDocument . uri)) \req res -> do
  ip <- findInteractionPoint (req ^. params . position)
  case ip of
    Just (ii, ip, _) -> withInteractionId ii do
      ctx  <- getContext
      comp <- traverse (uncurry localCompletionItem) (zip [0..] ctx)

      want  <- traverse getMetaTypeInContext (ipMeta ip)
      comp' <- traverse definedCompletionItem . Set.toList =<< fmap (^. scopeInScope) getScope

      reportSLn "lsp.completion" 10 $ show req
      res (Right (InL (catMaybes comp ++ catMaybes comp')))
    Nothing -> res $ Right (InR (InR Lsp.Null))

onTypeFormatting :: Handlers WorkerM
onTypeFormatting = requestHandler SMethod_TextDocumentOnTypeFormatting \req res -> do
  text <- fmap virtualFileText <$> getVirtualFile (toNormalizedUri (req ^. params . textDocument . uri))
  case text of
    Just text -> do
      let
        linum = req ^. params . position . line
        l = Text.lines text !! fromIntegral linum

        edit (T.TokSymbol T.SymQuestionMark ival)
          | posPos (iEnd ival) == posPos (iStart ival) + 1
          = pure $ TextEdit {_newText="{! !}", _range=toLsp ival}
        edit _ = []

      (toks, _) <- runPMIO $ P.parsePosString P.tokensParser (Pn Strict.Nothing 0 (fromIntegral linum + 1) 1) $ Text.unpack l
      case toks of
        Right (toks, _) -> res $ Right $ InL $ toks >>= edit
        Left  _         -> res $ Right $ InR Lsp.Null

    Nothing -> pure ()

goToDefinition :: ClientCapabilities -> Handlers WorkerM
goToDefinition caps = requestHandlerTCM SMethod_TextDocumentDefinition (view (params . textDocument . uri)) \req res -> do
  reportSLn "lsp.definition" 10 $ show req

  info <- useTC stSyntaxInfo
  delta <- getDelta
  tc <- getTCState

  let currentAspect = findAspect delta (req ^. params . position) info
  res case currentAspect >>= definitionSite . snd of
    Just (DefinitionSite mod pos _ _)
      | Just path <- tc ^. stModuleToSource . key mod
      , Just info <- tc ^. stVisitedModules . key mod
      , Just (_, RangeMap.PairInt (_ :!: node)) <- Map.lookupLE pos . RangeMap.rangeMap . iHighlighting . miInterface $ info
      , aspectRange node /= noRange
      ->
        let
          srcUri = Lsp.filePathToUri (filePath path)
          -- TODO: upgrade position if in the current file.
          srcPos = toLsp (aspectRange node)
        in Right $
          if fromMaybe False ((caps ^. textDocument) >>= view declaration >>= view linkSupport)
          then
            let
              link = Lsp.LocationLink
                { _originSelectionRange = fst <$> currentAspect
                , _targetUri = srcUri
                , _targetRange = srcPos
                , _targetSelectionRange = srcPos
                }
            in InR . InL $ [DefinitionLink link]
          else InL . Definition . InL $ Lsp.Location srcUri srcPos
    _ -> notFound

  where
    notFound = Right . InR . InR $ Lsp.Null

-- | Find an aspect at the specified position.
findAspect :: PosDelta -> Lsp.Position -> RangeMap Aspects -> Maybe (Lsp.Range, Aspects)
findAspect delta pos =
  -- TODO: This is currently a linear search. Ideally we'd be able to do a
  -- binary search over the aspect map.
  getFirst . foldMap (First . contains) . RangeMap.toList

  where
    contains (range, aspect) = do
      r <- toUpdatedPosition delta range
      guard (positionInRange pos r)
      pure (r, aspect)

highlightReferences :: Handlers WorkerM
highlightReferences = requestHandlerTCM SMethod_TextDocumentDocumentHighlight (view (params . textDocument . uri)) \req res -> do
  reportSLn "lsp.documentHighlight" 10 $ show req

  info <- useTC stSyntaxInfo
  delta <- getDelta

  let currentAspect = findAspect delta (req ^. params . position) info
  res case currentAspect >>= definitionSite . snd of
    Nothing -> Right . InR $ Lsp.Null
    Just s@(DefinitionSite mod pos _ _) -> Right . InL
      . mapMaybe (fmap makeHighlight . toUpdatedPosition delta . fst)
      . filter (\(_, x) -> s `elem` definitionSite x)
      $ RangeMap.toList info

  where
    makeHighlight r = DocumentHighlight r (Just DocumentHighlightKind_Read)


getCodeActions :: Handlers WorkerM
getCodeActions = requestHandlerTCM SMethod_TextDocumentCodeAction (view (params . textDocument . uri)) \req res -> do
  let fileUri = req ^. params . textDocument . uri
  reportSLn "lsp.codeAction" 10 $ show req

  ii <- getActiveInteractionPoints
  virtualFile <- theVirtualFile

  let
    reqRange@(Lsp.Range reqStart reqEnd) = req ^. params . range

    resolveInteraction i@(_, ip, range) = do
      (_, _, contents) <- interactionContents i =<< virtualFile
      pure (range, not (Text.null contents))

    -- Determine if this interaction point overlaps with the provided range
    -- at all
    containing
      = mapMaybe resolveInteraction
      $ filter (\(_, _, Lsp.Range iS iE) -> iE >= reqStart && iS <= reqEnd) ii

  res . Right $ case containing of
    [] -> InL []
    [(range, nonEmpty)] -> InL $
      [ InR $ makeCodeAction "Run proof search on this goal" "agda.auto" (Command_Auto fileUri (range ^. start)) ] ++
      [ InR $ makeCodeAction "Fill goal with current contents" "agda.give" (Command_Give fileUri (range ^. start)) | nonEmpty ] ++
      [ InR $ makeCodeAction "Refine goal" "agda.refine" (Command_Refine fileUri (range ^. start)) | nonEmpty ] ++
      [ InR $ makeCodeAction "Introduce term" "agda.refine" (Command_Intro fileUri (range ^. start)) | not nonEmpty ] ++
      [ InR $ makeCodeAction "Case split" "agda.case" (Command_Case fileUri (range ^. start)) ]
    _ -> InL []

  where
      makeCodeAction title kind command =
        CodeAction
        { _title = title
        , _kind = Just (CodeActionKind_Custom kind)
        , _diagnostics = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _edit = Nothing
        , _command = Just (toCommand title command)
        , _data_ = Nothing
        }

-- | Handle a command inside the TCM monad.
commandHandlerTCM
  :: Uri
  -> (Either ResponseError (Value |? Null) -> WorkerM ())
  -> ((forall a. WorkerM a -> IO a)
    -> Task (Either ResponseError (Value |? Null)))
  -> WorkerM ()
commandHandlerTCM uri res cont = withRunInIO \run -> run $
  runAtURI uri (cont run >>= (liftIO . run . res))

-- | Handle a command at an interaction point.
commandHandlerInteractionPoint
  :: Uri
  -> Lsp.Position
  -> (Either ResponseError (Value |? Null) -> WorkerM ())
  -> (InteractionPointInfo -> (Agda.Range, Int, Text.Text)
    -> Task (Either ResponseError (Value |? Null)))
  -> WorkerM ()
commandHandlerInteractionPoint uri pos res cont = commandHandlerTCM uri res \run -> do
  pos <- findInteractionPoint pos
  virtualFile <- theVirtualFile

  case pos of
    Just i
      | Just contents <- interactionContents i =<< virtualFile
      -> cont i contents

    _ -> pure . Left $
      ResponseError (InL LSPErrorCodes_RequestFailed) "Cannot find interaction at this point" Nothing

removeHighlightingFor :: (Agda.HasRange a, MonadTCState m) => a -> m ()
removeHighlightingFor x = modifyTCLens' stSyntaxInfo \map -> snd (insideAndOutside (rangeToRange (getRange x)) map)

executeAgdaCommand :: Handlers WorkerM
executeAgdaCommand = requestHandler SMethod_WorkspaceExecuteCommand \req res -> do
  lspDebug $ show (req ^. params)
  case parseCommand (req ^. params) of
    Aeson.Error e -> res (Left (ResponseError (InL LSPErrorCodes_RequestFailed) (Text.pack e) Nothing))
    Aeson.Success (Command_Auto uri pos) -> commandHandlerInteractionPoint uri pos res \(ii, ip, range) (inner, _, contents) -> do
      -- TODO: What's the proper range here? This gets passed from the elisp side, so
      -- I think it's the *inside* of the interaction point, after edits have been applied.
      result <- Mimer.mimer AsIs ii inner (Text.unpack contents)
      case result of
        Mimer.MimerNoResult -> showWorkspaceMessage MessageType_Error "No solution found"
        MimerExpr str -> sendWorkspaceEdit "Proof Search" (Map.singleton uri [TextEdit range (Text.pack str)]) (const (pure ()))
        MimerList sols -> showWorkspaceMessage MessageType_Info . Text.pack $
            "Multiple solutions:" ++
            intercalate "\n" ["  " ++ show i ++ ". " ++ s | (i, s) <- sols]
        MimerClauses{} -> __IMPOSSIBLE__    -- Mimer can't do case splitting yet

      pure (Right (InR Lsp.Null))

    Aeson.Success (Command_Give uri pos) -> giveHandler "Give" B.give uri pos res
    Aeson.Success (Command_Refine uri pos) -> giveHandler "Refine" B.refine uri pos res
    Aeson.Success (Command_ElaborateAndGive uri pos) -> giveHandler "Elaborate and give" (B.elaborate_give AsIs) uri pos res
    Aeson.Success (Command_Intro uri pos) -> commandHandlerInteractionPoint uri pos res \ipi@(ii, _, _) (range, offset, _) -> do
      ss <- liftTCM $ B.introTactic False ii -- TODO: pattern matching lambda?
      case ss of
        [] -> showWorkspaceMessage MessageType_Error "No introduction forms found." >> pure (Right (InR Lsp.Null))
        [s] -> runGive "Introduce" B.refine ipi (range, offset, Text.pack s)
        _:_:_ -> showWorkspaceMessage MessageType_Error "Multiple introduction forms found." >> pure (Right (InR Lsp.Null))

    Aeson.Success (Command_Case uri pos) -> commandHandlerInteractionPoint uri pos res \(ii, ip, _) (range, _, contents) -> do
      (f, casectxt, cs) <- liftTCM $ makeCase ii range (Text.unpack contents)

      withInteractionId ii do
        tel <- liftTCM $ lookupSection (qnameModule f) -- don't shadow the names in this telescope
        unicode <- getsTC $ optUseUnicode . getPragmaOptions

        let
          ipc = case ipClause ip of
            IPClause{ipcClause = cls} -> cls

            -- The interaction point will be in an IPClause because
            -- otherwise the case split tactic fails
            _ -> __IMPOSSIBLE__

          clr = getRange ipc

          -- Indentation prefix that aligns the clauses.
          indent = case rStart clr of
            Just pos -> replicate (fromIntegral (posCol pos - 1)) ' '
            Nothing  -> __IMPOSSIBLE__

        pcs <- liftTCM $ inTopContext $ addContext tel $ mapM prettyA cs

        let
          -- This is the same ugly hack for printing as in
          -- InteractionTop, so use that same code.
          (c0:cs0) = map (ITop.extlam_dropName unicode casectxt . ITop.decorate) pcs

          -- Note that, since we're just telling the editor to replace
          -- the entire clause by some new text, we have to align the
          -- later clauses ourselves.
          pcs'     = c0:map (indent ++) cs0

        -- We also have to replace question marks with interaction points.
        (replacement, _) <- liftIO $ rewriteHoles (Text.stripEnd (Text.pack (unlines pcs')))

        delta <- getPositionInfo

        case toUpdatedPosition (posInfoDelta delta) clr of
          Just pos -> do
            let edit = TextEdit pos replacement

            sendWorkspaceEdit "Case split" (Map.singleton uri [edit]) \case
              Left e  -> pure ()
              Right{} ->
                -- We have to reload after any pending events have been
                -- processed; trying to do it in the event handler
                -- causes pain.
                runLater reloadURI

            pure (Right (InR Lsp.Null))

          Nothing -> pure (Left (ResponseError (InL LSPErrorCodes_RequestFailed) "The clause to which this interaction point belongs does not have a range. Try reloading?" Nothing))

    Aeson.Success (Command_Reload uri) -> runAtURI uri reloadURI

  where
    applyEditNow range replacement contents
      = runIdentity . applyChange mempty contents
      . TextDocumentContentChangeEvent . InL
      $ TextDocumentContentChangePartial range Nothing replacement

    snapshotTCM :: TCM a -> Task (a, TCState)
    snapshotTCM task = do
      state <- liftTCM getTCState
      liftIO $ runTCM initEnv state task

    -- Apply some action that modifies the file, rewriting interaction points and
    -- highlighting information to match the new range.
    --
    -- The inner action should return an edit to apply, and an offset that is used to
    -- adjust the position of any changes that between the range of the entered
    -- expression, and its effective position in the file (for instance, due to the leading {!).
    rewritePositions :: PositionInfo -> Rope.Rope -> TCM (TextEdit, Int) -> TCM (TextEdit, Rope.Rope)
    rewritePositions posInfo fileContents makeEdit = do
      -- Capture the old highlighting and interaction points
      oldSyntaxInfo <- useTC stSyntaxInfo
      oldInteractionPoints <- useTC stInteractionPoints

      -- Clear the highlighting
      setTCLens stSyntaxInfo mempty
      -- Apply our edit, this will fill any new highlighting information
      (TextEdit range replacement, offset) <- makeEdit

      (replacement', remapPos) <- liftIO $ rewriteHoles replacement

      let
        newContents = applyEditNow range replacement' fileContents
        newDelta = posDeltaWithChange (posInfoDelta posInfo) range replacement'
        newSyntaxInfo = updateRangeMap newDelta newContents oldSyntaxInfo

        newRange = toAgdaRange newContents Null.empty range
        remapPos' x = remapPos newRange (x - offset)

        updatePoints _ [] = []
        updatePoints rs ((ii, ip):xs)
          -- If we're a new interaction point, use the range from the source code.
          | not (BiMap.source ii oldInteractionPoints)
          = let (r, rs') = uncons rs in (ii, ip { ipRange = r }):updatePoints rs' xs

          -- If we're an existing point, update the range according to the delta.
          | Just pos <- toUpdatedPosition newDelta (ipRange ip)
          = (ii, ip { ipRange = toAgdaRange newContents (rangeFile (ipRange ip)) pos }):updatePoints rs xs

          -- Otherwise ignore.
          | otherwise = (ii, ip):updatePoints rs xs

      replacementTokens <- generateTokenInfoFromString newRange (Text.unpack replacement')

      -- Offset any new highlighting information, and then add back the (now remapped)
      -- information.
      modifyTCLens stSyntaxInfo ((replacementTokens <> ) . (newSyntaxInfo <>) . modifyRangeMapPositions remapPos')

      -- Reparse the input string to the find the interaction points, and use that to correct
      -- the position of the interaction points. Refine will produce entirely incorrect
      -- positions, so it's easier to start from scratch than try to offset.
      ipRanges <- liftIO $ mapMaybe (fmap getRange . findInteractionPointToken) . fromJust <$> getTokens (fromJust (rStart newRange)) replacement'
      modifyTCLens stInteractionPoints (BiMap.fromList . updatePoints ipRanges . BiMap.toList)

      pure (TextEdit range replacement', newContents)

    giveHandler
      :: Text.Text -> (UseForce -> InteractionId -> Maybe Agda.Range -> A.Expr -> TCM A.Expr)
      -> Uri -> Lsp.Position -> (Either ResponseError (Value |? Null) -> WorkerM ())
      -> WorkerM ()
    giveHandler name give uri pos res = commandHandlerInteractionPoint uri pos res (runGive name give)

    runGive
      :: Text.Text -> (UseForce -> InteractionId -> Maybe Agda.Range -> A.Expr -> TCM A.Expr)
      -> InteractionPointInfo -> (Agda.Range, Int, Text.Text)
      -> Task (Either ResponseError (Value |? Null))
    runGive name give (ii, ip, range) (inner, offset, contents) = do
      scope <- getInteractionScope ii

      uri <- theURI
      posInfo <- getPositionInfo
      fileContents <- view file_text . fromJust <$> theVirtualFile

      ((edit, fileContents'), state) <- snapshotTCM $ removeHighlightingFor (ipRange ip) >> rewritePositions posInfo fileContents do
        given <- B.parseExprIn ii inner (Text.unpack contents)
        res <- give WithoutForce ii Nothing given
        ce <- abstractToConcreteScope scope res

        -- Issue 7218. See also InteractionTop
        saturateOpaqueBlocks

        let
          (replacement, offset') =
            if given == res
            then case ce of
              -- FIXME: Handle the case where we already have parens.
              C.Paren{} -> ("(" <> contents <> ")", offset - 1)
              _ -> (contents, offset)
            else (Text.pack (prettyShow ce), offset)

        highlightExpr res

        pure (TextEdit range replacement, offset')

      sendWorkspaceEdit name (Map.singleton uri [edit]) (const (pure ()))

      let fileLines = Rope.toTextLines fileContents'
      setSnapshot . Just $ StateSnapshot
        { snapshotText = TextLines.toText fileLines
        , snapshotTc = state
        , snapshotPosition = createPosDelta fileLines
       }

      pure (Right (InR Lsp.Null))

    getTokens :: Agda.Position -> Text.Text -> IO (Maybe [T.Token])
    getTokens pos txt = either (const Nothing) (Just . fst) . fst <$> Pa.runPMIO (Pa.parsePosString Pa.tokensParser pos (Text.unpack txt))

    -- Rewrite any holes in the expression, replacing "?" with "{!!}" and
    -- returning a function that offsets positions to adjust for this.
    rewriteHoles :: Text.Text -> IO (Text.Text, Agda.Range -> Int -> Int)
    rewriteHoles txt = do
      toks <- getTokens (Agda.Pn Null.empty 1 1 1) txt
      pure case toks of
        Nothing -> (txt, \_ x -> x)
        Just toks ->
          let
            (txts, ips, map) = rewriteHoleWorker txt toks 0 0
            remapPos range pos =
              let
                -- Adjust the position to be relative to the start of the document.
                pos' = pos - 1 - maybe 0 posOffset (rStart range)
                -- Then find the offset at or before the current character.
                (before, now, _) = Map.splitLookup pos' map
                offset = fromMaybe 0 $ now <|> (snd <$> Map.lookupMax before)
              -- And use it to offset
              in pos + offset
          in (Text.concat txts, remapPos)

    rewriteHoleWorker
      :: Text.Text -- The input text.
      -> [T.Token] -- The remaining token stream
      -> Int       -- The position of the last token emitted
      -> Int       -- The cumulative offset
      -> ([Text.Text], [(Int, Int)], Map.Map Int Int)
    rewriteHoleWorker txt [] p _ = ([Text.drop p txt], [], mempty)
    rewriteHoleWorker txt (T.TokSymbol T.SymQuestionMark ival:ts) p o
      | start <- posOffset (iStart ival), end <- posOffset (iEnd ival)
      , start + 1 == end =
        let (txts, ips, map) = rewriteHoleWorker txt ts end (o + 4)
        in (substring p start txt:"{! !}":txts, ips, Map.insert end (o + 4) map)
    rewriteHoleWorker txt (t:ts) p o = rewriteHoleWorker txt ts p o

    substring start end = Text.take (end - start) . Text.drop start
    posOffset x = fromIntegral (posPos x) - 1

    uncons [] = Prelude.error "Expected non-empty list"
    uncons (x:xs) = (x, xs)

lspHandlers :: ClientCapabilities -> Handlers WorkerM
lspHandlers caps = mconcat
  [ onTextDocumentOpen
  , onTextDocumentSaved
  , onTextDocumentClosed
  , onInitialized
  , onTextDocumentChange
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration (const (pure ()))
  , provideSemanticTokens
  , goal
  , Agda.LSP.Main.completion
  , Agda.LSP.Main.onTypeFormatting
  , goToDefinition caps
  , highlightReferences
  , getCodeActions
  , executeAgdaCommand
  ]

tryError :: MonadError e m => m a -> m (Either e a)
tryError = (`catchError` pure . Left) . fmap Right
