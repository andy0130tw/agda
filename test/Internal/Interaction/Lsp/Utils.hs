{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Internal.Interaction.Lsp.Utils where

import System.Environment
import System.Directory
import System.FilePath
import System.IO.Temp

import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Control.Applicative

import qualified Data.Text as Text
import Data.Foldable
import Data.Default
import Data.Proxy
import Data.Aeson

import GHC.TypeLits (symbolVal)

import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens
import Language.LSP.Test as LspT


import qualified Agda.Syntax.Concrete as C
import Agda.Syntax.Literal

import Agda.LSP.Translation
import Agda.LSP.Goal
import Agda.LSP.Output

import Agda.Utils.CallStack
import Agda.Utils.Lens


import Utils
import Agda.Syntax.Position (noRange)

lspRoot :: FilePath
lspRoot = "test/lsp"

runAgdaSession :: ClientCapabilities -> FilePath -> Session a -> IO a
runAgdaSession caps path session = withSystemTempDirectory (takeBaseName path) \tempDir -> do
  -- Copy the files to a temporary directory, so we can edit them without issues.
  copyFiles path tempDir

  -- Then write a an agda-lib file, so we can find Common files in the existing test directory.
  rootDir <- getCurrentDirectory
  writeFile (tempDir </> "lsp.agda-lib") ("name: test-lsp\ninclude:\n  .\n  " ++ (rootDir </> "test"))

  -- Then run Agda.
  agdaBin <- getAgdaBin <$> getEnvironment
  runSession (agdaBin ++ " --lsp -vlsp:10") caps tempDir do
    -- Wait for agda/highlightingInit to be received, so we know we're fully initialised.
    _ <- satisfy \case
      FromServerMess (SMethod_CustomMethod p) _
        | "agda/highlightingInit" <- symbolVal p -> True
      _ -> False

    session

  where
    copyFiles :: FilePath -> FilePath -> IO ()
    copyFiles src dest = do
      isDir <- doesDirectoryExist src
      if isDir
      then do
        contents <- listDirectory src
        createDirectoryIfMissing False dest
        traverse_ (\x -> copyFiles (src </> x) (dest </> x)) contents
      else copyFile src dest

    config :: SessionConfig
    config = def { messageTimeout = 10 }

-- | Wait for an Agda file to be reloaded.
waitForReload :: Session [Diagnostic]
waitForReload = worker [] where
  worker :: [Diagnostic] -> Session [Diagnostic]
  worker diag = do
    msg <- anyMessage
    case msg of
      FromServerMess SMethod_TextDocumentPublishDiagnostics req -> worker (req ^. params . diagnostics)
      -- If we receive a agda/goals, then TCing has finished.
      FromServerMess (SMethod_CustomMethod ty) _ | symbolVal ty == "agda/goals" -> pure diag
      _ -> worker diag

-- | Wait for an Agda file to be reloaded, and assert that it reported no errors.
waitForSuccessfulReload :: Session ()
waitForSuccessfulReload = do
  diags <- waitForReload
  when (diags /= []) $ liftIO $ throw UnexpectedDiagnostics

-- | Wait for an Agda file to be reloaded, and assert that it reported no errors.
saveFile :: TextDocumentIdentifier -> Session ()
saveFile doc = sendNotification SMethod_TextDocumentDidSave DidSaveTextDocumentParams
  { _textDocument = doc
  , _text = Nothing
  }

-- | Change some text in a range.
changeText :: TextDocumentIdentifier -> Range -> String -> Session ()
changeText doc range text = changeDoc doc [TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial range Nothing (Text.pack text)))]

-- | Insert some text at a position.
insertText :: TextDocumentIdentifier -> Position -> String -> Session ()
insertText doc pos text = changeText doc (Range pos pos) text

decodeTokens :: [UInt] -> [SemanticTokenRelative]
decodeTokens [] = []
decodeTokens (dl:dc:len:ty:mod:xs) = SemanticTokenRelative
  { _deltaLine = dl
  , _deltaStartChar = dc
  , _length = len
    -- TODO: Replace with an array?
  , _tokenType = fromOpenEnumBaseType ((agdaTokenLegend ^. tokenTypes) !! fromIntegral ty)
  , _tokenModifiers = []
  } : decodeTokens xs
decodeTokens _ = Prelude.error "Malformed token list"

-- | Get the absolute semantic tokens.
getAbsSemanticTokens :: TextDocumentIdentifier -> Session [SemanticTokenAbsolute]
getAbsSemanticTokens doc = do
  toks <- getSemanticTokens doc
  pure case toks of
    InL t -> absolutizeTokens . decodeTokens . view data_ $ t
    InR _ -> []

-- | Query the agda server
goalQuery :: (HasCallStack, ToJSON a, FromJSON a) => TextDocumentIdentifier -> Query a -> Session a
goalQuery doc query = do
  let fullQuery = SomeQuery (doc ^. uri) query
  res <- request (SMethod_CustomMethod (Proxy :: Proxy "agda/query")) (toJSON fullQuery)
  case res ^. result of
    Left e -> Prelude.error (show e)
    Right x -> parse x

  where
    parse x = case fromJSON x of
      Success x -> pure x
      Error e -> Prelude.error e

skipManyTill :: Alternative m => m a -> m end -> m end
skipManyTill p end = go where go = end <|> (p *> go)
