-- | An entrypoint to the language server, with tweaks to work under a WASM/WASI
-- environment.
module Main (main) where

import Control.Monad.IO.Class
import Control.Concurrent

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text

import Data.ByteString.Builder.Extra (defaultChunkSize)

import System.Exit
import System.IO

import Language.LSP.Logging
import Language.LSP.Server

import Agda.LSP.Main (serverDefinition)

import qualified Colog.Core.Severity as L
import qualified Colog.Core.Action as L
import qualified Colog.Core.IO as L

main :: IO ()
main = do
  let
    clientIn = do
      -- Wait for stdin to be readable first. Reading from stdin (via fd_read)
      -- will block under WASI, stopping the rest of the server from working.
      -- This uses poll_oneoff under the hood, which isn't correctly implemented
      -- on all platforms.
      threadWaitRead (fromIntegral 0)
      chunk <- BS.hGetNonBlocking stdin defaultChunkSize
      if BS.null chunk then clientIn else pure chunk

    clientOut out = BSL.hPut stdout out >> hFlush stdout

    formatMsg (L.WithSeverity s m) = Text.pack $ "[" <> show s <> "] " <> show m

    logMsg :: (Show a, MonadIO m) => L.LogAction m (L.WithSeverity a)
    logMsg = L.LogAction (liftIO . BS.hPut stderr . Text.encodeUtf8 . (<> "\n") . formatMsg)

  exc <- runServerWith logMsg (L.cmap (fmap (Text.pack . show)) defaultClientLogger) clientIn clientOut . serverDefinition $ pure ()
  case exc of
    0 -> exitSuccess
    n -> exitWith (ExitFailure n)