module Agda.LSP.Monad.Config where

import Data.Default
import Data.Aeson

import GHC.Generics

import Agda.TypeChecking.Monad

-- | Configuration for the LSP server
data LspConfig = LspConfig
  { -- | Specifies the highlighting level that should be used over this
    -- connection.
    lspHighlightingLevel :: HighlightingLevel
    -- | Whether to reload open files when they are saved.
  , lspReloadOnSave      :: Bool
  }
  deriving (Show, Generic)

instance FromJSON HighlightingLevel
instance FromJSON LspConfig where
  parseJSON = withObject "LspConfig" \x -> (x .:? "lsp" .!= Object mempty) >>= (withObject "lsp") \lsp -> do
    reloadOnSave <- lsp .:? "reloadOnSave" .!= lspReloadOnSave def
    highlightingLevel <- lsp .:? "highlightingLevel" .!= lspHighlightingLevel def
    pure $ LspConfig
      { lspReloadOnSave = reloadOnSave
      , lspHighlightingLevel = highlightingLevel }

instance Default LspConfig where
  def = LspConfig
    { lspHighlightingLevel = NonInteractive
    , lspReloadOnSave      = True
    }
