module Internal.Interaction.Lsp.Tests (tests) where

import qualified Internal.Interaction.Lsp.Interaction as Interaction
import Internal.Helpers

tests :: TestTree
tests = testGroup "Internal.Interaction.Lsp.Tests"
  [ Interaction.tests
  ]
