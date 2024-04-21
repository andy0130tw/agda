module Agda.LSP.Translation where

import qualified Data.Text as Text
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Aeson (ToJSON)

import Control.Monad (guard)

import qualified Language.LSP.Protocol.Types as Lsp
import qualified Language.LSP.Protocol.Lens as Lsp

import Agda.Syntax.Common.Pretty
import Agda.Syntax.Position

import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Errors ()

import qualified Agda.Interaction.Highlighting.Range as Hl
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Utils.RangeMap (RangeMap)
import Agda.Utils.Impossible
import Agda.Utils.Lens
import Agda.Syntax.Common.Aspect as Asp
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.FileName (filePath)
import Agda.Syntax.Abstract.Name (Name(nameBindingSite), qnameName)
import Agda.LSP.Position

class ToLsp a where
  type LspType a
  toLsp :: a -> LspType a

instance ToLsp (Position' a) where
  type LspType (Position' a) = Lsp.Position
  toLsp (Pn _file _boff line col) = Lsp.Position (fromIntegral (line - 1)) (fromIntegral (col - 1))

instance ToLsp (Interval' a) where
  type LspType (Interval' a) = Lsp.Range
  toLsp (Interval start end) =
    let
      Lsp.Position le ce = toLsp end
    in Lsp.Range (toLsp start) (Lsp.Position le ce)

instance ToLsp (Range' a) where
  type LspType (Range' a) = Lsp.Range
  toLsp range = case rangeToIntervalWithFile range of 
    Nothing -> Lsp.Range (Lsp.Position 0 0) (Lsp.Position 0 0) -- TODO
    -- TODO: Agda has non-contiguous ranges but LSP just wants a start
    -- and end position.
    Just i -> toLsp (Interval (iStart i) (iEnd i))

instance ToLsp Aspect where
  type LspType Aspect = Lsp.SemanticTokenTypes
  toLsp = \case
    Comment -> Lsp.SemanticTokenTypes_Comment
    Keyword -> Lsp.SemanticTokenTypes_Keyword
    String  -> Lsp.SemanticTokenTypes_String
    Number  -> Lsp.SemanticTokenTypes_Number
    Hole    -> Lsp.SemanticTokenTypes_Custom "interactionPoint"
    Symbol  -> Lsp.SemanticTokenTypes_Custom "symbol"
    PrimitiveType -> Lsp.SemanticTokenTypes_Custom "primitiveType"
    Name Nothing _ -> Lsp.SemanticTokenTypes_Variable
    Name (Just kind) _ -> case kind of
      Asp.Bound                   -> Lsp.SemanticTokenTypes_Variable
      Asp.Generalizable           -> Lsp.SemanticTokenTypes_TypeParameter
      Asp.Constructor Inductive   -> Lsp.SemanticTokenTypes_EnumMember
      Asp.Constructor CoInductive -> Lsp.SemanticTokenTypes_Custom "coinductiveCons"
      Asp.Datatype                -> Lsp.SemanticTokenTypes_Enum
      Asp.Field                   -> Lsp.SemanticTokenTypes_Property
      Asp.Function                -> Lsp.SemanticTokenTypes_Function
      Asp.Module                  -> Lsp.SemanticTokenTypes_Namespace
      Asp.Postulate               -> Lsp.SemanticTokenTypes_Custom "postulate"
      Asp.Primitive               -> Lsp.SemanticTokenTypes_Custom "primitive"
      Asp.Record                  -> Lsp.SemanticTokenTypes_Struct
      Asp.Argument                -> Lsp.SemanticTokenTypes_Parameter
      Asp.Macro                   -> Lsp.SemanticTokenTypes_Macro
    Pragma     -> Lsp.SemanticTokenTypes_Custom "pragma"
    Background -> Lsp.SemanticTokenTypes_Custom "background"
    Markup     -> Lsp.SemanticTokenTypes_Custom "markup"

agdaTokenLegend :: Lsp.SemanticTokensLegend
agdaTokenLegend = Lsp.SemanticTokensLegend
  { _tokenTypes =
    [ "comment", "keyword", "string", "number", "interactionPoint",
    "symbol", "primitiveType", "variable", "typeParameter",
    "enumMember", "coinductiveCons", "enum", "property", "function",
    "namespace", "postulate", "primitive", "struct", "parameter",
    "macro", "pragma", "background", "markup" ]
  , _tokenModifiers = []
  }

aspectMapToTokens :: PosDelta -> RangeMap Aspects -> [Lsp.SemanticTokenAbsolute]
aspectMapToTokens delta = mapMaybe go . RangeMap.toList where
  go (range, aspect -> Just asp)
    | isInteresting asp
    , Just range <- toUpdatedPosition delta range
    , (range ^. Lsp.start . Lsp.line) == (range ^. Lsp.end . Lsp.line)
    = Just Lsp.SemanticTokenAbsolute
        { _tokenType      = toLsp asp
        , _line           = range ^. Lsp.start . Lsp.line
        , _startChar      = range ^. Lsp.start . Lsp.character
        , _length         = range ^. Lsp.end . Lsp.character - range ^. Lsp.start . Lsp.character
        , _tokenModifiers = []
        }
  go _ = Nothing

  isInteresting :: Aspect -> Bool
  isInteresting Comment       = False
  isInteresting Keyword       = False
  isInteresting String        = False
  isInteresting Number        = False
  isInteresting Hole          = True
  isInteresting Symbol        = False
  isInteresting PrimitiveType = True
  isInteresting Name{}        = True
  isInteresting Pragma        = False
  isInteresting Background    = False
  isInteresting Markup        = False
