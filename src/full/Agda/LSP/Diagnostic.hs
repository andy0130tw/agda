module Agda.LSP.Diagnostic where

import qualified Language.LSP.Protocol.Types as Lsp
import qualified Language.LSP.Protocol.Lens as Lsp

import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Traversable

import qualified Agda.Syntax.Concrete.Pretty ()
import qualified Agda.Syntax.Common.Pretty as Ppr
import Agda.Syntax.Concrete.Definitions (notSoNiceDeclarations)
import Agda.Syntax.Abstract.Name
import Agda.Syntax.Position (HasRange(getRange), Range' (NoRange))

import Agda.TypeChecking.Pretty.Warning (prettyWarning)
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Monad

import Agda.LSP.Monad.Base
import Agda.LSP.Position

import Agda.Utils.Maybe (fromMaybe, isNothing, caseMaybe)
import Agda.Utils.Lens
import Agda.Interaction.BasicOps (typeOfMetaMI)
import Agda.Interaction.Base (Rewrite(AsIs))
import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_)

class ToDiagnostic a where
  toDiagnostic :: a -> Task [Lsp.Diagnostic]

  default toDiagnostic :: forall f b. (Foldable f, a ~ f b, ToDiagnostic b) => a -> Task [Lsp.Diagnostic]
  toDiagnostic = foldMap toDiagnostic

instance ToDiagnostic a => ToDiagnostic [a] where

diagnostic :: HasRange x => Lsp.DiagnosticSeverity -> x -> Task Doc -> Task [Lsp.Diagnostic]
diagnostic sev rng doc | NoRange <- getRange rng = pure []
diagnostic sev rng doc = do
  msg <- Text.pack . Ppr.render <$> doc
  delta <- posInfoDelta <$> getPositionInfo
  let
    upd  = toUpdatedPosition delta (getRange rng)
    rng' = fromMaybe (Lsp.Range (Lsp.Position 0 0) (Lsp.Position 0 0)) upd

  msg `seq` rng' `seq` pure (pure Lsp.Diagnostic
    { _range              = rng'
    , _severity           = Just sev
    , _code               = Nothing
    , _codeDescription    = Nothing
    , _source             = Just "agda"
    , _message            = msg
    , _tags               = Nothing
    , _relatedInformation = Nothing
    , _data_              = Nothing
    })

warning :: HasRange x => x -> Task Doc -> Task [Lsp.Diagnostic]
warning = diagnostic Lsp.DiagnosticSeverity_Warning

info :: HasRange x => x -> Task Doc -> Task [Lsp.Diagnostic]
info = diagnostic Lsp.DiagnosticSeverity_Information

error' :: HasRange x => x -> Task Doc -> Task [Lsp.Diagnostic]
error' = diagnostic Lsp.DiagnosticSeverity_Error

seeAlso :: HasRange x => x -> Task Doc -> Lsp.Diagnostic -> Task Lsp.Diagnostic
seeAlso rng doc diag = do
  msg <- Text.pack . Ppr.render <$> doc
  delta <- posInfoDelta <$> getPositionInfo
  uri <- theURI
  msg `seq` case toUpdatedPosition delta (getRange rng) of
    Just rng ->
      let
        loc = Lsp.Location uri rng
        info = Lsp.DiagnosticRelatedInformation
          { _location = loc
          , _message  = msg
          }
      in pure $! diag & Lsp.relatedInformation %~ (pure . (info:) . fromMaybe [])
    Nothing -> pure diag

instance ToDiagnostic TCWarning where
  toDiagnostic :: TCWarning -> Task [Lsp.Diagnostic]
  toDiagnostic warn = case tcWarning warn of
    TerminationIssue err -> error' warn (prettyTCM warn)
    NicifierIssue warn -> warning warn (pretty warn)

    InteractionMetaBoundaries mvs -> flip foldMap mvs \rng ->
      info rng "This interaction point has unsolved boundary constraints."

    w -> warning warn (prettyWarning w)
    -- _ -> error "TODO"

instance ToDiagnostic (Closure TypeError) where
  toDiagnostic c = case clValue c of
    ClashingDefinition nm old suggestion ->
      error' (envRange (clEnv c))
        (("Multiple definitions of " <> prettyTCM nm <> ".") $$ vcat do
          caseMaybe suggestion [] (\d ->
            [  "Perhaps you meant to write "
            $$ nest 2 ("'" <> pretty (notSoNiceDeclarations d) <> "'")
            $$ "In data definitions separate from data declaration, the ':' and type must be omitted."
            ]))
        >>= traverse (seeAlso (nameBindingSite (qnameName old)) "First declared here")

    e -> error' (envRange (clEnv c)) (prettyTCM c)

instance ToDiagnostic TCErr where
  toDiagnostic = \case
    TypeError loc _ Closure{ clValue = NonFatalErrors ws } ->
      foldMap toDiagnostic ws
    TypeError loc s e -> withTCState (const s) $ toDiagnostic e
    err -> error' err (prettyTCM err)

instance ToDiagnostic MetaVariable where
  toDiagnostic mv = withMetaInfo' mv do
    judg <- abstractToConcrete_ =<< liftTCM (typeOfMetaMI AsIs (jMetaId (mvJudgement mv)))
    warning mv ("Unsolved metavariable:" $$ nest 2 (pretty judg))
