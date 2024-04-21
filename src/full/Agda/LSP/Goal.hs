{-# LANGUAGE DataKinds, DisambiguateRecordFields #-}
module Agda.LSP.Goal where

import qualified Data.Text as Text
import Data.Foldable

import qualified Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Types

import GHC.Generics (Generic)

import qualified Agda.Syntax.TopLevelModuleName as C
import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Common as Common
import Agda.Syntax.Concrete.Pretty ()
import Agda.Syntax.Common.Pretty
import Agda.Syntax.Internal
import Agda.Syntax.Common

import Agda.TypeChecking.Monad

import Agda.Interaction.BasicOps ()
import Agda.Interaction.Base (Rewrite)

import Agda.TypeChecking.Pretty ()
import Agda.Interaction.JSON

import Agda.LSP.Monad.Base
import Agda.LSP.Output
import Agda.Interaction.Output (OutputConstraint, OutputForm)

data Query a where
  Query_GoalAt     :: Lsp.Position  -> Query (Maybe InteractionId)
  Query_ModuleName :: Query (Maybe (Printed C.TopLevelModuleName))
  Query_GoalInfo   :: InteractionId -> Query GoalInfo
  Query_AllGoals   :: Bool -> Query [Goal]

deriving instance Show (Query a)
deriving newtype instance FromJSON InteractionId
deriving newtype instance ToJSON InteractionId

data SomeQuery where
  SomeQuery :: forall a. (FromJSON a, ToJSON a) => Lsp.Uri -> Query a -> SomeQuery

deriving instance Show SomeQuery

data Goal = Goal
  { goalId    :: InteractionId
  , goalRange :: Range
  , goalType  :: Maybe (Printed C.Expr)
  }
  deriving (Show, Generic)

instance ToJSON Goal
instance FromJSON Goal

-- data Meta = Meta
--   { metaId    :: MetaId
--   , metaRange :: Range
--   , metaType  :: Maybe (Printed C.Expr)
--   }
--   deriving (Show, Generic)

-- instance ToJSON Meta
-- instance FromJSON Meta

data ReifiedName = ReifiedName
  { reifiedNameActual :: C.Name
  , reifiedName       :: C.Name
  }
  deriving (Show, Generic)

instance Pretty ReifiedName where
  pretty (ReifiedName n x)
    | n == x                     = pretty x
    | C.isInScope n == C.InScope = pretty n
    | otherwise                  = pretty x

data Binder = Binder
  { binderName    :: ReifiedName
  , binderType    :: C.Expr
  , binderIsValue :: Bool
  }
  deriving (Show, Generic)

instance Pretty Binder where
  pretty (Binder name expr islet) = sep [ pretty name <+> sym, nest 2 (wrap (pretty expr)) ] where
    sym  | islet = equals    | otherwise = colon
    wrap | islet = hlSubtree | otherwise = id

data LocalFlag = NotInScope | Inaccessible Relevance | Erased | Instance
  deriving (Show, Generic)
instance ToJSON LocalFlag
instance FromJSON LocalFlag

data Local = Local
  { localBinder      :: Printed Binder
  , localBindingSite :: Maybe Lsp.Range
  , localValue       :: Maybe (Printed Binder)
  , localFlags       :: Maybe [LocalFlag]
  , localHiding      :: Hiding
  , localModality    :: Modality
  }
  deriving (Show, Generic)

instance ToJSON Relevance where
  toJSON = toJSON . show

instance FromJSON Relevance where
  parseJSON = withText "Relevance" \x -> maybe (fail "Not a valid relevance") pure $ find ((x ==) . Text.pack . show) [minBound..maxBound]

instance ToJSON Quantity where
  toJSON = \case
    Quantity0{} -> "0"
    Quantity1{} -> "1"
    Quantityω{} -> "ω"

instance FromJSON Quantity where
  parseJSON (String "0") = pure $ Quantity0 Q0Inferred
  parseJSON (String "1") = pure $ Quantity1 Q1Inferred
  parseJSON (String "ω") = pure $ Quantityω QωInferred
  parseJSON _ = fail "Invalid Quantity"

instance FromJSON Cohesion where
  parseJSON = withText "Cohesion" \x -> maybe (fail "Not a valid cohesion") pure $ find ((x ==) . Text.pack . show) [minBound..maxBound]

instance ToJSON Cohesion where
  toJSON = toJSON . show

instance ToJSON Modality
instance FromJSON Modality

instance ToJSON Hiding where
  toJSON = \case
    Hidden      -> "Hidden"
    Common.Instance{}  -> "Instance"
    NotHidden{} -> "NotHidden"


instance FromJSON Hiding where
  parseJSON (String "Hidden")    = pure Hidden
  parseJSON (String "Instance")  = pure $ Common.Instance NoOverlap
  parseJSON (String "NotHidden") = pure NotHidden
  parseJSON _ = fail "Invalid Hiding value"

instance ToJSON Local
instance FromJSON Local

data GoalInfo = GoalInfo
  { goalGoal        :: Goal
  , goalContext     :: [Local]
  , goalBoundary    :: Maybe [Printed (IPFace' C.Expr)]
  , goalConstraints :: [Printed (OutputForm C.Expr C.Expr)]
  }
  deriving (Show, Generic)

instance ToJSON GoalInfo
instance FromJSON GoalInfo

instance FromJSON SomeQuery where
  parseJSON = withObject "Query" \obj -> do
    uri <- obj .: "uri"
    obj .: "kind" >>= withText "kind" \case
      "GoalAt"     -> SomeQuery uri <$> Query_GoalAt <$> obj .: "position"
      "GoalInfo"   -> SomeQuery uri <$> Query_GoalInfo <$> obj .: "goal"
      "AllGoals"   -> SomeQuery uri <$> Query_AllGoals <$> obj .: "types"
      "ModuleName" -> pure $ SomeQuery uri Query_ModuleName
      _ -> fail "Unknown query kind"

instance ToJSON SomeQuery where
  toJSON (SomeQuery uri query) =
    let
      kind :: String
      (kind, rest) = case query of
        Query_GoalAt pos   -> ("GoalAt", ["position" .= toJSON pos])
        Query_GoalInfo iid -> ("GoalInfo", ["goal" .= toJSON iid])
        Query_AllGoals t   -> ("AllGoals", ["types" .= toJSON t])
        Query_ModuleName   -> ("ModuleName", [])
    in
    object ("uri" .= toJSON uri : "kind" .= toJSON kind : rest)

-- data GoalResponse a
--   = AllGoals
--     { goals :: [Goal a] }
--   | OneGoal
--     { goalGoal    :: Goal a
--     , goalContext :: [Local a]
--     , goalFaces   :: [TC.IPFace' a]
--     }

-- data Judgement a = OfType a | IsSort

-- data Goal a = Goal
--   { goalMeta      :: InteractionId
--   , goalRange     :: Range
--   , goalJudgement :: Judgement a
--   }

-- data Local a = Local
--   { localName        :: C.Name
--   , localReifiedName :: C.Name
--   , localType        :: a
--   , localLet         :: Maybe a
--   , localInScope     :: C.NameInScope
--   }
