{-# LANGUAGE DataKinds #-}
module Agda.LSP.Commands
  ( CommandArgs(..)
  , commandNames
  , toCommand
  , parseCommand
  ) where

import Control.Applicative
import Control.Arrow

import qualified Data.Text as Text
import Data.Aeson
import Data.Proxy
import Data.Maybe

import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens (command, arguments)
import Agda.Utils.Lens

import GHC.Generics
import GHC.TypeLits


data CommandArgs
  -- | Run Mimer on the interaction point at the given range.
  = Command_Auto             Uri Position
  | Command_Give             Uri Position
  | Command_Refine           Uri Position
  | Command_ElaborateAndGive Uri Position
  | Command_Intro            Uri Position
  | Command_Case             Uri Position
  -- | Reload the given file.
  | Command_Reload Uri
  deriving (Generic)

-- | Encode/decode a list of arguments to a list of JSON values.
class GCommandArgs f where
  toCommandArgs :: f a -> [Value]
  fromCommandArgs :: [Value] -> Result (f a, [Value])

instance (ToJSON a, FromJSON a) => GCommandArgs (K1 i a) where
  toCommandArgs (K1 x) = [toJSON x]
  fromCommandArgs [] = fail "Missing argument"
  fromCommandArgs (x:xs) = (,xs) . K1 <$> fromJSON x

instance GCommandArgs U1 where
  toCommandArgs U1 = []
  fromCommandArgs = pure . (U1, )

instance GCommandArgs a => GCommandArgs (M1 i c a) where
  toCommandArgs (M1 x) = toCommandArgs x
  fromCommandArgs = fmap (first M1) . fromCommandArgs

instance (GCommandArgs a, GCommandArgs b) => GCommandArgs (a :*: b) where
  toCommandArgs (a :*: b) = toCommandArgs a ++ toCommandArgs b
  fromCommandArgs xs = do
    (a, xs') <- fromCommandArgs xs
    (b, xs'') <- fromCommandArgs xs'
    pure (a :*: b, xs'')

-- | Encode/decode a sum type to a set of datatypes.
class GCommand f where
  gToCommand :: Text.Text -> f a -> Command

  -- | Parse this command. Returns `Nothing' if the command name is not known,
  -- and Just if the command is known (but possibly failed to parse).
  gParseCommand :: ExecuteCommandParams -> Maybe (Result (f a))

  gCommandNames :: Proxy f -> [Text.Text]

instance (GCommand a, GCommand b) => GCommand (a :+: b) where
  gToCommand t (L1 x) = gToCommand t x
  gToCommand t (R1 x) = gToCommand t x

  gParseCommand args = (fmap L1 <$> gParseCommand args) <|> (fmap R1 <$> gParseCommand args)

  gCommandNames Proxy = gCommandNames (Proxy :: Proxy a) <> gCommandNames (Proxy :: Proxy b)

commandName :: KnownSymbol name => Proxy name -> Text.Text
commandName = Text.toLower . fromJust . Text.stripPrefix "Command_" . Text.pack . symbolVal

instance (GCommandArgs a, KnownSymbol name) => GCommand (C1 ('MetaCons name p x) a) where
  gToCommand t (M1 x) = Command t (commandName (Proxy :: Proxy name)) (Just (toCommandArgs x))

  gParseCommand cmd =
    if cmd ^. command  == commandName (Proxy :: Proxy name)
    then Just case fromCommandArgs (fromMaybe [] (cmd ^. arguments)) of
      Success (res, _) -> pure (M1 res)
      Error e -> Error e
    else Nothing

  gCommandNames Proxy = [commandName (Proxy :: Proxy name)]

instance GCommand a => GCommand (D1 x a) where
  gToCommand t (M1 x) = gToCommand t x
  gParseCommand = fmap (fmap M1) . gParseCommand
  gCommandNames Proxy = gCommandNames (Proxy :: Proxy a)

-- | The names of all defined commands.
commandNames :: [Text.Text]
commandNames = gCommandNames (Proxy :: Proxy (Rep CommandArgs))

-- | Create a command from a title and our command arguments.
toCommand :: Text.Text -> CommandArgs -> Command
toCommand t = gToCommand t . from

-- | Parse a command from a set of command parameters.
parseCommand :: ExecuteCommandParams -> Result CommandArgs
parseCommand cmd = maybe (Error ("No such command: " <> show (toJSON cmd))) (fmap to) (gParseCommand cmd)
