module Agda.LSP.Position
  ( PosDelta
  , createPosDelta
  , posDeltaWithChange
  , posDeltaWithChangeEvents

  , Positionable(..)

  , modifyRangeMapPositions
  , updateRangeMap

  , toAgdaRange
  , toAgdaPos
  , modifyRangePositions
  ) where

import Control.Monad

import qualified Data.Text.Utf16.Rope.Mixed as Rope
import qualified Data.Text.Lines as TextLines
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text.Lines (TextLines)
import Data.Strict.Tuple (Pair(..))
import Data.Maybe

import Language.LSP.Protocol.Types as Lsp
import Language.LSP.Protocol.Lens

import qualified Agda.Interaction.Highlighting.Range as Range
import qualified Agda.Syntax.Position as Position
import qualified Agda.Utils.RangeMap as RangeMap
import Agda.Syntax.Common.Aspect

import Agda.Utils.Lens


-- | Maps positions in an original document to a newer version.
data PosDelta = PosDelta
  { delta :: !(Position -> Maybe Position)
  , lines :: TextLines
  }

-- | Create a new 'PosDelta' from the contents of a file.
createPosDelta :: TextLines -> PosDelta
createPosDelta = PosDelta pure

-- | Create a new delta that updates positions after applying some edit.
posDeltaWithChange :: PosDelta -> Range -> Text.Text -> PosDelta
posDeltaWithChange (PosDelta delta txt) range text = PosDelta (delta >=> toCurrent range text) txt

-- | Create a new delta that updates positions after applying an edit event.
posDeltaWithChangeEvent :: PosDelta -> TextDocumentContentChangeEvent -> PosDelta
posDeltaWithChangeEvent delta (TextDocumentContentChangeEvent (InL x)) = posDeltaWithChange delta (x ^. range) (x ^. text)
posDeltaWithChangeEvent (PosDelta _ txt) (TextDocumentContentChangeEvent (InR _)) = PosDelta (const Nothing) txt

-- | Create a new delta that updates positions after applying a batch of edit events.
posDeltaWithChangeEvents :: PosDelta -> [TextDocumentContentChangeEvent] -> PosDelta
posDeltaWithChangeEvents delta changes = foldl posDeltaWithChangeEvent delta changes

toCurrent :: Range -> Text.Text -> Position -> Maybe Position
toCurrent (Range start@(Position startLine startColumn) end@(Position endLine endColumn)) t (Position line column)
  | line < startLine || line == startLine && column < startColumn =
    -- Position is before the change and thereby unchanged.
    Just $ Position line column
  | line > endLine || line == endLine && column >= endColumn =
    -- Position is after the change so increase line and column number
    -- as necessary.
    Just $ newLine `seq` newColumn `seq` Position newLine newColumn
  | otherwise = Nothing
  -- Position is in the region that was changed.
  where
    lineDiff = linesNew - linesOld
    linesNew = Text.count "\n" t
    linesOld = fromIntegral endLine - fromIntegral startLine

    newEndColumn :: UInt
    newEndColumn
      | linesNew == 0 = fromIntegral $ fromIntegral startColumn + Text.length t
      | otherwise = fromIntegral $ Text.length $ Text.takeWhileEnd (/= '\n') t

    newColumn :: UInt
    newColumn
      | line == endLine = (column + newEndColumn) - endColumn
      | otherwise = column

    newLine :: UInt
    newLine = fromIntegral $ fromIntegral line + lineDiff

-- | Convert a character offset to a position in the new document.
getNewPosition :: PosDelta -> Int -> Maybe Lsp.Position
getNewPosition d@(PosDelta delta lines) offset =
  let
    (before, _) = TextLines.splitAt (fromIntegral offset - 1) lines
    pos = TextLines.lengthAsPosition before
  in delta $ Lsp.Position (fromIntegral (TextLines.posLine pos)) (fromIntegral (TextLines.posColumn pos))

getNewRange :: PosDelta -> Int -> Int -> Maybe Lsp.Range
getNewRange delta s e = do
  s' <- getNewPosition delta s
  -- Ranges are exclusive. So walk back one character, and then step it forward again.
  e' <- over character (+ 1) <$> getNewPosition delta (e - 1)
  guard (s' <= e')
  pure (Range s' e')

class Positionable a where
  -- | The result of updating a position, typically an LSP type.
  type PositionableResult a

  -- | Update this position according to the given position delta.
  toUpdatedPosition :: PosDelta -> a -> Maybe (PositionableResult a)

instance Positionable (Position.Position' a) where
  type PositionableResult (Position.Position' a) = Lsp.Position
  toUpdatedPosition delta pos = getNewPosition delta (fromIntegral (Position.posPos pos))

instance Positionable (Position.Interval' a) where
  type PositionableResult (Position.Interval' a) = Lsp.Range
  toUpdatedPosition delta (Position.Interval s e) = getNewRange delta (fromIntegral (Position.posPos s)) (fromIntegral (Position.posPos e))

instance Positionable (Position.Range' a) where
  type PositionableResult (Position.Range' a) = Lsp.Range
  toUpdatedPosition delta range = toUpdatedPosition delta =<< Position.rangeToInterval range

instance Positionable Range.Range where
  type PositionableResult Range.Range = Lsp.Range
  toUpdatedPosition delta (Range.Range s e) = getNewRange delta s e


modifyRangeMapPositions :: (Int -> Int) -> RangeMap.RangeMap a -> RangeMap.RangeMap a
modifyRangeMapPositions f = RangeMap.RangeMap . Map.fromAscList . map updatePos . Map.toList . RangeMap.rangeMap where
  updatePos (s, RangeMap.PairInt (e :!: a)) = (f s, RangeMap.PairInt (f e :!: a))

modifyRangePositions :: (Int -> Int) -> Rope.Rope -> Position.Range' a -> Position.Range' a
modifyRangePositions f _ Position.NoRange = Position.NoRange
modifyRangePositions f lines (Position.Range s is) = Position.Range s (fmap goI is) where
  goI (Position.Interval s e) = Position.Interval (goP s) (goP e)
  goP (Position.Pn s o l c) =
    let
      o' = fromIntegral . f $ fromIntegral o
      (before, _) = Rope.charSplitAt (fromIntegral o' - 1) lines
      pos = Rope.charLengthAsPosition before
    in Position.Pn s o' (fromIntegral (TextLines.posLine pos) + 1) (fromIntegral (TextLines.posColumn pos) + 1)

updateRangeMap :: PosDelta -> Rope.Rope -> RangeMap.RangeMap Aspects -> RangeMap.RangeMap Aspects
updateRangeMap delta newLines = RangeMap.RangeMap . Map.fromList . mapMaybe updatePos . RangeMap.toList where
  updatePos (r, aspect) = do
    Lsp.Range s e <- toUpdatedPosition delta r
    pure (getCharOffset newLines s, RangeMap.PairInt (getCharOffset newLines e :!: aspect))

-- | Convert an LSP range to an Agda range.
toAgdaRange :: Rope.Rope -> a -> Lsp.Range -> Position.Range' a
toAgdaRange lines file (Lsp.Range start end) = Position.Range file . pure $
  Position.Interval (toAgdaPos lines () start) (toAgdaPos lines () end)

-- | Convert an LSP range to an Agda position.
toAgdaPos :: Rope.Rope -> a -> Lsp.Position -> Position.Position' a
toAgdaPos lines file pos = Position.Pn
  file
  (fromIntegral (getCharOffset lines pos))
  (fromIntegral (pos ^. line) + 1)
  (fromIntegral (pos ^. character) + 1)

-- | Get the offset of a character from an LSP position.
getCharOffset :: Rope.Rope -> Lsp.Position -> Int
getCharOffset lines pos =
  let (before, _) = Rope.splitAtLine (fromIntegral (pos ^. line)) lines
  in fromIntegral (Rope.charLength before) + fromIntegral (pos ^. character) + 1
