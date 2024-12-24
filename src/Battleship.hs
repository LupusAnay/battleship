{-# LANGUAGE BlockArguments #-}

module Battleship where

import Control.Monad.Trans.State (State, execState)
import Control.Monad.Trans.State qualified as State
import Data.Functor (unzip)
import Data.List (List, uncons)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)

data Board = Board
  { board :: Map.Map Point ShootResult,
    killLog :: [NonEmpty Point],
    shotLog :: [(Point, ShootResult)]
  }
  deriving (Show)

data ShootResult = Hit | Miss | HitSunk
  deriving (Show, Eq)

isHit :: ShootResult -> Bool
isHit = (`elem` [Hit, HitSunk])

type ShootFn = Int -> Int -> State Board ShootResult

emptyBoard :: Board
emptyBoard = Board mempty [] []

renderBoard :: Int -> Int -> Map.Map Point ShootResult -> Text
renderBoard n m board =
  let separator = "\n+" <> (T.intercalate "+" $ replicate m "-") <> "+\n|"
   in separator
        <> T.intercalate
          separator
          [ T.concat
              [ case Map.lookup (x, y) board of
                  Just HitSunk -> "X|"
                  Just Hit -> "X|"
                  Just Miss -> "•|"
                  Nothing -> " |"
              | y <- [0 .. m - 1]
              ]
          | x <- [0 .. n - 1]
          ]
        <> separator

type Point = (Int, Int)

showText :: (Show a) => a -> Text
showText = T.pack . show

sinkAllShips :: Int -> Int -> List Int -> ShootFn -> IO ()
sinkAllShips n m shipLengths shoot = do
  let board = play n m shipLengths shoot
  T.putStr $ "Board is cleared in " <> showText (length board.shotLog) <> " shots"
  T.putStr $ "The shots are: " <> T.intercalate "\n" [showText res <> " to " <> showText x <> ":" <> showText y | ((x, y), res) <- board.shotLog]
  T.putStr "Resulting board is:"
  T.putStr $ renderBoard n m board.board
  pure ()

play :: Int -> Int -> [Int] -> ShootFn -> Board
play n m shipLengths shoot = flip execState emptyBoard $ step fullBoard Nothing (0, 0)
  where
    fullBoard = Set.fromList $ concat [[(x, y) | y <- [0 .. m - 1]] | x <- [0 .. n - 1]]

    step :: Set.Set Point -> Maybe (NonEmpty Point) -> Point -> State Board ()
    step freeSpace mbCurrentShip point@(x, y) =
      do
        (mbShip, mbNextPoint, updatedFreeSpace) <-
          shoot x y >>= \res -> do
            State.modify (\b -> b {shotLog = (point, res) : b.shotLog})
            case res of
              Hit -> do
                let ship = maybe (NE.singleton point) (point <|) mbCurrentShip
                nextCoordinates <- nextCoordinateShip ship
                pure (Just ship, fst <$> uncons nextCoordinates, Set.delete point freeSpace)
              HitSunk -> do
                let ship = maybe (NE.singleton point) (point <|) mbCurrentShip
                State.modify (\b -> b {killLog = ship : b.killLog})
                let shipCells = Set.fromList $ concatMap (getAdjacentCells n m) $ NE.toList ship

                pure (Nothing, Set.lookupMin freeSpace, Set.difference freeSpace shipCells)
              Miss -> do
                nextCoordinate <- case mbCurrentShip of
                  Just ship -> (fmap fst . uncons) <$> nextCoordinateShip ship
                  Nothing -> pure $ Set.lookupMin freeSpace

                pure (mbCurrentShip, nextCoordinate, Set.delete point freeSpace)

        -- TODO: We should check possible coordinates if there's enough space for left ships,
        -- and if not - finish the game early
        isComplete <- isGameComplete
        case mbNextPoint of
          Just nextPoint | not isComplete -> step updatedFreeSpace mbShip nextPoint
          _ -> pure ()

    -- Just check if count of ships is the same
    -- this can be replaced with proper version which checks the
    -- length of the ships, but there's no point for now
    isGameComplete = do
      State.gets (\b -> length b.killLog == length shipLengths)

    isInBounds (x, y) = x >= 0 && x < n && y >= 0 && y < m

    nextCoordinateShip :: NE.NonEmpty Point -> State Board [Point]
    nextCoordinateShip ship = do
      let (xs', ys') = Data.Functor.unzip ship
          possiblePoints = case (NE.uncons $ NE.sort xs', NE.uncons $ NE.sort ys') of
            -- more than two points known, can calculate direction
            ((x1, Just xs), (y1, Just ys))
              | (NE.last xs - x1) == 0 -> [(x1, y1 - 1), (x1, NE.last ys + 1)] -- ship is vertical
              | (NE.last ys - y1) == 0 -> [(x1 - 1, y1), (NE.last xs + 1, y1)] -- ship is horizontal
              -- this is an impossible case with proper data, but if needed - can be handled differently
              | otherwise -> error "ship is neither vertical nor horizontal"
            -- only one point known, should choose direction at will
            ((x1, Nothing), (y1, Nothing)) -> [(x1 - 1, y1), (x1 + 1, y1), (x1, y1 - 1), (x1, y1 + 1)]
            -- this should be an impossible case
            _ -> error "coordinates list is asymmetrical"

      catMaybes <$> for possiblePoints \point -> do
        candidateState <- State.gets (Map.lookup point . board)
        if isInBounds point && isNothing candidateState
          then pure $ Just point
          else pure Nothing

getAdjacentCells :: Int -> Int -> Point -> [Point]
getAdjacentCells n m (x, y) =
  [ (x + xd, y + yd)
  | (xd, yd) <- [(-1, 1), (0, 1), (1, 1), (-1, 0), (0, 0), (1, 0), (-1, -1), (0, -1), (1, -1)],
    let x' = x + xd
        y' = y + xd,
    x' >= 0 && x' < n,
    y' >= 0 && y' < m
  ]
