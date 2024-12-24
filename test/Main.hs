module Main (main) where

import Battleship (Board (..), Point, ShootResult (..), getAdjacentCells, play, renderBoard)
import Control.Monad (replicateM)
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.State qualified as State
import Data.Foldable (foldrM)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable (for)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, forAll, oneof, quickCheck, suchThat)

main :: IO ()
main = quickCheck prop_shouldSinkAllShips

-- Simple property which checks if count of generated ships are the same with a count of killed ships
prop_shouldSinkAllShips :: Property
prop_shouldSinkAllShips =
  forAll (genBoard 100 100) \(shipLengths, board, n, m) ->
    let res = play n m shipLengths (shootFn board)
     in counterexample (T.unpack $ renderBoard n m res.board) $ length res.killLog == (length shipLengths)

-- Simple game implementation which updates the state after result of the shot is known
shootFn :: (Map.Map (Int, Int) Int) -> Int -> Int -> State Board ShootResult
shootFn ships x y = do
  let point = (x, y)
  case Map.lookup point ships of
    Nothing -> do
      updateBoard point Miss
      pure Miss
    Just shipId -> do
      updateBoard point Hit

      shipState <- getShip shipId

      let shipIsDead = all (`elem` [Just Hit, Just HitSunk]) (Map.elems shipState)

      if shipIsDead
        then do
          updateBoard point HitSunk
          pure HitSunk
        else pure Hit
  where
    getShip shipId = do
      let shipCoordinates = Map.keys $ Map.filter (== shipId) ships

      shipState <-
        Map.fromList <$> for shipCoordinates \point -> do
          shootResult <- State.gets (Map.lookup point . board)
          pure (point, shootResult)

      pure shipState

    updateBoard point res = State.modify (\b@Board {..} -> b {board = Map.insert point res board})

genBoard :: Int -> Int -> Gen ([Int], Map.Map (Int, Int) Int, Int, Int)
genBoard n m = do
  maxNumberOfShips <- arbitrary `suchThat` (\ns -> ns > 0 && ns < (n * m))
  shipLengths <- replicateM maxNumberOfShips $ arbitrary `suchThat` (\sl -> sl > 0 && sl < m && sl < n)
  (ships, _) <-
    foldrM
      ( \l (ships, freeSpace) ->
          genShip n m l (ships, freeSpace) mempty
      )
      (mempty, fullBoard)
      shipLengths

  let board = Map.fromList $ concat $ zipWith (\shipId ship -> zip (Set.toList ship) (repeat shipId)) [0 ..] ships
  pure (map Set.size ships, board, n, m)
  where
    fullBoard = Set.fromList $ concat [[(x, y) | y <- [0 .. m - 1]] | x <- [0 .. n - 1]]

-- Recursively try different position until run out of space or ship is placed
genShip :: Int -> Int -> Int -> ([Set.Set Point], Set.Set Point) -> Set.Set Point -> Gen ([Set.Set Point], Set.Set Point)
genShip n m shipLength (ships, freeSpace) checkedLocations
  | Set.size freeSpace >= shipLength && Set.size checkedLocations <= Set.size freeSpace = do
      point <- oneof $ map pure $ Set.toList freeSpace

      let shipAxisDiff = [0 .. (shipLength - 1)]
          shipCandidates (x, y) =
            map
              NE.fromList
              [ [(x + xd, y) | xd <- shipAxisDiff],
                [(x - xd, y) | xd <- shipAxisDiff],
                [(x, y + yd) | yd <- shipAxisDiff],
                [(x, y - yd) | yd <- shipAxisDiff]
              ]

          getShipExclusionZone ship = Set.fromList $ concat [getAdjacentCells n m p | p <- Set.toList ship]

          possibleLocations p =
            filter
              ( \(Set.fromList . NE.toList -> ship) ->
                  ship `Set.isSubsetOf` freeSpace
                    && (getShipExclusionZone ship) `Set.isSubsetOf` freeSpace
              )
              (shipCandidates p)

      case NE.nonEmpty (possibleLocations point) of
        Just (NE.toList -> candidates) -> do
          ship <- oneof $ map (pure . Set.fromList . NE.toList) candidates
          let updateFreeSpace = Set.difference freeSpace ship
          pure (ship : ships, updateFreeSpace)
        Nothing -> genShip n m shipLength (ships, freeSpace) (Set.insert point checkedLocations)
  | otherwise = pure (ships, freeSpace)
