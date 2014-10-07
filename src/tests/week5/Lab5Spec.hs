module Lab5Spec(spec) where

import Test.Hspec
import Test.QuickCheck

import Lab5
import Week5


sudoku :: Sudoku
sudoku (r,c) = 
            case (r,c) of
              (1,8) -> 1
              (2,6) -> 2
              (2,9) -> 3
              (3,4) -> 4
              (4,7) -> 5
              (5,1) -> 4
              (5,3) -> 1
              (5,4) -> 6
              (6,3) -> 7
              (6,4) -> 1
              (7,2) -> 5
              (7,7) -> 2
              (8,5) -> 8
              (8,8) -> 4
              (9,2) -> 3
              (9,4) -> 9
              (9,5) -> 1
              (_,_) -> 0

spec :: Spec
spec = do
  describe "isMinimal" $ do
    it "returns false for empty sudoku" $ do
      isMinimal emptyN `shouldBe` False

    --it "returns true for a minimal sudoku" $ do
    --  let sudoku = grid2sud  [[0,0,0,0,0,0,0,1,0],
    --                          [0,0,0,0,0,2,0,0,3],
    --                          [0,0,0,4,0,0,0,0,0],
    --                          [0,0,0,0,0,0,5,0,0],
    --                          [4,0,1,6,0,0,0,0,0],
    --                          [0,0,7,1,0,0,0,0,0],
    --                          [0,5,0,0,0,0,2,0,0],
    --                          [0,0,0,0,8,0,0,4,0],
    --                          [0,3,0,9,1,0,0,0,0]]
    --               in isMinimal (sudoku, constraints sudoku) `shouldBe` True


  describe "sud2grid" $ do
    it "returns the grid from the given sudoku" $ do
      sud2grid sudoku `shouldBe` [[0,0,0,0,0,0,0,1,0],
                                  [0,0,0,0,0,2,0,0,3],
                                  [0,0,0,4,0,0,0,0,0],
                                  [0,0,0,0,0,0,5,0,0],
                                  [4,0,1,6,0,0,0,0,0],
                                  [0,0,7,1,0,0,0,0,0],
                                  [0,5,0,0,0,0,2,0,0],
                                  [0,0,0,0,8,0,0,4,0],
                                  [0,3,0,9,1,0,0,0,0]]

    --it "should return a grid with the right dimensions" $ property $
    --  propSud2GridDimensions

  describe "grid2sud" $ do
    it "returns the grid from the given sudoku" $ do
      let grid = [[0,0,0,0,0,0,0,1,0],
                  [0,0,0,0,0,2,0,0,3],
                  [0,0,0,4,0,0,0,0,0],
                  [0,0,0,0,0,0,5,0,0],
                  [4,0,1,6,0,0,0,0,0],
                  [0,0,7,1,0,0,0,0,0],
                  [0,5,0,0,0,0,2,0,0],
                  [0,0,0,0,8,0,0,4,0],
                  [0,3,0,9,1,0,0,0,0]]
      let sud = grid2sud grid
      and [ sud (r,c) == ((grid !! (r - 1)) !! (c - 1)) | r <- [1..9], c <- [1..9]] `shouldBe` True

  describe "bl" $ do
    it "returns the right array of Int elements" $ do
      and ( [ bl x == [1,2,3] | x <- [1..3] ] ++
            [ bl x == [4,5,6] | x <- [4..6] ] ++
            [ bl x == [7,8,9] | x <- [7..9] ]) `shouldBe` True

  describe "subGrid" $ do
    it "returns an empty array if the row is not between 1 and 9" $ do
      subGrid sudoku (10,1) `shouldBe` []

    it "returns an empty array if the column is not between 1 and 9" $ do
      subGrid sudoku (1,10) `shouldBe` []

    it "returns the right values when the row and the column are between 1 and 9" $ do
      subGrid sudoku (1,4) `shouldBe` [0,0,0,0,0,2,4,0,0]

  describe "freeInSeq" $ do
    it "returns an empty array if all the values are present in the sequence" $ do
      freeInSeq [1..9] `shouldBe` []

    it "returns all the values if the sequence is am empty list" $ do
      freeInSeq [] `shouldBe` [1..9]

    it "returns the remaining values missing from the sequence array" $ do
      freeInSeq [1,2,3,7] `shouldBe` [4,5,6,8,9]

  describe "freeInRow" $ do
    it "returns all the values if the row is not between 1 and 9" $ do
      freeInRow sudoku 90 `shouldBe` [1..9]

    it "returns the right free values if the row is between 1 and 9" $ do
      freeInRow sudoku 9 `shouldBe` [2,4,5,6,7,8]

  describe "freeInColumn" $ do
    it "returns all the values if the column is not between 1 and 9" $ do
      freeInColumn sudoku 90 `shouldBe` [1..9]

    it "returns the right free values if the column is between 1 and 9" $ do
      freeInColumn sudoku 9 `shouldBe` [1,2,4,5,6,7,8,9]

  describe "freeInSubgrid" $ do
    it "returns all the values if the row or column are not between 1 and 9" $ do
      freeInSubgrid sudoku (90,1) `shouldBe` [1..9]

    it "returns the right free values if the row and column are between 1 and 9" $ do
      freeInSubgrid sudoku (5,5) `shouldBe` [2,3,4,5,7,8,9]

  describe "freeAtPos" $ do
    it "returns all the values if the row and column are not between 1 and 9" $ do
      freeAtPos sudoku (20,10) `shouldBe` [1..9]

    it "returns the right free values if the row and column are between 1 and 9" $ do
      freeAtPos sudoku (5,5) `shouldBe` [2,3,5,7,9]


-- Properties

--propSud2GridDimensions :: Sudoku -> Bool
--propSud2GridDimensions sud = (length (sud2grid sud) == 9) && (all (== 9) $ (map length (sud2grid sud)))




