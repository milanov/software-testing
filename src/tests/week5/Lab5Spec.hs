module Lab5Spec(spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List

import Lab5
import Week5


-- a party real Sudoku function and its corresponding grid
sudokuM :: Sudoku
sudokuM (r,c) = if r == c then r else 0

gridM :: [[Value]]
gridM = [[1,0,0,0,0,0,0,0,0],
        [0,2,0,0,0,0,0,0,0],
        [0,0,3,0,0,0,0,0,0],
        [0,0,0,4,0,0,0,0,0],
        [0,0,0,0,5,0,0,0,0],
        [0,0,0,0,0,6,0,0,0],
        [0,0,0,0,0,0,7,0,0],
        [0,0,0,0,0,0,0,8,0],
        [0,0,0,0,0,0,0,0,9]]

-- a trivial Sudoku function and its corresponding grid
sudokuTrivial :: Sudoku
sudokuTrivial _ = 0

gridTrivial :: [[Value]]
gridTrivial = [[0,0,0,0,0,0,0,0,0] | r <- [1..9]]

-- an improper sudoku function (returns duplicates for rows/columns/subgrids)
sudokuImproper :: Sudoku
sudokuImproper _ = 1

-- checks whether two Sudoku function produce the same values
eqSudokus :: Sudoku -> Sudoku -> Bool
eqSudokus s1 s2 = and [ s1 (r,c) == s2 (r, c) | r <- [1..9], c <- [1..9]]

spec :: Spec
spec = do
  -- describe "isMinimal" $ do
  --   it "returns false for empty sudoku" $ do
  --     isMinimal emptyN `shouldBe` False

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
      sud2grid sudokuM `shouldBe` gridM

    it "works for the trivial sudoku" $ do
      sud2grid sudokuTrivial `shouldBe` gridTrivial

  describe "grid2sud" $ do
    it "returns the grid from the given sudoku" $ do
      eqSudokus (grid2sud gridM) sudokuM `shouldBe` True

    it "works for the trivial sudoku" $ do
      eqSudokus (grid2sud gridTrivial) sudokuTrivial `shouldBe` True

  describe "bl" $ do
    it "returns the right array of values" $ do
      let intervals = [[1..3], [4..6], [7..9]]
      and [bl x == block | block <- intervals, x <- block] `shouldBe` True

    it "returns an empty list if the given value is not between 1 and 9" $ do
      bl 90 `shouldBe` []

  describe "subGrid" $ do
    it "returns an empty array if the row is not between 1 and 9" $ do
      subGrid sudokuM (10,1) `shouldBe` []

    it "returns an empty array if the column is not between 1 and 9" $ do
      subGrid sudokuM (1,10) `shouldBe` []

    it "returns the right values when the row and the column are between 1 and 9" $ do
      subGrid sudokuM (1, 3) `shouldBe` [1,0,0,0,2,0,0,0,3]

  describe "freeInSeq" $ do
    it "returns an empty array if all the values are present in the sequence" $ do
      freeInSeq [1..9] `shouldBe` []

    it "returns all the values if the sequence is am empty list" $ do
      freeInSeq [] `shouldBe` [1..9]

    it "returns the remaining values missing from the sequence array" $ do
      freeInSeq [1,2,3,7] `shouldBe` [4,5,6,8,9]

  describe "freeInRow" $ do
    it "returns all the values if the row is not between 1 and 9" $ do
      freeInRow sudokuM 90 `shouldBe` [1..9]

    it "returns the right free values if the row is between 1 and 9" $ do
      freeInRow sudokuM 9 `shouldBe` [1..8]

  describe "freeInColumn" $ do
    it "returns all the values if the column is not between 1 and 9" $ do
      freeInColumn sudokuM 90 `shouldBe` [1..9]

    it "returns the right free values if the column is between 1 and 9" $ do
      freeInColumn sudokuM 9 `shouldBe` [1..8]

  describe "freeInSubgrid" $ do
    it "returns all the values if the row or column are not between 1 and 9" $ do
      freeInSubgrid sudokuM (90,1) `shouldBe` [1..9]

    it "returns the right free values if the row and column are between 1 and 9" $ do
      freeInSubgrid sudokuM (5,5) `shouldBe` ([1..3] ++ [7..9])

  describe "freeAtPos" $ do
    it "returns all the values if the row and column are not between 1 and 9" $ do
      freeAtPos sudokuM (20,10) `shouldBe` [1..9]

    it "returns the right free values if the row and column are between 1 and 9" $ do
      freeAtPos sudokuM (5,5) `shouldBe` ([1..3] ++ [7..9])

  describe "injective" $ do
    it "returns true when the given argument does not contain duplicates" $ do
      injective [1..9] `shouldBe` True

    it "returns false when the given argument contains duplicates" $ do
      injective [1,2,1] `shouldBe` False

  describe "rowInjective" $ do
    it "returns true when the filled elements from a given row are distinct" $ do
      rowInjective sudokuM 1 `shouldBe` True

    it "workds for the trivial case (no elements filled in the sudoku)" $ do
      rowInjective sudokuTrivial 1 `shouldBe` True

    it "returns false when the given row's filled elements contain duplicates" $ do
      rowInjective sudokuImproper 1 `shouldBe` False

  describe "colInjective" $ do
    it "returns true when the filled elements from a given column are distinct" $ do
      colInjective sudokuM 1 `shouldBe` True

    it "workds for the trivial case (no elements filled in the sudoku)" $ do
      colInjective sudokuTrivial 1 `shouldBe` True

    it "returns false when the given column's filled elements contain duplicates" $ do
      colInjective sudokuImproper 1 `shouldBe` False

  describe "subgridInjective" $ do
    it "returns true when the filled elements from a given subgrid are distinct" $ do
      subgridInjective sudokuM (1,1) `shouldBe` True

    it "workds for the trivial case (no elements filled in the sudoku)" $ do
      subgridInjective sudokuTrivial (1,1) `shouldBe` True

    it "returns false when the given subgrid's filled elements contain duplicates" $ do
      subgridInjective sudokuImproper (1,1) `shouldBe` False

  describe "consistent" $ do
    it "considers a valid sudoku to be consistent" $ do
      consistent sudokuM `shouldBe` True

    it "considers the trivial emtpy sudoku to be consistent" $ do
      consistent sudokuTrivial `shouldBe` True

    it "returns false when the sudoku has either rows/columns/subgrids which are not injective" $ do
      consistent sudokuImproper `shouldBe` False

  describe "update" $ do
    it "returns the new value after an update to the sudoku" $ do
      let newSudoku = update sudokuM ((1,1), 9)
      newSudoku (1,1) `shouldBe` 9

    it "does not change the values of the other fields" $ do
      let newSudoku = update sudokuM ((1,1), 1)
      eqSudokus newSudoku sudokuM `shouldBe` True

  describe "solved" $ do
    it "returns true when the given node doesn't have constraints" $ do
      solved (sudokuM, []) `shouldBe` True

    it "returns false when there are constraints in the given node" $ do
      solved (sudokuM, [(1,1,[1])]) `shouldBe` False

  describe "sameblock" $ do
    it "returns true when the two fields are in the same block" $ do
      sameblock (1,1) (3,3) `shouldBe` True

    it "returns false when the two fields are no in the same block" $ do
      sameblock (1,1) (5,5) `shouldBe` False

  describe "openPositions" $ do
    let allPos = [(r,c) | r <- [1..9], c <- [1..9]]
    let diagPos = [(r,r) | r <- [1..9]]

    it "returns all the open positions" $ do
      openPositions sudokuM `shouldBe` (allPos \\ diagPos)

    it "returns all the positions for the trivial sudoku" $ do
      openPositions sudokuTrivial `shouldBe` allPos

    it "returns an empty list for a sudoku which is all filled up" $ do
      openPositions sudokuImproper `shouldBe` []

  describe "constraints" $ do
    it "returns all the values for every field of the trivial sudoku" $ do
      constraints sudokuTrivial `shouldBe` [(r, c, [1..9]) | r <- [1..9], c <- [1..9]]

    it "returns an empty array for a filled up sudoku puzzle" $ do
      constraints sudokuImproper `shouldBe` []

    it "return the correct list of constraints for a sudoku puzzle" $ do
      let rev = reverse [1..9]
      let grid = [0:[2..9], 0:[2..9], 0:[2..9], rev, rev, rev, rev, rev, rev]
      constraints (grid2sud grid) `shouldBe` [(1,1,[1]), (2,1,[1]), (3,1,[1])]

  describe "grow" $ do
    it "returns an empty tree for an empty growing step" $ do
      grow (\_ -> []) 0 `shouldBe` T 0 []

    it "returns a properly 'grown' tree corresponding to the growing function" $ do
      let branch = T 1 [T 2 [], T 2 []]
      let expectedRes = T 0 [branch, branch]

      grow (\x -> if x < 2 then [x+1, x+1] else []) 0 `shouldBe` expectedRes

  describe "count" $ do
    it "returns one for a tree with no branches" $ do
      count (T 0 []) `shouldBe` 1

    it "counts all the nodes in the tree properly" $ do
      count (T 1 [T 2 [], T 2 []]) `shouldBe` 3
