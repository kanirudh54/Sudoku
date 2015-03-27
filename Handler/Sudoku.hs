module Handler.Sudoku where

import Import

dat::Num t => [[[[t]]]] 
dat = [ [ [ [4,8,7],[6,1,9],[3,5,2] ] , [ [9,5,2],[4,3,8],[7,1,6] ] , [ [3,1,6],[2,7,5],[9,8,4] ] ] , [ [ [9,4,5],[1,7,8],[2,6,3] ] , [ [1,2,7],[3,6,4],[8,9,5] ] , [ [6,3,8],[5,9,2],[7,4,1] ] ] , [ [ [7,2,4],[5,3,1],[8,9,6] ] , [ [5,8,3],[6,4,9],[2,7,1] ] , [ [1,6,9],[8,2,7],[4,5,3] ] ] ]

getSudokuR ::Handler Html
getSudokuR = defaultLayout $ do
    setTitle "Sudoku"
    $(widgetFile "sudoku")

postSudokuR :: Handler Html
postSudokuR = error "Not yet implemented: postSudokuR"

-- Num t => [[[[t]]]] -> 