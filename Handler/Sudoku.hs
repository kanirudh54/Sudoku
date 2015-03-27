module Handler.Sudoku where

import Import

getSudokuR :: Handler Html
getSudokuR = defaultLayout $ do
    setTitle "Sudoku"
    $(widgetFile "sudoku")

postSudokuR :: Handler Html
postSudokuR = error "Not yet implemented: postSudokuR"
