module Handler.Sudoku where

import Import
import Control.Applicative ((<$>),(<*>))
import Data.Text
import Yesod.Form.Jquery
import Data.List as L
import Control.Monad as M
import Data.String (fromString)
import qualified System.Random as Rand
import Text.Read as R
import System.CPUTime
import Prelude as P


domai = [1,2,3,4,5,6,7,8,9]
puzzle1d = [0,0,0,4,0,0,0,3,0,0,0,4,0,0,7,1,0,2,8,0,0,0,0,0,0,4,0,1,9,0,8,0,0,4,0,6,0,0,0,9,0,2,0,0,0,2,0,6,0,0,1,0,9,5,0,6,0,0,0,0,0,0,1,4,0,9,1,0,0,3,0,0,0,5,0,0,0,9,0,0,0]
puzzle2d = [0,5,0,0,3,0,9,0,0,0,3,9,4,0,0,0,0,0,0,0,0,0,0,9,6,4,0,0,6,0,0,0,8,4,0,0,5,0,0,0,0,0,0,0,8,0,0,1,9,0,0,0,2,0,0,8,2,6,0,0,0,0,0,0,0,0,0,0,5,7,6,0,0,0,5,0,9,0,0,8,0]
puzzle3d = [0,0,0,0,0,0,0,8,9,7,0,0,0,0,2,3,0,0,0,1,0,0,0,3,2,0,0,8,0,0,0,3,6,7,0,2,0,0,0,9,0,5,0,0,0,9,0,1,2,8,0,0,0,3,0,0,9,3,0,0,0,5,0,0,0,5,6,0,0,0,0,4,2,8,0,0,0,0,0,0,0]
puzzle4d = [0,0,8,9,7,0,0,5,0,7,3,0,0,0,2,0,0,0,6,0,0,0,0,0,7,0,0,4,0,0,3,0,0,0,0,8,3,0,0,0,9,0,0,0,7,8,0,0,0,0,5,0,0,1,0,0,2,0,0,0,0,0,5,0,0,0,2,0,0,0,4,9,0,7,0,0,8,3,1,0,0]
puzzle5d = [8,5,9,1,0,0,0,0,2,6,0,0,0,0,0,0,0,0,0,0,1,0,2,8,0,3,0,0,0,0,4,9,0,1,0,0,0,0,0,0,6,0,0,0,0,0,0,7,0,8,1,0,0,0,0,3,0,7,1,0,8,0,0,0,0,0,0,0,0,0,0,6,9,0,0,0,0,6,7,5,4]
puzzle6e = [0,0,7,0,0,0,3,0,6,6,0,0,4,0,0,2,7,0,0,5,2,0,1,6,0,8,0,0,0,0,1,0,7,0,3,8,0,7,8,0,0,0,5,9,0,2,6,0,8,0,5,0,0,0,0,2,0,5,8,0,1,6,0,0,3,1,0,0,9,0,0,7,8,0,6,0,0,0,4,0,0]
puzzle7e = [0,1,0,0,4,0,0,3,0,3,0,2,0,0,0,1,8,0,0,0,6,0,7,0,9,0,2,0,8,0,0,9,5,0,0,0,0,0,0,3,0,8,0,0,0,0,0,0,4,6,0,0,5,0,7,0,1,0,8,0,5,0,0,0,3,5,0,0,0,8,0,6,0,2,0,0,3,0,0,1,0]
puzzle8e = [0,0,9,0,0,0,0,3,0,3,0,0,0,0,9,0,0,2,0,5,2,0,3,1,0,7,0,0,0,4,2,5,0,0,0,0,2,9,0,0,0,0,0,5,1,0,0,0,0,1,3,2,0,0,0,8,0,3,4,0,5,1,0,5,0,0,1,0,0,0,0,6,0,1,0,0,0,0,8,0,0]
puzzle9e = [0,0,1,9,0,6,0,0,3,0,4,3,2,0,1,9,7,0,0,9,6,0,4,0,0,0,0,8,0,9,0,0,0,7,0,0,4,7,2,5,0,8,6,9,1,0,0,5,0,0,0,2,0,4,0,0,0,0,2,0,3,4,0,0,2,4,6,0,9,8,5,0,9,0,0,3,0,4,1,0,0]
puzzle10e =[6,0,0,0,0,0,0,0,0,0,3,9,4,0,0,6,0,2,1,0,0,3,7,0,0,0,8,0,0,2,0,6,3,1,5,0,7,0,8,0,1,0,3,0,9,0,1,5,7,4,0,2,0,0,8,0,0,0,5,4,0,0,3,9,0,1,0,0,8,7,6,0,0,0,0,0,0,0,0,0,4]
puzzles = [puzzle1d,puzzle2d,puzzle3d,puzzle4d,puzzle5d,puzzle6e,puzzle7e,puzzle8e,puzzle9e,puzzle10e]


--The Labelling Function
labelling ::[Integer] -> [([Char],Integer)]
labelling xs = labelling' xs 0 (L.length xs)

labelling' (x:xs) z y = [helper x z] L.++ (labelling' xs (z+1) (y-1))
labelling' [] z 0 = []

--Gives the (label,value)
helper x y =  ( "r" L.++ (rownum y) L.++ "c" L.++ (colnum y) , x )

--Gives row number and column number
rownum y =  (show ((quot y 9)+1) )
colnum y =  (show ((rem y 9)+1) )


fun [] = []
fun xs =
	let (a,b) = (L.splitAt 9 xs)
	in a:(fun b)


--Rand Element from puzzles
randelem xs = do
    index <- Rand.randomRIO (0,(L.length xs)-1)
    return (xs!!index)



--Converts to the format which the CSP solver understands
question xs =do
    x <- randelem xs
    let y = labelling x
    return (help y,x)   


help xs = [ (label,dom) | (label,value) <- xs, let dom = if (value==0) then domai else [value] ]
---Can be used to generate the condition where the terms in xs will be unequal.
{-
generation of condition is done instead of modifying the verfy function, because it will lead to coding 
every thing from start. Our algorithm is designed to take conditions of format (var1,condition,var2) which
is binary. All other types of condtions which can be converted to binay format can be used here
-}
alldiff::[[Char]] -> [([Char],Char,[Char])]
alldiff xs = [(a,'/',b) | a <- xs , b <- xs, a/=b]

--CSP Solver--
condi = [("r1c1",'/',"r1c2"),("r1c1",'/',"r1c3"),("r1c1",'/',"r1c4"),("r1c1",'/',"r1c5"),("r1c1",'/',"r1c6"),("r1c1",'/',"r1c7"),("r1c1",'/',"r1c8"),("r1c1",'/',"r1c9"),("r1c2",'/',"r1c3"),("r1c2",'/',"r1c4"),("r1c2",'/',"r1c5"),("r1c2",'/',"r1c6"),("r1c2",'/',"r1c7"),("r1c2",'/',"r1c8"),("r1c2",'/',"r1c9"),("r1c3",'/',"r1c4"),("r1c3",'/',"r1c5"),("r1c3",'/',"r1c6"),("r1c3",'/',"r1c7"),("r1c3",'/',"r1c8"),("r1c3",'/',"r1c9"),("r1c4",'/',"r1c5"),("r1c4",'/',"r1c6"),("r1c4",'/',"r1c7"),("r1c4",'/',"r1c8"),("r1c4",'/',"r1c9"),("r1c5",'/',"r1c6"),("r1c5",'/',"r1c7"),("r1c5",'/',"r1c8"),("r1c5",'/',"r1c9"),("r1c6",'/',"r1c7"),("r1c6",'/',"r1c8"),("r1c6",'/',"r1c9"),("r1c7",'/',"r1c8"),("r1c7",'/',"r1c9"),("r1c8",'/',"r1c9"),("r2c1",'/',"r2c2"),("r2c1",'/',"r2c3"),("r2c1",'/',"r2c4"),("r2c1",'/',"r2c5"),("r2c1",'/',"r2c6"),("r2c1",'/',"r2c7"),("r2c1",'/',"r2c8"),("r2c1",'/',"r2c9"),("r2c2",'/',"r2c3"),("r2c2",'/',"r2c4"),("r2c2",'/',"r2c5"),("r2c2",'/',"r2c6"),("r2c2",'/',"r2c7"),("r2c2",'/',"r2c8"),("r2c2",'/',"r2c9"),("r2c3",'/',"r2c4"),("r2c3",'/',"r2c5"),("r2c3",'/',"r2c6"),("r2c3",'/',"r2c7"),("r2c3",'/',"r2c8"),("r2c3",'/',"r2c9"),("r2c4",'/',"r2c5"),("r2c4",'/',"r2c6"),("r2c4",'/',"r2c7"),("r2c4",'/',"r2c8"),("r2c4",'/',"r2c9"),("r2c5",'/',"r2c6"),("r2c5",'/',"r2c7"),("r2c5",'/',"r2c8"),("r2c5",'/',"r2c9"),("r2c6",'/',"r2c7"),("r2c6",'/',"r2c8"),("r2c6",'/',"r2c9"),("r2c7",'/',"r2c8"),("r2c7",'/',"r2c9"),("r2c8",'/',"r2c9"),("r3c1",'/',"r3c2"),("r3c1",'/',"r3c3"),("r3c1",'/',"r3c4"),("r3c1",'/',"r3c5"),("r3c1",'/',"r3c6"),("r3c1",'/',"r3c7"),("r3c1",'/',"r3c8"),("r3c1",'/',"r3c9"),("r3c2",'/',"r3c3"),("r3c2",'/',"r3c4"),("r3c2",'/',"r3c5"),("r3c2",'/',"r3c6"),("r3c2",'/',"r3c7"),("r3c2",'/',"r3c8"),("r3c2",'/',"r3c9"),("r3c3",'/',"r3c4"),("r3c3",'/',"r3c5"),("r3c3",'/',"r3c6"),("r3c3",'/',"r3c7"),("r3c3",'/',"r3c8"),("r3c3",'/',"r3c9"),("r3c4",'/',"r3c5"),("r3c4",'/',"r3c6"),("r3c4",'/',"r3c7"),("r3c4",'/',"r3c8"),("r3c4",'/',"r3c9"),("r3c5",'/',"r3c6"),("r3c5",'/',"r3c7"),("r3c5",'/',"r3c8"),("r3c5",'/',"r3c9"),("r3c6",'/',"r3c7"),("r3c6",'/',"r3c8"),("r3c6",'/',"r3c9"),("r3c7",'/',"r3c8"),("r3c7",'/',"r3c9"),("r3c8",'/',"r3c9"),("r4c1",'/',"r4c2"),("r4c1",'/',"r4c3"),("r4c1",'/',"r4c4"),("r4c1",'/',"r4c5"),("r4c1",'/',"r4c6"),("r4c1",'/',"r4c7"),("r4c1",'/',"r4c8"),("r4c1",'/',"r4c9"),("r4c2",'/',"r4c3"),("r4c2",'/',"r4c4"),("r4c2",'/',"r4c5"),("r4c2",'/',"r4c6"),("r4c2",'/',"r4c7"),("r4c2",'/',"r4c8"),("r4c2",'/',"r4c9"),("r4c3",'/',"r4c4"),("r4c3",'/',"r4c5"),("r4c3",'/',"r4c6"),("r4c3",'/',"r4c7"),("r4c3",'/',"r4c8"),("r4c3",'/',"r4c9"),("r4c4",'/',"r4c5"),("r4c4",'/',"r4c6"),("r4c4",'/',"r4c7"),("r4c4",'/',"r4c8"),("r4c4",'/',"r4c9"),("r4c5",'/',"r4c6"),("r4c5",'/',"r4c7"),("r4c5",'/',"r4c8"),("r4c5",'/',"r4c9"),("r4c6",'/',"r4c7"),("r4c6",'/',"r4c8"),("r4c6",'/',"r4c9"),("r4c7",'/',"r4c8"),("r4c7",'/',"r4c9"),("r4c8",'/',"r4c9"),("r5c1",'/',"r5c2"),("r5c1",'/',"r5c3"),("r5c1",'/',"r5c4"),("r5c1",'/',"r5c5"),("r5c1",'/',"r5c6"),("r5c1",'/',"r5c7"),("r5c1",'/',"r5c8"),("r5c1",'/',"r5c9"),("r5c2",'/',"r5c3"),("r5c2",'/',"r5c4"),("r5c2",'/',"r5c5"),("r5c2",'/',"r5c6"),("r5c2",'/',"r5c7"),("r5c2",'/',"r5c8"),("r5c2",'/',"r5c9"),("r5c3",'/',"r5c4"),("r5c3",'/',"r5c5"),("r5c3",'/',"r5c6"),("r5c3",'/',"r5c7"),("r5c3",'/',"r5c8"),("r5c3",'/',"r5c9"),("r5c4",'/',"r5c5"),("r5c4",'/',"r5c6"),("r5c4",'/',"r5c7"),("r5c4",'/',"r5c8"),("r5c4",'/',"r5c9"),("r5c5",'/',"r5c6"),("r5c5",'/',"r5c7"),("r5c5",'/',"r5c8"),("r5c5",'/',"r5c9"),("r5c6",'/',"r5c7"),("r5c6",'/',"r5c8"),("r5c6",'/',"r5c9"),("r5c7",'/',"r5c8"),("r5c7",'/',"r5c9"),("r5c8",'/',"r5c9"),("r6c1",'/',"r6c2"),("r6c1",'/',"r6c3"),("r6c1",'/',"r6c4"),("r6c1",'/',"r6c5"),("r6c1",'/',"r6c6"),("r6c1",'/',"r6c7"),("r6c1",'/',"r6c8"),("r6c1",'/',"r6c9"),("r6c2",'/',"r6c3"),("r6c2",'/',"r6c4"),("r6c2",'/',"r6c5"),("r6c2",'/',"r6c6"),("r6c2",'/',"r6c7"),("r6c2",'/',"r6c8"),("r6c2",'/',"r6c9"),("r6c3",'/',"r6c4"),("r6c3",'/',"r6c5"),("r6c3",'/',"r6c6"),("r6c3",'/',"r6c7"),("r6c3",'/',"r6c8"),("r6c3",'/',"r6c9"),("r6c4",'/',"r6c5"),("r6c4",'/',"r6c6"),("r6c4",'/',"r6c7"),("r6c4",'/',"r6c8"),("r6c4",'/',"r6c9"),("r6c5",'/',"r6c6"),("r6c5",'/',"r6c7"),("r6c5",'/',"r6c8"),("r6c5",'/',"r6c9"),("r6c6",'/',"r6c7"),("r6c6",'/',"r6c8"),("r6c6",'/',"r6c9"),("r6c7",'/',"r6c8"),("r6c7",'/',"r6c9"),("r6c8",'/',"r6c9"),("r7c1",'/',"r7c2"),("r7c1",'/',"r7c3"),("r7c1",'/',"r7c4"),("r7c1",'/',"r7c5"),("r7c1",'/',"r7c6"),("r7c1",'/',"r7c7"),("r7c1",'/',"r7c8"),("r7c1",'/',"r7c9"),("r7c2",'/',"r7c3"),("r7c2",'/',"r7c4"),("r7c2",'/',"r7c5"),("r7c2",'/',"r7c6"),("r7c2",'/',"r7c7"),("r7c2",'/',"r7c8"),("r7c2",'/',"r7c9"),("r7c3",'/',"r7c4"),("r7c3",'/',"r7c5"),("r7c3",'/',"r7c6"),("r7c3",'/',"r7c7"),("r7c3",'/',"r7c8"),("r7c3",'/',"r7c9"),("r7c4",'/',"r7c5"),("r7c4",'/',"r7c6"),("r7c4",'/',"r7c7"),("r7c4",'/',"r7c8"),("r7c4",'/',"r7c9"),("r7c5",'/',"r7c6"),("r7c5",'/',"r7c7"),("r7c5",'/',"r7c8"),("r7c5",'/',"r7c9"),("r7c6",'/',"r7c7"),("r7c6",'/',"r7c8"),("r7c6",'/',"r7c9"),("r7c7",'/',"r7c8"),("r7c7",'/',"r7c9"),("r7c8",'/',"r7c9"),("r8c1",'/',"r8c2"),("r8c1",'/',"r8c3"),("r8c1",'/',"r8c4"),("r8c1",'/',"r8c5"),("r8c1",'/',"r8c6"),("r8c1",'/',"r8c7"),("r8c1",'/',"r8c8"),("r8c1",'/',"r8c9"),("r8c2",'/',"r8c3"),("r8c2",'/',"r8c4"),("r8c2",'/',"r8c5"),("r8c2",'/',"r8c6"),("r8c2",'/',"r8c7"),("r8c2",'/',"r8c8"),("r8c2",'/',"r8c9"),("r8c3",'/',"r8c4"),("r8c3",'/',"r8c5"),("r8c3",'/',"r8c6"),("r8c3",'/',"r8c7"),("r8c3",'/',"r8c8"),("r8c3",'/',"r8c9"),("r8c4",'/',"r8c5"),("r8c4",'/',"r8c6"),("r8c4",'/',"r8c7"),("r8c4",'/',"r8c8"),("r8c4",'/',"r8c9"),("r8c5",'/',"r8c6"),("r8c5",'/',"r8c7"),("r8c5",'/',"r8c8"),("r8c5",'/',"r8c9"),("r8c6",'/',"r8c7"),("r8c6",'/',"r8c8"),("r8c6",'/',"r8c9"),("r8c7",'/',"r8c8"),("r8c7",'/',"r8c9"),("r8c8",'/',"r8c9"),("r9c1",'/',"r9c2"),("r9c1",'/',"r9c3"),("r9c1",'/',"r9c4"),("r9c1",'/',"r9c5"),("r9c1",'/',"r9c6"),("r9c1",'/',"r9c7"),("r9c1",'/',"r9c8"),("r9c1",'/',"r9c9"),("r9c2",'/',"r9c3"),("r9c2",'/',"r9c4"),("r9c2",'/',"r9c5"),("r9c2",'/',"r9c6"),("r9c2",'/',"r9c7"),("r9c2",'/',"r9c8"),("r9c2",'/',"r9c9"),("r9c3",'/',"r9c4"),("r9c3",'/',"r9c5"),("r9c3",'/',"r9c6"),("r9c3",'/',"r9c7"),("r9c3",'/',"r9c8"),("r9c3",'/',"r9c9"),("r9c4",'/',"r9c5"),("r9c4",'/',"r9c6"),("r9c4",'/',"r9c7"),("r9c4",'/',"r9c8"),("r9c4",'/',"r9c9"),("r9c5",'/',"r9c6"),("r9c5",'/',"r9c7"),("r9c5",'/',"r9c8"),("r9c5",'/',"r9c9"),("r9c6",'/',"r9c7"),("r9c6",'/',"r9c8"),("r9c6",'/',"r9c9"),("r9c7",'/',"r9c8"),("r9c7",'/',"r9c9"),("r9c8",'/',"r9c9"),("r1c1",'/',"r2c1"),("r1c1",'/',"r3c1"),("r1c1",'/',"r4c1"),("r1c1",'/',"r5c1"),("r1c1",'/',"r6c1"),("r1c1",'/',"r7c1"),("r1c1",'/',"r8c1"),("r1c1",'/',"r9c1"),("r2c1",'/',"r3c1"),("r2c1",'/',"r4c1"),("r2c1",'/',"r5c1"),("r2c1",'/',"r6c1"),("r2c1",'/',"r7c1"),("r2c1",'/',"r8c1"),("r2c1",'/',"r9c1"),("r3c1",'/',"r4c1"),("r3c1",'/',"r5c1"),("r3c1",'/',"r6c1"),("r3c1",'/',"r7c1"),("r3c1",'/',"r8c1"),("r3c1",'/',"r9c1"),("r4c1",'/',"r5c1"),("r4c1",'/',"r6c1"),("r4c1",'/',"r7c1"),("r4c1",'/',"r8c1"),("r4c1",'/',"r9c1"),("r5c1",'/',"r6c1"),("r5c1",'/',"r7c1"),("r5c1",'/',"r8c1"),("r5c1",'/',"r9c1"),("r6c1",'/',"r7c1"),("r6c1",'/',"r8c1"),("r6c1",'/',"r9c1"),("r7c1",'/',"r8c1"),("r7c1",'/',"r9c1"),("r8c1",'/',"r9c1"),("r1c2",'/',"r2c2"),("r1c2",'/',"r3c2"),("r1c2",'/',"r4c2"),("r1c2",'/',"r5c2"),("r1c2",'/',"r6c2"),("r1c2",'/',"r7c2"),("r1c2",'/',"r8c2"),("r1c2",'/',"r9c2"),("r2c2",'/',"r3c2"),("r2c2",'/',"r4c2"),("r2c2",'/',"r5c2"),("r2c2",'/',"r6c2"),("r2c2",'/',"r7c2"),("r2c2",'/',"r8c2"),("r2c2",'/',"r9c2"),("r3c2",'/',"r4c2"),("r3c2",'/',"r5c2"),("r3c2",'/',"r6c2"),("r3c2",'/',"r7c2"),("r3c2",'/',"r8c2"),("r3c2",'/',"r9c2"),("r4c2",'/',"r5c2"),("r4c2",'/',"r6c2"),("r4c2",'/',"r7c2"),("r4c2",'/',"r8c2"),("r4c2",'/',"r9c2"),("r5c2",'/',"r6c2"),("r5c2",'/',"r7c2"),("r5c2",'/',"r8c2"),("r5c2",'/',"r9c2"),("r6c2",'/',"r7c2"),("r6c2",'/',"r8c2"),("r6c2",'/',"r9c2"),("r7c2",'/',"r8c2"),("r7c2",'/',"r9c2"),("r8c2",'/',"r9c2"),("r1c3",'/',"r2c3"),("r1c3",'/',"r3c3"),("r1c3",'/',"r4c3"),("r1c3",'/',"r5c3"),("r1c3",'/',"r6c3"),("r1c3",'/',"r7c3"),("r1c3",'/',"r8c3"),("r1c3",'/',"r9c3"),("r2c3",'/',"r3c3"),("r2c3",'/',"r4c3"),("r2c3",'/',"r5c3"),("r2c3",'/',"r6c3"),("r2c3",'/',"r7c3"),("r2c3",'/',"r8c3"),("r2c3",'/',"r9c3"),("r3c3",'/',"r4c3"),("r3c3",'/',"r5c3"),("r3c3",'/',"r6c3"),("r3c3",'/',"r7c3"),("r3c3",'/',"r8c3"),("r3c3",'/',"r9c3"),("r4c3",'/',"r5c3"),("r4c3",'/',"r6c3"),("r4c3",'/',"r7c3"),("r4c3",'/',"r8c3"),("r4c3",'/',"r9c3"),("r5c3",'/',"r6c3"),("r5c3",'/',"r7c3"),("r5c3",'/',"r8c3"),("r5c3",'/',"r9c3"),("r6c3",'/',"r7c3"),("r6c3",'/',"r8c3"),("r6c3",'/',"r9c3"),("r7c3",'/',"r8c3"),("r7c3",'/',"r9c3"),("r8c3",'/',"r9c3"),("r1c4",'/',"r2c4"),("r1c4",'/',"r3c4"),("r1c4",'/',"r4c4"),("r1c4",'/',"r5c4"),("r1c4",'/',"r6c4"),("r1c4",'/',"r7c4"),("r1c4",'/',"r8c4"),("r1c4",'/',"r9c4"),("r2c4",'/',"r3c4"),("r2c4",'/',"r4c4"),("r2c4",'/',"r5c4"),("r2c4",'/',"r6c4"),("r2c4",'/',"r7c4"),("r2c4",'/',"r8c4"),("r2c4",'/',"r9c4"),("r3c4",'/',"r4c4"),("r3c4",'/',"r5c4"),("r3c4",'/',"r6c4"),("r3c4",'/',"r7c4"),("r3c4",'/',"r8c4"),("r3c4",'/',"r9c4"),("r4c4",'/',"r5c4"),("r4c4",'/',"r6c4"),("r4c4",'/',"r7c4"),("r4c4",'/',"r8c4"),("r4c4",'/',"r9c4"),("r5c4",'/',"r6c4"),("r5c4",'/',"r7c4"),("r5c4",'/',"r8c4"),("r5c4",'/',"r9c4"),("r6c4",'/',"r7c4"),("r6c4",'/',"r8c4"),("r6c4",'/',"r9c4"),("r7c4",'/',"r8c4"),("r7c4",'/',"r9c4"),("r8c4",'/',"r9c4"),("r1c5",'/',"r2c5"),("r1c5",'/',"r3c5"),("r1c5",'/',"r4c5"),("r1c5",'/',"r5c5"),("r1c5",'/',"r6c5"),("r1c5",'/',"r7c5"),("r1c5",'/',"r8c5"),("r1c5",'/',"r9c5"),("r2c5",'/',"r3c5"),("r2c5",'/',"r4c5"),("r2c5",'/',"r5c5"),("r2c5",'/',"r6c5"),("r2c5",'/',"r7c5"),("r2c5",'/',"r8c5"),("r2c5",'/',"r9c5"),("r3c5",'/',"r4c5"),("r3c5",'/',"r5c5"),("r3c5",'/',"r6c5"),("r3c5",'/',"r7c5"),("r3c5",'/',"r8c5"),("r3c5",'/',"r9c5"),("r4c5",'/',"r5c5"),("r4c5",'/',"r6c5"),("r4c5",'/',"r7c5"),("r4c5",'/',"r8c5"),("r4c5",'/',"r9c5"),("r5c5",'/',"r6c5"),("r5c5",'/',"r7c5"),("r5c5",'/',"r8c5"),("r5c5",'/',"r9c5"),("r6c5",'/',"r7c5"),("r6c5",'/',"r8c5"),("r6c5",'/',"r9c5"),("r7c5",'/',"r8c5"),("r7c5",'/',"r9c5"),("r8c5",'/',"r9c5"),("r1c6",'/',"r2c6"),("r1c6",'/',"r3c6"),("r1c6",'/',"r4c6"),("r1c6",'/',"r5c6"),("r1c6",'/',"r6c6"),("r1c6",'/',"r7c6"),("r1c6",'/',"r8c6"),("r1c6",'/',"r9c6"),("r2c6",'/',"r3c6"),("r2c6",'/',"r4c6"),("r2c6",'/',"r5c6"),("r2c6",'/',"r6c6"),("r2c6",'/',"r7c6"),("r2c6",'/',"r8c6"),("r2c6",'/',"r9c6"),("r3c6",'/',"r4c6"),("r3c6",'/',"r5c6"),("r3c6",'/',"r6c6"),("r3c6",'/',"r7c6"),("r3c6",'/',"r8c6"),("r3c6",'/',"r9c6"),("r4c6",'/',"r5c6"),("r4c6",'/',"r6c6"),("r4c6",'/',"r7c6"),("r4c6",'/',"r8c6"),("r4c6",'/',"r9c6"),("r5c6",'/',"r6c6"),("r5c6",'/',"r7c6"),("r5c6",'/',"r8c6"),("r5c6",'/',"r9c6"),("r6c6",'/',"r7c6"),("r6c6",'/',"r8c6"),("r6c6",'/',"r9c6"),("r7c6",'/',"r8c6"),("r7c6",'/',"r9c6"),("r8c6",'/',"r9c6"),("r1c7",'/',"r2c7"),("r1c7",'/',"r3c7"),("r1c7",'/',"r4c7"),("r1c7",'/',"r5c7"),("r1c7",'/',"r6c7"),("r1c7",'/',"r7c7"),("r1c7",'/',"r8c7"),("r1c7",'/',"r9c7"),("r2c7",'/',"r3c7"),("r2c7",'/',"r4c7"),("r2c7",'/',"r5c7"),("r2c7",'/',"r6c7"),("r2c7",'/',"r7c7"),("r2c7",'/',"r8c7"),("r2c7",'/',"r9c7"),("r3c7",'/',"r4c7"),("r3c7",'/',"r5c7"),("r3c7",'/',"r6c7"),("r3c7",'/',"r7c7"),("r3c7",'/',"r8c7"),("r3c7",'/',"r9c7"),("r4c7",'/',"r5c7"),("r4c7",'/',"r6c7"),("r4c7",'/',"r7c7"),("r4c7",'/',"r8c7"),("r4c7",'/',"r9c7"),("r5c7",'/',"r6c7"),("r5c7",'/',"r7c7"),("r5c7",'/',"r8c7"),("r5c7",'/',"r9c7"),("r6c7",'/',"r7c7"),("r6c7",'/',"r8c7"),("r6c7",'/',"r9c7"),("r7c7",'/',"r8c7"),("r7c7",'/',"r9c7"),("r8c7",'/',"r9c7"),("r1c8",'/',"r2c8"),("r1c8",'/',"r3c8"),("r1c8",'/',"r4c8"),("r1c8",'/',"r5c8"),("r1c8",'/',"r6c8"),("r1c8",'/',"r7c8"),("r1c8",'/',"r8c8"),("r1c8",'/',"r9c8"),("r2c8",'/',"r3c8"),("r2c8",'/',"r4c8"),("r2c8",'/',"r5c8"),("r2c8",'/',"r6c8"),("r2c8",'/',"r7c8"),("r2c8",'/',"r8c8"),("r2c8",'/',"r9c8"),("r3c8",'/',"r4c8"),("r3c8",'/',"r5c8"),("r3c8",'/',"r6c8"),("r3c8",'/',"r7c8"),("r3c8",'/',"r8c8"),("r3c8",'/',"r9c8"),("r4c8",'/',"r5c8"),("r4c8",'/',"r6c8"),("r4c8",'/',"r7c8"),("r4c8",'/',"r8c8"),("r4c8",'/',"r9c8"),("r5c8",'/',"r6c8"),("r5c8",'/',"r7c8"),("r5c8",'/',"r8c8"),("r5c8",'/',"r9c8"),("r6c8",'/',"r7c8"),("r6c8",'/',"r8c8"),("r6c8",'/',"r9c8"),("r7c8",'/',"r8c8"),("r7c8",'/',"r9c8"),("r8c8",'/',"r9c8"),("r1c9",'/',"r2c9"),("r1c9",'/',"r3c9"),("r1c9",'/',"r4c9"),("r1c9",'/',"r5c9"),("r1c9",'/',"r6c9"),("r1c9",'/',"r7c9"),("r1c9",'/',"r8c9"),("r1c9",'/',"r9c9"),("r2c9",'/',"r3c9"),("r2c9",'/',"r4c9"),("r2c9",'/',"r5c9"),("r2c9",'/',"r6c9"),("r2c9",'/',"r7c9"),("r2c9",'/',"r8c9"),("r2c9",'/',"r9c9"),("r3c9",'/',"r4c9"),("r3c9",'/',"r5c9"),("r3c9",'/',"r6c9"),("r3c9",'/',"r7c9"),("r3c9",'/',"r8c9"),("r3c9",'/',"r9c9"),("r4c9",'/',"r5c9"),("r4c9",'/',"r6c9"),("r4c9",'/',"r7c9"),("r4c9",'/',"r8c9"),("r4c9",'/',"r9c9"),("r5c9",'/',"r6c9"),("r5c9",'/',"r7c9"),("r5c9",'/',"r8c9"),("r5c9",'/',"r9c9"),("r6c9",'/',"r7c9"),("r6c9",'/',"r8c9"),("r6c9",'/',"r9c9"),("r7c9",'/',"r8c9"),("r7c9",'/',"r9c9"),("r8c9",'/',"r9c9"),("r1c1",'/',"r2c2"),("r1c1",'/',"r2c3"),("r1c1",'/',"r3c2"),("r1c1",'/',"r3c3"),("r1c2",'/',"r2c1"),("r1c2",'/',"r2c3"),("r1c2",'/',"r3c1"),("r1c2",'/',"r3c3"),("r1c3",'/',"r2c1"),("r1c3",'/',"r3c2"),("r1c3",'/',"r3c1"),("r1c3",'/',"r3c2"),("r2c1",'/',"r3c2"),("r2c1",'/',"r3c3"),("r2c2",'/',"r3c1"),("r1c2",'/',"r3c3"),("r2c3",'/',"r3c1"),("r2c3",'/',"r3c2"),("r1c4",'/',"r2c5"),("r1c4",'/',"r2c6"),("r1c4",'/',"r3c5"),("r1c4",'/',"r3c6"),("r1c5",'/',"r2c4"),("r1c5",'/',"r2c6"),("r1c5",'/',"r3c4"),("r1c5",'/',"r3c6"),("r1c6",'/',"r2c4"),("r1c6",'/',"r3c5"),("r1c6",'/',"r3c4"),("r1c6",'/',"r3c5"),("r2c4",'/',"r3c5"),("r2c4",'/',"r3c6"),("r2c5",'/',"r3c4"),("r1c5",'/',"r3c6"),("r2c6",'/',"r3c4"),("r2c6",'/',"r3c5"),("r1c7",'/',"r2c8"),("r1c7",'/',"r2c9"),("r1c7",'/',"r3c8"),("r1c7",'/',"r3c9"),("r1c8",'/',"r2c7"),("r1c8",'/',"r2c9"),("r1c8",'/',"r3c7"),("r1c8",'/',"r3c9"),("r1c9",'/',"r2c7"),("r1c9",'/',"r3c8"),("r1c9",'/',"r3c7"),("r1c9",'/',"r3c8"),("r2c7",'/',"r3c8"),("r2c7",'/',"r3c9"),("r2c8",'/',"r3c7"),("r1c8",'/',"r3c9"),("r2c9",'/',"r3c7"),("r2c9",'/',"r3c8"),("r4c1",'/',"r5c2"),("r4c1",'/',"r5c3"),("r4c1",'/',"r6c2"),("r4c1",'/',"r6c3"),("r4c2",'/',"r5c1"),("r4c2",'/',"r5c3"),("r4c2",'/',"r6c1"),("r4c2",'/',"r6c3"),("r4c3",'/',"r5c1"),("r4c3",'/',"r6c2"),("r4c3",'/',"r6c1"),("r4c3",'/',"r6c2"),("r5c1",'/',"r6c2"),("r5c1",'/',"r6c3"),("r5c2",'/',"r6c1"),("r4c2",'/',"r6c3"),("r5c3",'/',"r6c1"),("r5c3",'/',"r6c2"),("r4c4",'/',"r5c5"),("r4c4",'/',"r5c6"),("r4c4",'/',"r6c5"),("r4c4",'/',"r6c6"),("r4c5",'/',"r5c4"),("r4c5",'/',"r5c6"),("r4c5",'/',"r6c4"),("r4c5",'/',"r6c6"),("r4c6",'/',"r5c4"),("r4c6",'/',"r6c5"),("r4c6",'/',"r6c4"),("r4c6",'/',"r6c5"),("r5c4",'/',"r6c5"),("r5c4",'/',"r6c6"),("r5c5",'/',"r6c4"),("r4c5",'/',"r6c6"),("r5c6",'/',"r6c4"),("r5c6",'/',"r6c5"),("r4c7",'/',"r5c8"),("r4c7",'/',"r5c9"),("r4c7",'/',"r6c8"),("r4c7",'/',"r6c9"),("r4c8",'/',"r5c7"),("r4c8",'/',"r5c9"),("r4c8",'/',"r6c7"),("r4c8",'/',"r6c9"),("r4c9",'/',"r5c7"),("r4c9",'/',"r6c8"),("r4c9",'/',"r6c7"),("r4c9",'/',"r6c8"),("r5c7",'/',"r6c8"),("r5c7",'/',"r6c9"),("r5c8",'/',"r6c7"),("r4c8",'/',"r6c9"),("r5c9",'/',"r6c7"),("r5c9",'/',"r6c8"),("r7c1",'/',"r8c2"),("r7c1",'/',"r8c3"),("r7c1",'/',"r9c2"),("r7c1",'/',"r9c3"),("r7c2",'/',"r8c1"),("r7c2",'/',"r8c3"),("r7c2",'/',"r9c1"),("r7c2",'/',"r9c3"),("r7c3",'/',"r8c1"),("r7c3",'/',"r9c2"),("r7c3",'/',"r9c1"),("r7c3",'/',"r9c2"),("r8c1",'/',"r9c2"),("r8c1",'/',"r9c3"),("r8c2",'/',"r9c1"),("r7c2",'/',"r9c3"),("r8c3",'/',"r9c1"),("r8c3",'/',"r9c2"),("r7c4",'/',"r8c5"),("r7c4",'/',"r8c6"),("r7c4",'/',"r9c5"),("r7c4",'/',"r9c6"),("r7c5",'/',"r8c4"),("r7c5",'/',"r8c6"),("r7c5",'/',"r9c4"),("r7c5",'/',"r9c6"),("r7c6",'/',"r8c4"),("r7c6",'/',"r9c5"),("r7c6",'/',"r9c4"),("r7c6",'/',"r9c5"),("r8c4",'/',"r9c5"),("r8c4",'/',"r9c6"),("r8c5",'/',"r9c4"),("r7c5",'/',"r9c6"),("r8c6",'/',"r9c4"),("r8c6",'/',"r9c5"),("r7c7",'/',"r8c8"),("r7c7",'/',"r8c9"),("r7c7",'/',"r9c8"),("r7c7",'/',"r9c9"),("r7c8",'/',"r8c7"),("r7c8",'/',"r8c9"),("r7c8",'/',"r9c7"),("r7c8",'/',"r9c9"),("r7c9",'/',"r8c7"),("r7c9",'/',"r9c8"),("r7c9",'/',"r9c7"),("r7c9",'/',"r9c8"),("r8c7",'/',"r9c8"),("r8c7",'/',"r9c9"),("r8c8",'/',"r9c7"),("r7c8",'/',"r9c9"),("r8c9",'/',"r9c7"),("r8c9",'/',"r9c8")]

--checks whether the given label and values satisfy the condition
--check::(Int,Int)->(Char,Char) -> [(Char,Char,Char)] -> Bool
check (value1,value2) (label1,label2) xs =
	let (x,y,z) = (chec (value1,value2) (label1,label2) xs)!!0
	    bool = if (label1 == x) then (verify (value1,y,value2)) else (verify (value2,y,value1))
	in bool

--Gives the exact condition in with those labels are present
--chec::(Int,Int) -> (Char,Char) -> [(Char,Char,Char)] -> [(Char,Char,Char)]
chec (value1,value2) (label1,label2) [] = []
chec (value1,value2) (label1,label2) ((x,y,z):xs) =
	let  cond = if ((label1 == x && label2 == z) || (label1 == z && label2 == x)) then [(x,y,z)] else chec (value1,value2) (label1,label2) xs
	in cond

--verifies the (value,binaryfunction,value)
--verify::(Int,Char,Int) -> Bool
verify (x,y,z) = case y of '/' -> (x /= z)
                           '<' -> (x < z)
                           '>' -> (x > z)
                           '=' -> (x == z)


--MRV--
--sorth::[(Char, [Int])] -> [(Char, [Int])]
sorth [] = []
sorth (x:xs) =
	let ys = sorth [y | y <- xs, (L.length (snd y)) < (L.length (snd x))]
	    zs = sorth [y | y <- xs, (L.length (snd y)) >= (L.length (snd x))]
	in (ys L.++ [x] L.++ zs)

--The Algorithm
--search::[(Char,[Int])] -> [(Char,Int)] -> [(Char,Char,Char)] -> Maybe [(Char,Int)]
search [] instantiatedVars _ = Just instantiatedVars
search freeVars instantiatedVars cond =
       if (L.null domain) then
           Nothing
       else
           findSolution [search (restrict sortedFreeVars cond (label,val)) ((label,val):instantiatedVars) cond | val <- domain]

       where
           ((label,domain):sortedFreeVars) = sorth freeVars	



findSolution []            = Nothing
findSolution (Nothing:xs)  = findSolution xs
findSolution ((Just x):xs) = Just x
		     


--Access Methods of 3-tuple
--first::(Char,Char,Char) -> Char
first (x,_,_) = x
--second::(Char,Char,Char) -> Char
second (_,x,_) = x
--third::(Char,Char,Char) -> Char
third (_,_,x) = x

--extractLabels gives the list of labels present , whose domain needs to be checked
--extractLabels::[(Char,Char,Char)] -> Char -> [Char]
extractLabels xs label = [ y | (x,op,y)<-xs, x == label] L.++ [ x | (x,op,y)<-xs, y == label]

-- this resticts the domain of each varibale
--restrict::[(Char,[Int])] -> [(Char,Char,Char)] -> (Char,Int) -> [(Char,[Int])]
restrict [] _ _ = []
restrict xs condition (label,value) = --we need to check the list xs in which the label is there. find give the refined list
	let relevantConditions = [ (x,op,y) | (x,op,y)  <- condition, (label == x || label == y)]
	    labels = extractLabels relevantConditions label
	    freevarsincondition = [ (a,b) | (a,b) <- xs, a `L.elem` labels ]
	    freevarsnotincondition = xs L.\\ freevarsincondition
	in freevarsnotincondition L.++ [ (l,newdomain) | (l,dom) <- freevarsincondition, let newdomain = [y | y <- dom, (Handler.Sudoku.check (value,y) (label,l) relevantConditions)] ] --addthe difference


----------------------------------------------------------



--sorting function for answer
sortlab (a1,b1) (a2,b2)
    |(rownumber a1) < (rownumber a2) = LT
    |(rownumber a1) > (rownumber a2) = GT
    |(rownumber a1) == (rownumber a2) = compare (columnnumber a1) (columnnumber a2)

rownumber x = (R.read [x!!1] :: Int)
columnnumber x = (R.read [x!!3] :: Int)

sortedanswer :: Monad m => [(String, Int)] -> m [(String, Int)]
sortedanswer xs = do
    return (L.sortBy sortlab xs)

answerlist [] = []
answerlist ((l,valu):xs) = valu : (answerlist xs)

getSudokuR :: Handler Html
getSudokuR = defaultLayout $ do
    (quest,lis) <- liftIO $ question puzzles
    --let (Just answer) = search quest [] condi
    --sortedans <- sortedanswer answer
    --let answers = fun (answerlist sortedans)
    (iviews, enctype) <- generateFormPost (listEditMForm (labelling lis))
    let y = fun iviews
    setTitle "Sudoku"
    $(widgetFile "sudoku")


--listEditMForm ::(Integer a) => [([Char],Integer)] -> Html -> MForm Handler (FormResult [FormResult a], [FieldView App])
listEditMForm lis extra = do
    ifields <- M.forM lis (\(s,i) -> mreq intField (fromString s){fsName = Just (fromString s)} ((Just i)))
    let (iresults,iviews) = L.unzip ifields
    return ((FormSuccess iresults), iviews)


numberofzeros xs =
    let f = answerlist xs
        zeros= P.filter (\x -> (x==0)) f
        len = (P.fromIntegral (L.length(zeros)))::Double
    in (len/81)


funct (FormSuccess x) = x
lise = labelling []  ----Doesn't matter as the values required obtained from runRequestBo
postSudokuR :: Handler Html
postSudokuR = do
    body <- runRequestBody
    let x = fst body
    let stringconvert = [(Data.Text.unpack w, R.read (Data.Text.unpack e)::Int ) |  (w,e)<- x]
    start <- liftIO $ getCPUTime
    let (Just answer) =  search (help stringconvert) [] condi
    end <- liftIO $ getCPUTime 
    let diff = ((P.fromIntegral (end - start))::Double) / ((10^6)::Double)
    let difficulty = (numberofzeros stringconvert) + diff
    let diffi = if (difficulty < 1) then ("Easy"::String) else if (difficulty < 2.3) then ("Medium"::String) else ("Difficult"::String)
    let sortedans = L.sortBy sortlab answer
    let answers = fun (answerlist sortedans)
    ((results, iviews), enctype) <- runFormPostNoToken (listEditMForm lise)
    defaultLayout $(widgetFile "solution")