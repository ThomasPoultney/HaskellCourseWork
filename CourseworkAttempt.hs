--group Number: 128
--Group Members : Alex Pearce 91398, Thomas Poultney 963541, Benjamin joseph Rochford 969573

import Test.QuickCheck
import Data.List.Split
import System.IO
-- Question 1
-- checks how many are above average using guards
howManyAboveAverage1::    Float -> Float -> Float -> Int
howManyAboveAverage1 x y z
    |x > average && y > average && z <= average = 2
    |x > average && y <= average && z > average = 2
    |x <= average && y > average && z > average = 2
    |x > average && y <= average && z <= average = 1
    |x <= average && y > average && z <= average = 1
    |x <= average && y <= average && z > average = 1
    | otherwise   = 0
    where average = ((x + y + z) / 3)
-- *Main> howManyAboveAverage1 963541 969573 913987
-- 2
howManyAboveAverage2 :: Float-> Float -> Float -> Int
howManyAboveAverage2 x y z = length (filter (>average x y z) [x,y,z])

average :: Float -> Float -> Float -> Float
average x y z = ((x + y + z) / 3)
--Main> howManyAboveAverage1 963541 969573 913987
--2

-- result: +++ OK, passed 100 tests.
checkcorrectness  :: Float -> Float -> Float -> Bool
checkcorrectness x y z = howManyAboveAverage1 x y z == howManyAboveAverage2 x y z

--Some test results
testGrid = [[True,False,True],[False,True,True],[True,False,True]]
testString = "O.O\n.OO\nO.O\n...\nOOO"

--Q4

expmod :: Integer -> Integer -> Integer -> Integer
--M,e,n
expmod x y z = if y == 0 then 1
 else ((expmod x (y - 1) z) * x) `mod` z

--if the exponent is divisible by 2 then it is halved and the function is called again with  (e / 2)
--otherwise it calls again with (e - 1), this makes it EVEN so it is halved and repeat
expmodfast :: Integer -> Integer -> Integer -> Integer
expmodfast x y z = if y == 0 then 1
 else if y `mod` 2 == 0 then ((expmodfast x (y `div` 2) z) ^ 2) `mod` z
 else ((expmodfast x (y - 1) z) * x) `mod` z

-- Question 5
-- a) Checks if grid is valid by first making a new list with only lists of boolean that
-- are the same length as the list at the head. it then compares length of newly created list
-- with original to see if any of the lists in original was removed. if no list were removed all
-- are same as head

valid :: [[Bool]] -> Bool
valid xs = length xs == length[x | x <- xs, length x == length(head xs)]

-- b) converts grid to a string by calling another function which coverts each list
-- in the list of bools into a string and returns to original function.
showGrid :: [[Bool]] -> String
showGrid [] = ""
showGrid (x:xs) = calculateRowString x ++ "\n" ++showGrid xs

calculateRowString :: [Bool] -> String
calculateRowString[] =  ""
calculateRowString (x:xs) = if (x == True) then "O" ++ calculateRowString xs else "." ++ calculateRowString xs

-- prints grid to screen recursively.
printGrid :: [[Bool]] -> IO()
printGrid [] = return ()
printGrid (x:xs) = do putStr(calculateRowString x)
                      putChar '\n'
                      printGrid xs

--5c) splits string into into sepearte lists where \n is found
readGrid :: String -> [[Bool]]
readGrid xs = breakUpList(splitString xs)

-- takes string of chars and converts them to bools recursivly
breakUpList ::  [[Char]] -> [[Bool]]
breakUpList [] = return []
breakUpList (x:xs) = convertCharToBool x:breakUpList xs

--converts individual lists from char to bool
convertCharToBool :: [Char] -> [Bool]
convertCharToBool xs =  [toBool x |x <- xs]

-- used to map each element from char to bool
toBool :: Char -> Bool
toBool x = if x == 'O' then True else
      False

--splits string
splitString :: String -> [[Char]]
splitString xs = splitOn("\n") xs

--5d)
neighbours :: [Bool] -> Int
neighbours [] = 0
neighbours (x:xs) = toInt x + neighbours xs

newCell :: Int -> Bool -> Bool
newCell x y
  |y == True && x < 2 = False
  |y == True && x > 3 = False
  |y == True && x >2 && x <=3 = True
  |y == False && x==3 = True
  |otherwise = False


--step :: [[Bool]] -> [[Bool]]
--step xs = if (valid xs == true) then  gridWidth = length xs
--          else error "Invalid Grid"


toInt :: Bool -> Int
toInt x = if x==True
then 1
else 0


--5e)
loadGridFile :: IO()
loadGridFile = do
    withFile "test.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)
