
module Helpers
( lengthOfMarix
,evenUpTo20
,d
,addToList
)
where

import System.IO

list = [3,5,7,11,13,17,1]
-- find length of the list
len = length list
-- get element in the list
atIndex1 = list !! 1
-- Check if value is in list
isInList = 7 `elem` list
-- We can filter the results to only show values divisible by 9
pow3ListDiv9 = [3^n | n <- [1..10], 3^n `mod` 9 == 0]

-- Generate a multiplication table by multiplying x * y where y has the values
-- 1 through 10 and where x does as well
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

listBiggerThan5 = filter(>5) list
-- while loop
evenUpTo20 = takeWhile ( <= 20) [2,4..]
-- TO RUN IN CMMD LINE: ghc --make nemaOfTheFile

-- You can define a type declaration for functions
-- funcName :: param1 -> param2 -> returnType
addMe :: Int -> Int -> Int

-- funcName param1 param2 = operations (Returned Value)
-- Execute with : addMe 4 5
addMe x y = x + y

-- Without type declaration you can add floats as well
sumMe x y = x + y

-- You can also add tuples : addTuples (1,2) (3,4) = (4,6)
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

-- You can perform different actions based on values
whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"

-- The default
whatAge x = "Nothing Important"

-- We can use guards that provide different actions based on conditions
isOdd :: Int -> Bool
isOdd n
	-- if the modulus using 2 equals 0 return False
	| n `mod` 2 == 0 = False
	
	-- Else return True
	| otherwise = True
	
-- This could be shortened to
isEven n = n `mod` 2 == 0

-- Use guards to define the school to output
whatGrade :: Int -> String
whatGrade age
	| (age >= 5) && (age <= 6) = "Kindergarten"
	| (age > 6) && (age <= 10) = "Elementary School"
	| (age > 10) && (age <= 14) = "Middle School"
	| (age > 14) && (age <= 18) = "High School"
	| otherwise = "Go to college"

	-- The where clause keeps us from having to repeat a calculation
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "Terrible Batting Average"
	| avg <= 0.250 = "Average Player"
	| avg <= 0.280 = "Your doing pretty good"
	| otherwise = "You're a Superstar"
	where avg = hits / atBats 


boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

lengthOfMarix :: [[Char]] -> Int
lengthOfMarix [] = 0
lengthOfMarix (x:xs) = 1 + (lengthOfMarix xs)

slidingTest :: [[Char]] -> (Int, Int) -> IO()
slidingTest (row:rows) (i,j) 
    | i > len = print i
       where len = lengthOfMarix (row:rows)

d :: [a] -> Int -> [a]
d (row:rows) b  
	| b == 1 = c
	| otherwise = (row:rows)
	where c = drop 1 (row:rows)

-- findAllPlayers :: [[Char]] -> [Char]
-- findAllPlayers [] = []
-- findAllPlayers (row:rows)
-- 	| a `elem` row = [a] ++ (findAllPlayers rows)
-- 	| b `elem` row = [b] ++ (findAllPlayers rows)
-- 	| c `elem` row = [c] ++ (findAllPlayers rows)
-- 	| d `elem` row = [d] ++ (findAllPlayers rows) 
-- 	| otherwise = (findAllPlayers rows)
-- 	where
-- 		a = '1'
-- 		b = '2'
-- 		c = '3'
-- 		d = '4'

addToList :: Char -> [Char] -> [Char]
addToList a [] = [a]
addToList a (row:rows) = row: addToList a rows


findAllPlayers :: [[Char]] -> [Char]
findAllPlayers [] = []
findAllPlayers (row:rows) = (checkPlayers row) ++ (findAllPlayers rows)
    -- | '1' `elem` row = ['1'] ++ (findAllPlayers rows) 
    -- | '2' `elem` row = ['2'] ++ (findAllPlayers rows) 
    -- | '3' `elem` row = ['3'] ++ (findAllPlayers rows) 
    -- | '4' `elem` row = ['4'] ++ (findAllPlayers rows)  
    -- | otherwise = (findAllPlayers rows)


checkPlayers :: [Char] -> [Char]
checkPlayers [] = []
checkPlayers a
    | '1' == h = ['1'] ++ checkPlayers (drop 1 a)
    | '2' == h = ['2'] ++ checkPlayers (drop 1 a)
    | '3' == h = ['3'] ++ checkPlayers (drop 1 a)
    | '4' == h = ['4'] ++ checkPlayers (drop 1 a)
    | otherwise = checkPlayers (drop 1 a)
    where h = head a
         


