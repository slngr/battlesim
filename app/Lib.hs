module Lib where

import Data.List (intercalate)
import System.Console.ANSI -- terminal control
import Control.Monad.Trans.Maybe -- MaybeT
import Data.Char (isLetter, isNumber, digitToInt)
import Control.Monad.State (StateT, lift, modify) 
import System.Exit (exitSuccess)

type Grid = String
baseGrid :: Grid
baseGrid = init (drop 1 (intercalate (replicate xAxis cell) (replicate (yAxis +1) "\n"))) -- the battlefield
xAxis :: Int
xAxis = 5
yAxis :: Int
yAxis = 3
cell :: Char
cell = '+'

type Coordinates = (Int, Int)
selection :: Char
selection = '@'
charToString :: a -> [a]
charToString = (:[])

resetScreen :: IO ()
resetScreen = do
    setCursorPosition 0 0
    clearScreen

titleScreen :: String -> IO ()
titleScreen title = do
    resetScreen
    setSGR [SetColor Background Dull Red]
    setSGR [SetColor Foreground Vivid White]
    putStrLn title
    setSGR [Reset]

username :: MaybeT IO String
username = MaybeT $ do
    putStrLn "Welcome, please enter your name."
    string <- getLine
    resetScreen
    if any isLetter string
        then pure $ Just string
        else pure Nothing

drawBattlefield :: Grid -> IO ()
drawBattlefield grid = do
    setSGR [SetColor Background Dull Green]
    setSGR [SetColor Foreground Vivid White]
    putStrLn grid
    setSGR [Reset]

target :: Coordinates -> StateT Int IO ()
target enemyLocation = do
    
    lift $ putStrLn "What coordinates are you targeting? Directly input two digits for the X and Y values."
    
    charX <- lift getChar -- gets a character for the X position chosen by the user
    let xCoord = if isNumber charX then digitToInt charX else 0
    charY <- lift getChar -- gets a character for the Y position chosen by the user
    let yCoord = if isNumber charY then digitToInt charY else 0
    let aim = (xCoord, yCoord) -- creates the pair of coordinates to target

    let targetedGrid :: Coordinates -> Grid
        targetedGrid theAim = init (concat (replicate ((snd theAim)-1) (replicate (xAxis) cell ++ "\n")) ++ init (replicate (fst theAim) cell) ++ charToString selection ++ replicate (xAxis-(fst theAim)) cell ++ "\n" ++ concat (replicate (yAxis-(snd theAim)) (replicate xAxis cell ++ "\n")))

    lift resetScreen
    modify (+1) -- increments the number of tries
    
    let monitor :: Coordinates -> StateT Int IO ()
        monitor theAim
            | fst theAim == 0 = disgrace
            | snd theAim == 0 = disgrace
            | fst theAim > xAxis = disgrace
            | snd theAim > yAxis = disgrace
            | otherwise = valid
            where disgrace = lift $ putStrLn ("Invalid coordinates, you are a disgrace to the academy, get lost!") >> exitSuccess
                  valid = lift $ putStr ("You chose " ++ show theAim)
    monitor aim -- checks for valid coordinates

    case compare aim enemyLocation of
        EQ -> lift $ putStrLn " and destroyed the enemy!" >> drawBattlefield (targetedGrid aim)
        _ -> do
            lift $ putStrLn " and missed the enemy, try again." >> drawBattlefield (targetedGrid aim)
            target enemyLocation
