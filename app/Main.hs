module Main where

import Lib
import Art
import Control.Monad.Trans.Maybe (runMaybeT)
import System.Random (getStdRandom, Random(randomR)) 
import Control.Monad.State (execStateT)

main :: IO ()
main = do

    titleScreen title

    maybeUser <- runMaybeT $ do -- welcomes the user
        user <- username
        pure user
    case maybeUser of
        Nothing -> putStrLn "You cannot play anonymously, press any key to identify again." >> getChar >> main
        Just user -> putStrLn $ "Hello " ++ user ++ ", here is a representation of the battlefield, your enemy is hidding behind a cell of this " ++ show xAxis ++ "x" ++ show yAxis ++ " grid:"
        
    drawBattlefield baseGrid -- shows the battlefield

    enemyXCoord <- getStdRandom (randomR (1, xAxis))
    enemyYCoord <- getStdRandom (randomR (1, yAxis))
    let enemyLocation = (enemyXCoord, enemyYCoord) -- creates a pair of coordinates for the enemy location
 
    try <- execStateT (target enemyLocation) 0 -- the user attacks a position
    if try == 1 then putStrLn "Success on the first try!" else putStrLn $ "Succes after " ++ show try ++ " tries."
