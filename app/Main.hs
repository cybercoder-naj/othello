{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Types
import qualified Lib
import System.Console.ANSI
import Control.Monad.State
import GHC.IO.Handle (hFlush, Handle)
import GHC.IO.Handle.FD (stdout)
import System.IO (hReady)

data IOState = IOState {
    coordinates :: (Int, Int)
    , showTargets :: Bool
    , quit :: Bool
}

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

main :: IO ()
main = do
    clearFromCursorToScreenBeginning
    setCursorPosition 0 0

    putStrLn "======== Welcome to Othello (Battle Reversi) ========"
    putStrLn "======== Player 1 is Black and they start ========\n"
    let gState = Lib.initGameState
    let ioState = IOState (0, 0) False False

    _ <- runStateT runGame (gState, ioState)
    pure ()

runGame :: StateT (Types.RawState, IOState) IO ()
runGame = do
    (gState, ioState) <- get
    if quit ioState then do
        return ()
    else do
        liftIO clearFromCursorToScreenBeginning
        liftIO $ setCursorPosition 0 0

        liftIO $ printBoard gState ioState
        liftIO $ putStr "Enter your action (Use ? for help): "
        liftIO $ hFlush stdout

        input <- liftIO getLine
        liftIO $ putStr "\n"

        handleAction $ head input
        runGame

handleAction :: Char -> StateT (Types.RawState, IOState) IO ()
handleAction 'q' = do 
    (gState, ioState) <- get
    put (gState, ioState { quit = True }) 
handleAction 'j' = do 
    (gState, ioState) <- get
    put (gState, ioState { coordinates = goDown (coordinates ioState) })
    where
        goDown (x, y)
          | x == 7    = (7, y)
          | otherwise = (x + 1, y)
handleAction 'k' = do 
    (gState, ioState) <- get
    put (gState, ioState { coordinates = goUp (coordinates ioState) })
    where
        goUp (x, y)
          | x == 0    = (0, y)
          | otherwise = (x - 1, y)
handleAction 'h' = do 
    (gState, ioState) <- get
    put (gState, ioState { coordinates = goLeft (coordinates ioState) })
    where
        goLeft (x, y)
          | y == 0    = (x, 0)
          | otherwise = (x, y - 1)
handleAction 'l' = do 
    (gState, ioState) <- get
    put (gState, ioState { coordinates = goRight (coordinates ioState) })
    where
        goRight (x, y)
          | y == 7    = (x, 7)
          | otherwise = (x, y + 1)
handleAction 'p' = do 
    (gState, ioState) <- get
    -- TODO need to do the turn here
    put (gState, ioState) 
handleAction 's' = do
    (gState, ioState) <- get
    -- TODO need to show the targets
    put (gState, ioState) 
handleAction _    = undefined

printBoard :: Types.RawState -> IOState -> IO ()
printBoard (_, Types.B cells) ioState = do
    let (x, y) = coordinates ioState

    let innerFor j (i, cell) = if i == x && j == y then do {
        setSGR [SetColor Background Vivid White, SetColor Foreground Vivid Black];
        putStr . show $ cell;
        setSGR [Reset];
        putStr " ";
    } else do {
        putStr . show $ cell;
        putStr " ";
    }
    let outerFor i row = forEachIndexed innerFor (map (i,) row) >> putStrLn ""

    forEachIndexed outerFor cells

forEach :: (a -> IO ()) -> [a] -> IO ()
forEach f = foldr ((>>) . f) (pure ())

forEachIndexed :: (Int -> a -> IO ()) -> [a] -> IO ()
forEachIndexed f xs = foldr (\(i, x) acc -> f i x >> acc) (pure ()) (zip [0..] xs)