{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Types
import qualified Utils
import qualified Lib
import System.Console.ANSI
import Control.Monad.State
import GHC.IO.Handle (hFlush, Handle)
import GHC.IO.Handle.FD (stdout)
import System.IO (hReady)
import Data.Map (Map)
import qualified Data.Map as M


data IOState = IOState {
    coordinates :: (Int, Int)
    , showTargets :: Bool
    , quit :: Bool
}

type RawState       = (Types.GameState, IOState)
type CombinedState  = StateT RawState IO

-- UNUSED
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

runGame :: CombinedState ()
runGame = do
    rawState@(gState, ioState) <- get
    if quit ioState then do
        return ()
    else do
        liftIO clearFromCursorToScreenBeginning
        liftIO $ setCursorPosition 0 0

        liftIO $ printBoard rawState

        if Utils.even (fst gState) then
            liftIO $ putStr "Player 1: "
        else
            liftIO $ putStr "Player 2: "
        liftIO $ putStr "Enter your action (Use ? for help): "
        liftIO $ hFlush stdout

        actions <- liftIO getLine
        liftIO $ putStr "\n"

        handleActions actions
        runGame

handleActions :: String -> CombinedState ()
handleActions actions = do
    if null actions then
        return ()
    else do
        let (a : as) = actions

        if M.notMember a transformer then
            return ()
        else do
            modify (transformer M.! a)
            handleActions as
            where
                transformer :: Map Char (RawState -> RawState)
                transformer = M.fromList [
                    ('q', \(g, io) -> (g, io { quit = True })),
                    ('j', \(g, io) -> (g, io { coordinates = goDown $ coordinates io })),
                    ('k', \(g, io) -> (g, io { coordinates = goUp $ coordinates io })),
                    ('h', \(g, io) -> (g, io { coordinates = goLeft $ coordinates io })),
                    ('l', \(g, io) -> (g, io { coordinates = goRight $ coordinates io })),
                    ('p', \(g, io) -> (execState (Lib.place (coordinates io)) g, io)),
                    ('s', \(g, io) -> (g, io { showTargets = not $ showTargets io }))]

                goDown (x, y)
                    | x == 7    = (7, y)
                    | otherwise = (x + 1, y)

                goUp (x, y)
                    | x == 0    = (0, y)
                    | otherwise = (x - 1, y)

                goLeft (x, y)
                    | y == 0    = (x, 0)
                    | otherwise = (x, y - 1)

                goRight (x, y)
                    | y == 7    = (x, 7)
                    | otherwise = (x, y + 1)

printBoard :: RawState -> IO ()
printBoard ((_, Types.B cells), ioState) = do
    let (x, y) = coordinates ioState

    let printCell cell = do {
        if Utils.isTarget cell && showTargets ioState then
            putStr . show $ cell;
        else if Utils.isTarget cell && not (showTargets ioState) then
            putStr . show $ Types.Empty;
        else
            putStr . show $ cell;
    }

    let innerFor j (i, cell) = if i == x && j == y then do {
        setSGR [SetColor Background Vivid White, SetColor Foreground Vivid Black];
        printCell cell;
        setSGR [Reset];
        putStr " ";
    } else do {
        printCell cell;
        putStr " ";
    }
    let outerFor i row = forEachIndexed innerFor (map (i,) row) >> putStrLn ""

    forEachIndexed outerFor cells

forEach :: (a -> IO ()) -> [a] -> IO ()
forEach f = foldr ((>>) . f) (pure ())

forEachIndexed :: (Int -> a -> IO ()) -> [a] -> IO ()
forEachIndexed f xs = foldr (\(i, x) acc -> f i x >> acc) (pure ()) (zip [0..] xs)