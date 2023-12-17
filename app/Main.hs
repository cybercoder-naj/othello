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

    let gState = Lib.initGameState
    let ioState = IOState (0, 0) False

    _ <- runStateT runGame (gState, ioState)
    pure ()

runGame :: CombinedState ()
runGame = do
    rawState@(gState, ioState) <- get
    if quit ioState || terminated gState then do
        return ()
    else do
        liftIO clearFromCursorToScreenBeginning
        liftIO $ setCursorPosition 0 0
        liftIO $ putStrLn "======== Welcome to Othello (Battle Reversi) ========"
        liftIO $ putStrLn "======== Player 1 is Black and they start ========\n"

        liftIO $ printBoard rawState

        if Utils.even (fst gState) then
            liftIO $ putStr "Player 1: "
        else
            liftIO $ putStr "Player 2: "
        liftIO $ putStr "Enter your action (Use ? for help): "
        liftIO $ hFlush stdout

        actions <- liftIO getLine
        liftIO $ putStr "\n"

        canPlay <- checkTargets
        if canPlay then do
            handleActions actions
            runGame
        else do
            modify (\((Types.N c, board), io) -> ((Types.N (c + 1), board), io))
            runGame

checkTargets :: CombinedState Bool
checkTargets = do
    ((_, Types.B cells), _) <- get
    return $ any (any Utils.isTarget) cells

terminated :: Types.GameState -> Bool
terminated (_, Types.B cells) = not . any (any Utils.isEmpty) $ cells

handleActions :: String -> CombinedState ()
handleActions actions = do
    if null actions then
        return ()
    else do
        let (a : as) = actions

        if a == '?' then do
            liftIO $ print a
            liftIO printHelp
            return ()
        else if M.notMember a transformer then
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
                    ('p', \(g, io) -> (execState (Lib.place (coordinates io)) g, io ))]

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

printHelp :: IO ()
printHelp = do
    liftIO clearFromCursorToScreenBeginning
    liftIO $ setCursorPosition 0 0

    putStrLn "These are the keybindings:\n"
    putStrLn " j: go up"
    putStrLn " k: go down"
    putStrLn " h: go left"
    putStrLn " l: go right"
    putStrLn " q: quit"
    putStrLn " p: place piece on target"
    putStrLn " ?: print this help\n"

    putStrLn "Press <Enter> to continue"
    _ <- getLine
    return ()

printBoard :: RawState -> IO ()
printBoard ((_, Types.B cells), ioState) = do
    let (x, y) = coordinates ioState

    let printCell cell = do {
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