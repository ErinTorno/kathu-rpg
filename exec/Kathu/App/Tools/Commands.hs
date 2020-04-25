module Kathu.App.Tools.Commands
    ( Command(..)
    , CommandState
    , newCommandState
    , runCommand
    , redoNextCommand
    , undoLastCommand
    ) where

import           Apecs
import           Data.IORef
import           Control.Monad              (unless)

import           Kathu.App.System            (SystemT')

data Command = Command
    { applyCommand  :: !(SystemT' IO ())
    , removeCommand :: !(SystemT' IO ())
    }

data CommandState = CommandState
    { performedCommands :: !(IORef [Command])
    , undoneCommands    :: !(IORef [Command])
    }

newCommandState :: IO CommandState
newCommandState = CommandState <$> newIORef [] <*> newIORef []

runCommand :: CommandState -> Command -> SystemT' IO ()
runCommand st cmd = do
    let pcmdRef = performedCommands st
        ucmdRef = undoneCommands st
        
    applyCommand cmd
    lift $ do
        performedCmds <- readIORef pcmdRef

        writeIORef pcmdRef (cmd:performedCmds)
        -- we clear the undone commands, as we have "diverged" from the previous undo-redo change set
        writeIORef ucmdRef []

redoNextCommand :: CommandState -> SystemT' IO ()
redoNextCommand = runCommandAndSwapBetweenStacks applyCommand undoneCommands performedCommands

undoLastCommand :: CommandState -> SystemT' IO ()
undoLastCommand = runCommandAndSwapBetweenStacks removeCommand performedCommands undoneCommands

runCommandAndSwapBetweenStacks :: (Command -> SystemT' IO ()) -> (CommandState -> IORef [Command]) -> (CommandState -> IORef [Command]) -> CommandState -> SystemT' IO ()
runCommandAndSwapBetweenStacks runCommandAction getPopList getPushList st = do
    let popCmdRef  = getPopList st
        pushCmdRef = getPushList st

    poppedCmds <- lift $ readIORef popCmdRef

    unless (null poppedCmds) $ do
        let lastCmd = head poppedCmds
            ucmdRef = undoneCommands st

        runCommandAction lastCmd
        lift $ do
            pushedCmds <- readIORef pushCmdRef
            -- moves command from applied to undone
            writeIORef ucmdRef (lastCmd:pushedCmds)
            writeIORef popCmdRef (tail pushedCmds)