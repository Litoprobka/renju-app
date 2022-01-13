{-# LANGUAGE ViewPatterns, PatternSynonyms, LambdaCase #-}

module CLI where

import DefaultImports
import Lib
import System.Directory
import Move (Move)
import qualified Move
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)

-- very simple console-based UI that works with a single Lib
-- I'm writing this to figure out what the GUI would need

pattern WithArg :: Text -> Text -> Text
pattern WithArg command arg <- (withArg -> Just (command, arg))

pattern Move :: Move -> Text
pattern Move move <- (Move.fromText -> Just move)

withArg :: Text -> Maybe (Text, Text)
withArg =
    words .> \case
    [_] -> Nothing
    command : rest -> Just (command, unwords rest)
    _ -> Nothing

printLib' :: Lib -> IO Lib
printLib' l = printLib l >> pure l

repl :: Lib -> IO ()
repl l = do
    printLib l
    text <- getLine
    case text of
        "quit" -> pass
        "back" -> repl <| back l
        "remove" -> repl (removeR (l^.moveSet) l |> back)

        "save" `WithArg` filePath -> (encodePretty l |> BS.writeFile (toString filePath)) >> repl l
        "load" `WithArg` filePath -> do
            fileExists <- doesFileExist <| toString filePath
            if fileExists -- there should be a better way to write this code block
                then do
                    l' <- BS.readFile <| toString filePath
                    case decode l' of
                        Just newLib -> repl newLib
                        Nothing -> putTextLn "invalid lib file" >> repl l
                else putTextLn "file not found" >> repl l

        "mirror" -> repl <| mirror l
        "rotate" -> repl <| rotate l

        "comment" `WithArg` t -> addComment t l |> repl
        (Move move) `WithArg` t -> addBoardText move t l |> repl

        Move move -> repl <| nextMove move l
        "add" `WithArg` (Move move) -> repl <| addMove move l -- nesting custom patterns like this is super neat
        _ -> putTextLn "invalid command" >> repl l


startRepl :: IO ()
startRepl = repl Lib.empty
