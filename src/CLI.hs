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

data LibLoadError
    = FileNotFound
    | ParseFailure
    deriving (Enum, Show, Eq)

withArg :: Text -> Maybe (Text, Text)
withArg =
    words .> \case
    [_] -> Nothing
    command : rest -> Just (command, unwords rest)
    _ -> Nothing

printLib' :: Lib -> IO Lib
printLib' l = putText (printLib l) >> pure l

loadLib :: Text -> IO (Either LibLoadError Lib)
loadLib filePath = do
    fileExists <- doesFileExist <| toString filePath
    if not fileExists then
            pure <| Left FileNotFound
        else
            BS.readFile (toString filePath)
            <&> decode
            <&> \case
                Just newLib -> Right newLib
                Nothing -> Left ParseFailure

saveLib :: Text -> Lib -> IO ()
saveLib filePath =
    encodePretty
    .> BS.writeFile (toString filePath)

repl :: Lib -> IO ()
repl l = do
    _ <- printLib' l
    text <- getLine
    case text of
        "quit" -> pass
        "back" -> repl <| back l
        "remove" -> repl (removeR (l^.moves) l |> back)

        "save" `WithArg` filePath -> (encodePretty l |> BS.writeFile (toString filePath)) >> repl l
        "load" `WithArg` filePath ->
            loadLib filePath
            >>= \case
                Left err -> print err >> repl l
                Right newLib -> repl newLib 

        "mirror" -> repl <| mirror l
        "rotate" -> repl <| rotate l

        "comment" `WithArg` t -> addComment t l |> repl
        (Move move) `WithArg` t -> addBoardText move t l |> repl

        Move move -> repl <| nextMove move l
        "add" `WithArg` (Move move) -> repl <| addMove move l -- nesting custom patterns like this is super neat
        _ -> putTextLn "invalid command" >> repl l


startRepl :: IO ()
startRepl = repl Lib.empty
