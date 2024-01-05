{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module CLI where

import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import DefaultImports
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import System.Directory (doesFileExist)

import Lib
import Move (Move)
import Move qualified

-- very simple console-based UI that works with a single Lib
-- I'm writing this to figure out what the GUI would need

pattern WithArg :: Text -> Text -> Text
pattern WithArg command arg <- (withArg -> Just (command, arg))

pattern Move :: Move -> Text
pattern Move move <- (Move.fromText -> Just move)

data LibLoadError
  = FileNotFound String
  | ParseFailure String
  deriving (Show, Eq)

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
  if not fileExists
    then pure <| Left <| FileNotFound <| toString filePath
    else
      readFileLBS (toString filePath)
        <&> eitherDecode
        <&> first ParseFailure

loadLib' :: Text -> IO Lib
loadLib' =
  loadLib
    >=> \case
      Left err -> fail <| show err
      Right lib -> pure lib

saveLib :: Text -> Lib -> IO ()
saveLib filePath =
  encodePretty
    .> atomicWriteFile (toString filePath)

repl :: Lib -> IO ()
repl l = do
  _ <- printLib' l
  text <- getLine
  case text of
    "quit" -> pass
    "back" -> repl <| back l
    "remove" -> repl (removeR (l ^. moves) l |> back)
    "save" `WithArg` filePath -> (encodePretty l |> writeFileLBS (toString filePath)) >> repl l
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

pickSubcommand :: [String] -> (Maybe Text -> IO ()) -> IO ()
pickSubcommand args gui = case args of
  arg : rest
    | arg == "cli" -> toFileName rest & loadLib' >>= repl
    | arg == "gui" -> gui <| Just <| toFileName rest
    | arg == "merge" -> merge rest
  _ -> gui Nothing
 where
  toFileName = map fromString .> unwords
  merge =
    reverse
      .> uncons
      .> ( \case
            Nothing -> pass
            Just (output, inputs) ->
              reverse inputs
                <&> fromString
                <&> loadLib'
                & sequence
                <&> foldr Lib.merge Lib.empty
                >>= saveLib (fromString output)
         )
