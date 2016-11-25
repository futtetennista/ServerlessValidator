#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
import Turtle
import qualified Control.Foldl as Fold
import qualified Data.List as L (head, drop, filter, (\\), null, length)
import qualified Data.Text as T


docker_ps :: Shell Text
docker_ps =
  inproc "docker" ["ps", "-a"] empty

docker_rm :: [Text] -> Shell Text
docker_rm ids =
  inproc "docker" ("rm" : ids) empty

minus :: Eq a => [a] -> [a] -> [a]
minus =
  (L.\\)

parseFirstColumn :: Text -> Text
parseFirstColumn =
  L.head . T.words

removeHeader :: [Text] -> [Text]
removeHeader xs =
  L.drop 1 xs

main :: IO ()
main =
  do
    whiteListedContainerIds <- options "A simple script to delete 'zombie' docker containers" $ many (argText "ids" "Zero or more container ids")
    putStrLn "The following containers won'be deleted:"
    mapM_ (putStrLn . T.unpack) whiteListedContainerIds
    outputFirstColumns <- fold (fmap parseFirstColumn docker_ps) Fold.list
    let containerIds = removeHeader outputFirstColumns
    let deleteContainerIds = containerIds `minus` whiteListedContainerIds
    case deleteContainerIds of
      [] ->
        putStrLn "Hurray! Nothing to remove"

      _ ->
        do
          sh $ docker_rm deleteContainerIds
          putStrLn $ "Hurray! Deleted " ++ (show $ L.length deleteContainerIds) ++ " 'zombie' containers"
