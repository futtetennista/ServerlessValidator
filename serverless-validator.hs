#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Serverless.yml reference: https://serverless.com/framework/docs/providers/aws/guide/serverless.yml/

import Data.Text.Internal (Text)
import Data.Traversable (for)
import Control.Monad (forM_)
import Data.Aeson.Types (Object, typeMismatch, withObject)
import Data.Yaml (FromJSON, Value (String, Object), Parser, ParseException, (.:), (.:?), (.!=))
import qualified Data.Yaml as YML (decodeFileEither, parseJSON)
import qualified Data.HashMap.Strict as Map (toList)
import qualified Data.Text as T (unpack, splitOn)
import qualified Data.CaseInsensitive as CI (mk)
import qualified System.Environment as S (getArgs)


data Serverless
  = S { service :: String
      , provider :: Provider
      , functions :: Functions
      }
  deriving Show


instance FromJSON Serverless where
  parseJSON (Object o) =
    S <$> o .: "service"
    <*> o .: "provider"
    <*> o .: "functions"

  parseJSON invalid =
    typeMismatch "Serverless" invalid


data Runtime
  = NodeJs4_3
  | NodeJs
  deriving Show


instance FromJSON Runtime where
  parseJSON (String r) =
    case toRuntime r of
      Right rt ->
        return rt

      Left e ->
        fail e

  parseJSON invalid =
    typeMismatch "Runtime" invalid


toRuntime :: Data.Text.Internal.Text -> Either String Runtime
toRuntime rt =
  case rt of
    "nodejs" ->
      Right NodeJs

    "nodejs4.3" ->
      Right NodeJs4_3

    _ ->
      Left $ "Unsupported runtime '" ++ T.unpack rt ++ "'"


type Environment = (String, String)


emptyEnvironment :: [Environment]
emptyEnvironment =
  []


data Provider
  = P { name :: String
      , globalRuntime :: Runtime
      , globalMemorySize :: Maybe Int
      , globalTimeout :: Maybe Int
      , globalEnvironment :: [Environment]
      }
  deriving Show


instance FromJSON Provider where
  parseJSON (Object o) =
    P <$> o .: "name"
    <*> o .: "runtime"
    <*> o .:? "memorySize"
    <*> o .:? "timeout"
    <*> o .:? "environment" .!= emptyEnvironment

  parseJSON invalid =
    typeMismatch "Provider" invalid


newtype Functions =
  FS [Function]
  deriving Show


instance FromJSON Functions where
  parseJSON value =
    withObject "Functions" parseFunctions value

    where
      parseFunctions :: Object -> Parser Functions
      parseFunctions obj =
        fmap FS . for (Map.toList obj) $ \(n, v) -> parseFunction n v


data Function =
  F { functionName :: Text
    , functionHandler :: Text
    , functionDeployedName :: Maybe Text
    , functionDescription :: Maybe Text
    , functionRuntime :: Maybe Runtime
    , functionMemorySize :: Maybe Int
    , functionTimeout :: Maybe Int
    , functionEnvironment :: [Environment]
    , functionEvents :: [Event]
    }
  deriving Show


parseFunction :: Text -> Value -> Parser Function
parseFunction fName fBody =
  withObject "Function" (\fObj -> parseFunctionBody fObj) fBody

  where
    parseFunctionBody :: Object -> Parser Function
    parseFunctionBody obj =
      F <$> return fName
      <*> obj .: "handler"
      <*> obj .:? "deployedName"
      <*> obj .:? "description"
      <*> obj .:? "runtime"
      <*> obj .:? "memorySize"
      <*> obj .:? "timeout"
      <*> obj .:? "environment" .!= emptyEnvironment
      <*> obj .: "events"


type Path = Text


data Event
  = HttpEvent { httpEventPath :: Path
              , httpEventMethod :: HttpMethod
              , httpEventCors :: Maybe Bool
              , httpEventPrivate :: Maybe Bool
              }
  | S3Event { s3EventBucket :: Text
            , s3EventEvent :: Text
            , s3EventRules :: [S3EventRule]
            }
  | ScheduleEvent
  | SnsEvent
  | StreamEvent
  | EmptyEvent Text
  deriving Show


data HttpMethod
  = Get
  | Post
  | Put
  | Delete
  | Patch
  deriving Show


data S3EventRule
  = Prefix Text
  | Suffix Text
  deriving Show


instance FromJSON S3EventRule where
  parseJSON value =
    withObject "S3 Event Rule" (\obj -> parseRule $ Map.toList obj) value

    where
      parseRule :: [(Text, Value)] -> Parser S3EventRule
      parseRule entries =
        case entries of
          [("suffix", String suffix)] ->
            return $ Suffix suffix

          [("prefix", String prefix)] ->
            return $ Prefix prefix

          _ ->
            typeMismatch "'prefix' or 'suffix' string" value


instance FromJSON Event where
  parseJSON value =
    withObject "Event" (\obj -> parseEvent $ Map.toList obj)  value

    where
      parseEvent :: [(Text, Value)] -> Parser Event
      parseEvent xs =
        case xs of
          [(eventName, eventConfig)] ->
            case eventName of
              "http" ->
                parseHttpEvent eventConfig

              "s3" ->
                parseS3Event eventConfig

              _ ->
                return $ EmptyEvent eventName

          _ ->
            typeMismatch "Event" value


parseS3Event :: Value -> Parser Event
parseS3Event (Object o) =
  S3Event <$> o .: "bucket"
  <*> parseEventType
  <*> o .:? "rules" .!= []
  where
    -- http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html#notification-how-to-event-types-and-destinations
    parseEventType :: Parser Text
    parseEventType =
      o .: "event"

parseS3Event invalid =
  typeMismatch "S3 Event" invalid


parseHttpEvent :: Value -> Parser Event
parseHttpEvent (String config) =
  case (T.splitOn " " config) of
    [httpMethodStr, httpEndpoint] ->
      case toHttpMethod $ httpMethodStr of
        Right m ->
          return HttpEvent { httpEventPath = httpEndpoint
                           , httpEventMethod = m
                           , httpEventCors = Nothing
                           , httpEventPrivate = Nothing
                           }

        Left err ->
          fail $ "Unknown HTTP method: " ++ err

    _ ->
      fail "HTTP string must contain only a HTTP method and a path, i.e. 'http: GET foo'"

parseHttpEvent (Object obj) =
  HttpEvent <$> obj .: "path"
  <*> obj .: "method"
  <*> obj .:? "cors"
  <*> obj .:? "private"

parseHttpEvent invalid =
  typeMismatch "HTTP Event" invalid


toHttpMethod :: Text -> Either String HttpMethod
toHttpMethod httpMethodStr =
  case CI.mk httpMethodStr of
    "get" ->
      Right Get

    "post" ->
      Right Post

    "put" ->
      Right Put

    "delete" ->
      Right Delete

    str ->
      Left $ show str


instance FromJSON HttpMethod where
  parseJSON (String str) =
    case toHttpMethod str of
      Right m ->
        return m

      Left err ->
        fail $ "Unknown HTTP method: " ++ err

  parseJSON invalid =
    typeMismatch "HttpMethod" invalid


decode :: String -> IO (Either ParseException Serverless)
decode serverlessPath =
  YML.decodeFileEither serverlessPath


main :: IO ()
main =
  do
    args <- S.getArgs
    let args = [ "fixtures/serverless.yml"
               , "fixtures/serverless-bogus.yml"
               ]
    case args of
      [] ->
        putStrLn "Usage: ./serverless-validator.hs /path/to/serverless.yml"

      xs ->
        forM_ xs validate

  where
    validate f =
      do
        res <- decode f
        case res of
          Left msg ->
            print msg

          Right serverless ->
            do
              print serverless
              putStrLn $ "The provided file '" ++ f ++ "' is valid"
