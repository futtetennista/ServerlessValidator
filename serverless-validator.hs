#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-
  - write posts about parsing dynamic json/yaml
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson.Types (typeMismatch)
import Data.Yaml (FromJSON, Value (String, Object), Parser, ParseException, (.:), (.:?), (.!=))
import qualified Data.Yaml as YML (parseEither, decodeFileEither, parseJSON)
import qualified Data.HashMap.Strict as Map (HashMap, insert, toList, empty)
import qualified Data.Text as T (unpack, splitOn)
import qualified Data.Text.Internal (Text)
import qualified Data.CaseInsensitive as CI (mk)
import qualified System.Environment as S (getArgs)

data Serverless
  = S { service :: String
      , provider :: Provider
      , functions :: Functions
      }
  deriving Show


data Runtime
  = NodeJs4_3
  | NodeJs
  deriving Show


toRuntime :: Data.Text.Internal.Text -> Either String Runtime
toRuntime runtime =
  case runtime of
    "nodejs" ->
      Right NodeJs

    "nodejs4.3" ->
      Right NodeJs4_3

    _ ->
      Left $ "Unsupported runtime '" ++ T.unpack runtime ++ "'"


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


data Functions
  = FS { fs :: Map.HashMap Data.Text.Internal.Text Function }
  deriving Show


instance FromJSON Functions where
  parseJSON value =
    case value of
      Object o ->
        case (foldr parseFunction (Right Map.empty) $ Map.toList o) of
          Right funcs ->
            return $ FS funcs

          Left e ->
            fail $ show e

      _ ->
        typeMismatch "Functions" value

    where
      parseFunction ::
        (Data.Text.Internal.Text, Value) -> Either String (Map.HashMap Data.Text.Internal.Text Function) -> Either String (Map.HashMap Data.Text.Internal.Text Function)
      parseFunction func acc =
        case acc of
          Left _ ->
            acc

          Right dict ->
            let
              k =
                fst func

              body =
                YML.parseEither YML.parseJSON (snd func)
            in
              case body of
                Right v ->
                  Right $ Map.insert k v dict

                Left e ->
                  Left e


data Function
  = F { handler :: String
      , deployedName :: Maybe String
      , description :: Maybe String
      , functionRuntime :: Maybe Runtime
      , functionMemorySize :: Maybe Int
      , functionTimeout :: Maybe Int
      , functionEnvironment :: [Environment]
      , events :: [Event]
      }
  deriving Show


instance FromJSON Function where
  parseJSON (Object o) =
    F <$> o .: "handler"
    <*> o .:? "deployedName"
    <*> o .:? "description"
    <*> o .:? "functionRuntime"
    <*> o .:? "functionMemorySize"
    <*> o .:? "functionTimeout"
    <*> o .:? "functionEnvironment" .!= emptyEnvironment
    <*> o .:? "events" .!= [Empty]

  parseJSON invalid =
    typeMismatch "Function" invalid


data Event
  = E { http :: Maybe Http
      -- , streams :: Maybe [String]
      -- , s3 :: Maybe [String]
      -- , schedule :: Maybe [String]
      -- , sns :: Maybe [String]
      }
  | Empty
  deriving Show


type Path = String


data Http
  = Obj { path :: Path
        , method :: HttpMethod
        }
  | Str HttpMethod Path
  deriving Show


data HttpMethod
  = Get
  | Post
  | Put
  | Delete
  | Patch
  deriving Show

toHttpMethod :: Data.Text.Internal.Text -> Maybe HttpMethod
toHttpMethod httpMethod =
  case CI.mk httpMethod of
    "get" ->
      Just Get

    "post" ->
      Just Post

    "put" ->
      Just Put

    "delete" ->
      Just Delete

    _ ->
      Nothing


instance FromJSON Runtime where
  parseJSON (String r) =
    case toRuntime r of
      Right runtime ->
        return runtime

      Left e ->
        fail e

  parseJSON invalid =
    typeMismatch "Runtime" invalid


instance FromJSON Event where
  parseJSON (Object o) =
    E <$> o .:? "http"
    -- <*> o .:? "streams"
    -- <*> o .:? "s3"
    -- <*> o .:? "schedule"
    -- <*> o .:? "sns"

  parseJSON invalid =
    typeMismatch "Event" invalid


instance FromJSON HttpMethod where
  parseJSON (String s) =
    case toHttpMethod s of
      Just httpMethod ->
        return httpMethod

      Nothing ->
        fail "HTTP method must be a string"

  parseJSON invalid =
    typeMismatch "HttpMethod" invalid


instance FromJSON Http where
  parseJSON (Object o) =
    Obj <$> o .: "path"
    <*> o .: "method"

  parseJSON (String config) =
    case (T.splitOn " " config) of
      [httpMethod, endpointPath] ->
        case toHttpMethod $ httpMethod of
          Just m ->
            return $ Str m (T.unpack endpointPath)

          Nothing ->
            fail $ "'" ++ T.unpack httpMethod ++ "' is not a valid HTTP method"

      _ ->
        fail "string must contain a HTTP method and a path"

  parseJSON invalid =
    typeMismatch "Http" invalid


instance FromJSON Provider where
  parseJSON (Object o) =
    P <$> o .: "name"
    <*> o .: "runtime"
    <*> o .:? "memorySize"
    <*> o .:? "timeout"
    <*> o .:? "environment"  .!= emptyEnvironment

  parseJSON invalid =
    typeMismatch "Provider" invalid


instance FromJSON Serverless where
  parseJSON (Object o) =
    S <$> o .: "service"
    <*> o .: "provider"
    <*> o .: "functions"

  parseJSON invalid =
    typeMismatch "Serverless" invalid


d :: String -> IO (Either ParseException Serverless)
d serverlessPath =
  YML.decodeFileEither serverlessPath


main :: IO ()
main =
  do
    -- args <- S.getArgs
    let args = ["fixtures/serverless.yml"]
    case args of
      [] ->
        putStrLn "Usage: ./serverless-validator.hs /path/to/serverless.yml"

      (p:_) ->
        validate p

  where
    validate f =
      do
        res <- d f
        case res of
          Left msg ->
            print msg

          Right _ ->
            putStrLn "The provided serverless.yml is valid"
