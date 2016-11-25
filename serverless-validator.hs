#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-
  - write posts about parsing dynamic json/yaml
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Yaml
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as Map (HashMap, insert, toList, empty)
import qualified Data.Text as T (unpack, splitOn)
import qualified Data.Text.Internal (Text)
import qualified Data.CaseInsensitive as CI (mk)

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


data Provider
  = P { name :: String
      , runtime :: String
      , memorySize :: Maybe Int
      , timeout :: Maybe Int
      }
  deriving Show


data Functions
  = FS { fs :: Map.HashMap Data.Text.Internal.Text Function }
  deriving Show


data Function
  = F { handler :: String
      , deployedFunctionName :: Maybe String
      , events :: [Event]
      }
  deriving Show


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


instance FromJSON Function where
  parseJSON (Object o) =
    F <$> o .: "handler"
    <*> o .:? "deployedFunctionName"
    <*> o .:? "events" .!= [Empty]

  parseJSON invalid =
    typeMismatch "Function" invalid


instance FromJSON Functions where
  parseJSON =
    parseFunctions


instance FromJSON Provider where
  parseJSON (Object o) =
    P <$> o .: "name"
    <*> o .: "runtime"
    <*> o .:? "memorySize"
    <*> o .:? "timeout"

  parseJSON invalid =
    typeMismatch "Provider" invalid


instance FromJSON Serverless where
  parseJSON (Object o) =
    S <$> o .: "service"
    <*> o .: "provider"
    <*> o .: "functions"

  parseJSON invalid =
    typeMismatch "Serverless" invalid


parseFunctions :: Value -> Parser Functions
parseFunctions value =
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
    parseFunction :: (Data.Text.Internal.Text, Value) -> Either String (Map.HashMap Data.Text.Internal.Text Function) -> Either String (Map.HashMap Data.Text.Internal.Text Function)
    parseFunction func acc =
      case acc of
        Left _ ->
          acc

        Right dict ->
          let
            k =
              fst func

            body =
              parseEither parseJSON (snd func)
          in
            case body of
              Right v ->
                Right $ Map.insert k v dict

              Left e ->
                Left e


d :: IO (Either ParseException Serverless)
d =
  decodeFileEither "/Users/futtetennista/Developer/scripts/fixtures/serverless.yml"


main :: IO ()
main =
  do
    res <- d
    print res
