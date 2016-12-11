#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-
- write posts about parsing dynamic json/yaml


I'm trying to parse the following JSON:

"foo" {
  "bars" {
    "bar1": {
      "a": 123,
      "b": "red",
      …
    },
    "bar2": {
      "a": 345,
      "b": "blue",
      …
    }
    …
  }
}

One thing to note is that  the internal structure of the objects "baz" and "bar" is known, so I can write a data type for them,
but their name is not and there. In my current working implementation I get the "bars" aeson object, get its hashmap representation
and then use foldr to return a hashmap where the keys are the object names and the values their bodies. The thing that bugs me
in this implementation is that, in order to make my parser fail if something is wrong in the "bars" object, I have to use a Either
type while folding and this makes it a bit ugly. So this is how the FromJSON instance looks like for the type Bars:

```
instance FromJSON Bars where
  parseJSON value =
    case value of
      Object o ->
        case (foldr parseBars (Right HashMap.empty) $ HashMap.toList o) of
          Right bars ->
            return $ Bars bars

          Left e ->
            fail $ show e

      _ ->
        typeMismatch "Bars" value

  where
    parseBar :: (Text, Value) -> Either String (HashMap Text Bar) -> Either String (Map.HashMap Text Bar)
    …
```

Ideally I'd like to have a function that applies the Bar parser N times and fails if parsing any of the objects fails. So something like:

```
many1 $ parseJSON bar
```

but I don't see how I can achieve that since the content inside "bars" is an aeson Object - that is a hash map.


Can anyone suggest any idea to a better solution to this problem?



Idea 1:
Achieve something like:
```many1 $ YML.parseJSON funcs```

Issue: funcs have type [(Text, Value)], that is N values, but parseJSON requires ONE Value type
1. fold all values in one value => didn't understand how to do it, dunno if it's possible

-}

-- Serverless.yml reference: https://serverless.com/framework/docs/providers/aws/guide/serverless.yml/

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


import Data.Text.Internal (Text)
import Data.Traversable (for)
import Control.Monad (forM_)
import Data.Aeson.Types (Object, typeMismatch, withObject)
import Data.Yaml (FromJSON, Value (String, Object), Parser, ParseException, (.:), (.:?), (.!=))
import qualified Data.Yaml as YML (parseEither, decodeFileEither, parseJSON)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.HashMap.Strict as Map (HashMap, insert, toList, empty)
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
      -- , globalRuntime :: Runtime
      -- , globalMemorySize :: Maybe Int
      -- , globalTimeout :: Maybe Int
      -- , globalEnvironment :: [Environment]
      }
  deriving Show


instance FromJSON Provider where
  parseJSON (Object o) =
    P <$> o .: "name"
    -- <*> o .: "runtime"
    -- <*> o .:? "memorySize"
    -- <*> o .:? "timeout"
    -- <*> o .:? "environment"  .!= emptyEnvironment

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
  F { handler :: String
    , deployedName :: Maybe String
    , description :: Maybe String
    , runtime :: Maybe Runtime
    , memorySize :: Maybe Int
    , timeout :: Maybe Int
    , environment :: [Environment]
    , events :: [Event]
    }
  deriving Show


parseFunction :: Text -> Value -> Parser Function
parseFunction fName fBody =
  YML.parseJSON fBody


instance FromJSON Function where
  parseJSON (Object o) =
    F <$> o .: "handler"
    <*> o .:? "deployedName"
    <*> o .:? "description"
    <*> o .:? "runtime"
    <*> o .:? "memorySize"
    <*> o .:? "timeout"
    <*> o .:? "environment" .!= emptyEnvironment
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


instance FromJSON HttpMethod where
  parseJSON (String s) =
    case toHttpMethod s of
      Just httpMethod ->
        return httpMethod

      Nothing ->
        fail "HTTP method must be a string"

  parseJSON invalid =
    typeMismatch "HttpMethod" invalid


instance FromJSON Event where
  parseJSON (Object o) =
    E <$> o .:? "http"
    -- <*> o .:? "streams"
    -- <*> o .:? "s3"
    -- <*> o .:? "schedule"
    -- <*> o .:? "sns"

  parseJSON invalid =
    typeMismatch "Event" invalid


d :: String -> IO (Either ParseException Serverless)
d serverlessPath =
  YML.decodeFileEither serverlessPath


main :: IO ()
main =
  do
    -- args <- S.getArgs
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
        res <- d f
        case res of
          Left msg ->
            print msg

          Right serverless ->
            do
              print serverless
              putStrLn $ "The provided file '" ++ f ++ "' is valid"
