#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Yaml
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as Map (HashMap, insert, toList, empty)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Internal (Text)


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
  = FS { fs :: Map.HashMap String Function }
  deriving Show


data Function
  = F { handler :: String
      , deployedFunctionName :: Maybe String
      }
  deriving (Show)


data Events
  = E { http :: Maybe [HttpEvent]
      , streams :: Maybe [String]
      , s3 :: Maybe [String]
      , schedule :: Maybe [String]
      , sns :: Maybe [String]
      }
  deriving Show


type Method = String
type Path = String


data HttpEvent
  = HTTPEvent { path :: String
              , method :: HttpMethod
              }
  | Description Method Path
  deriving Show


data HttpMethod
  = Get
  | Post
  | Put
  | Delete
  | Patch
  deriving Show


instance FromJSON Function where
  parseJSON (Object o) =
    F <$> o .: "handler"
    <*> o .:? "deployedFunctionName"

  parseJSON invalid =
    typeMismatch "Function" invalid


instance FromJSON Functions where
  parseJSON =
    functionsParser


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


functionsParser :: Value -> Parser Functions
functionsParser value =
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
    parseFunction :: (Data.Text.Internal.Text, Value) -> Either String (Map.HashMap String Function) -> Either String (Map.HashMap String Function)
    parseFunction func acc =
      case acc of
        Left _ ->
          acc

        Right dict ->
          let
            k =
              T.unpack $ fst func

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
