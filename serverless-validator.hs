#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import Data.Yaml
import Data.Aeson (genericParseJSON, defaultOptions)
import Data.Aeson.Types (parseMaybe, typeMismatch, omitNothingFields)
import GHC.Generics
import qualified Data.HashMap.Strict as Map (HashMap, insert, toList, empty)
import qualified Data.Text as T (unpack)


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
      --, deployedFunctionName :: Maybe String
      }
  deriving (Show)

instance FromJSON Functions where
  parseJSON = functionsParser

instance FromJSON Provider where
  parseJSON (Object o) =
    P <$> o .: "name"
    <*> o .: "runtime"
    <*> o .:? "memorySize"
    <*> o .:? "timeout"

  parseJSON invalid = typeMismatch "Provide" invalid

instance FromJSON Serverless where
  parseJSON (Object o) =
    S <$> o .: "service"
    <*> o .: "provider"
    <*> o .: "functions"

  parseJSON invalid = typeMismatch "Serverless" invalid

functionsParser :: Value -> Parser Functions
functionsParser obj =
  do
    case obj of
      Object o ->
        let
          fs =
            Map.empty

          function =
            head $ Map.toList o

          k =
            fst function

          functionDefinition =
            parseEither parseF (snd function)
        in
          case functionDefinition of
            Right v ->
                return $ FS (Map.insert (T.unpack k) v fs)

            Left _ ->
                typeMismatch "Function" $ snd function

      _ ->
        typeMismatch "Functions" obj

      where
        -- This can be instance FromJSON Function where …
        parseF :: Value -> Parser Function
        parseF = \f ->
          case f of
            Object o ->
              return F <*> o .: "handler"

            _ ->
              typeMismatch "Function" f

d :: IO (Either ParseException Serverless)
d =
  decodeFileEither "sample.yml"

main :: IO ()
main =
  do
    res <- d
    putStrLn $ show res
