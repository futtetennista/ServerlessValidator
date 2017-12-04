#!/usr/bin/env stack
{- stack
 --resolver lts-9.14
 --install-ghc runghc
 --package base
 --package protolude
 --package text
 --package aeson
 --package yaml
 --package unordered-containers
 --package case-insensitive
 --package regex-compat
 --
 -hide-all-packages
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- Serverless.yml reference: https://serverless.com/framework/docs/providers/aws/guide/serverless.yml/
module ServerlessValidator ( main
                           , toSemVer
                           , FrameworkVersion(..)
                           , Runtime(..)
                           , Event(..)
                           , AwsService(..)
                           )
where

import GHC.Show (show)
import Protolude hiding (Prefix, show)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Aeson.Types (Object, typeMismatch, withObject)
import Data.Yaml (FromJSON, Value (String, Object), Parser, ParseException, (.:), (.:?), (.!=))
import Control.Monad (forM_, fail)
import qualified Data.Yaml as YML (decodeFileEither, parseJSON)
import qualified Data.HashMap.Strict as Map (toList)
import qualified Data.Text as T (unpack, splitOn, pack)
import qualified Data.Text.Lazy.Read as TLR (decimal)
import qualified Data.Text.Lazy as TL (fromStrict, unpack, toStrict)
import qualified Data.CaseInsensitive as CI (mk)
import qualified System.Environment as S (getArgs)
import qualified Text.Regex as Regex (mkRegex, matchRegex)
import qualified Data.Maybe as Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText, fromString)


type ErrorMsg =
  Text


type ServiceName =
  Text


data Serverless
  = S { service :: ServiceName
      , frameworkVersion :: FrameworkVersion
      , provider :: Provider
      , functions :: Functions
      }
  deriving Show


mkServerless :: ServiceName -> Maybe FrameworkVersion -> Provider -> Functions -> Either ErrorMsg Serverless
mkServerless s fv p fs =
  Right S { service = s
          , frameworkVersion = Maybe.fromMaybe frameworkVersionLatestSupported fv
          , provider = p
          , functions = fs
          }


instance FromJSON Serverless where
  parseJSON (Object o) =
    do
      res <- mkServerless <$> o .: "service"
        <*> o .:? "frameworkVersion"
        <*> o .: "provider"
        <*> o .: "functions"
      either (fail . T.unpack) return res

  parseJSON invalid =
    typeMismatch "Serverless" invalid


data FrameworkVersion =
  FV { frameworkVersionMin :: SemVer
     , frameworkVersionMax :: SemVer
     }
  deriving (Show, Eq)


mkFrameworkVersion :: Text -> Either ErrorMsg FrameworkVersion
mkFrameworkVersion version =
  let
    -- TODO[1]: is this re-compiled each time? Shall it be moved this to the outer scope?!
    fVRegex =
      Regex.mkRegex "^>=([0-9]+\\.[0-9]+\\.[0-9]+) <([0-9]+\\.[0-9]+\\.[0-9]+)$"
  in
    case Regex.matchRegex fVRegex (T.unpack version) of
      Just [minVer, maxVer] ->
        let
          minSemVer =
            toSemVer $ T.pack minVer

          maxSemVer =
            toSemVer $ T.pack maxVer
        in
          if all Maybe.isJust [minSemVer, maxSemVer]
          then validateFrameworkVersionRange (Maybe.fromJust minSemVer) (Maybe.fromJust maxSemVer)
          else Left "Framework version must be a string of the form: >=x.x.x <x.x.x"

      _ ->
        Left "Framework version must be a string of the form: >=x.x.x <x.x.x"


type MinSemVer =
  SemVer

type MaxSemVer =
  SemVer


validateFrameworkVersionRange :: MinSemVer -> MaxSemVer -> Either ErrorMsg FrameworkVersion
validateFrameworkVersionRange minSV maxSV  =
  let
    minOrd =
      compare frameworkVersionMinSupported minSV

    maxOrd =
      compare maxSV frameworkVersionMaxSupported
  in
    case minOrd of
      LT ->
        Left minimumVersionNotSupported

      GT ->
        Left minimumVersionNotSupported

      _ ->
        case maxOrd of
          GT ->
            let
              err =
                TLB.toLazyText $ "Maximum version '" <> TLB.fromString (show maxSV) <> "' is not supported, '" <> TLB.fromString (show frameworkVersionMaxSupported) <> "' the maximum supported version (exclusive)"
            in
              Left . TL.toStrict $ err

          _ ->
            Right FV { frameworkVersionMin = minSV
                     , frameworkVersionMax = maxSV
                     }
  where
    minimumVersionNotSupported =
      TL.toStrict (TLB.toLazyText $ "Minimum version '" <> TLB.fromString (show minSV) <> "' is not supported, '" <> TLB.fromString (show frameworkVersionMinSupported) <> "' is the minimum supported version (inclusive)")


instance FromJSON FrameworkVersion where
  parseJSON (String version) =
    either (fail . T.unpack) return $ mkFrameworkVersion version

  parseJSON invalid =
    typeMismatch "Framework version" invalid


frameworkVersionLatestSupported :: FrameworkVersion
frameworkVersionLatestSupported =
  FV { frameworkVersionMin = frameworkVersionMinSupported
     , frameworkVersionMax = frameworkVersionMaxSupported
     }


data SemVer =
  SemVer { svMajor :: Int
         , svMinor :: Int
         , svPatch :: Int
         }
  deriving Eq


instance Show SemVer where
  show sv =
    show (svMajor sv) ++ "." ++ show (svMinor sv) ++ "." ++ show (svPatch sv)


instance Ord SemVer where
  compare a b =
    case compare (svMajor a) (svMajor b) of
      EQ ->
       case compare (svMinor a) (svMinor b) of
         EQ ->
           compare (svPatch a) (svPatch b)

         ord' ->
           ord'

      ord' ->
        ord'


frameworkVersionMinSupported :: SemVer
frameworkVersionMinSupported =
  Maybe.fromJust $ toSemVer "1.0.0"


frameworkVersionMaxSupported :: SemVer
frameworkVersionMaxSupported =
  Maybe.fromJust $ toSemVer "2.0.0"


toSemVer :: Text -> Maybe SemVer
toSemVer t =
  case map (TLR.decimal . TL.fromStrict) $ T.splitOn "." t of
    [Right (major, _), Right (minor, _), Right (patch, _)] ->
      Just SemVer { svMajor = major
                  , svMinor = minor
                  , svPatch = patch
                  }

    _ ->
      Nothing


data Runtime
  = NodeJs
  | Python
  | Java
  deriving (Show, Eq)


mkRuntime :: Text -> Either ErrorMsg Runtime
mkRuntime "nodejs6.10" =
  Right NodeJs

mkRuntime "nodejs4.3" =
  Right NodeJs

mkRuntime "java8" =
  Right Java

mkRuntime "python2.7" =
  Right Python

mkRuntime unknown =
  Left $ "Unsupported runtime '" <> unknown <> "'. Choose one among: 'nodejs6.10', 'nodejs4.3', 'java8', 'python2.8'"


instance FromJSON Runtime where
  parseJSON (String rt) =
    either (fail . T.unpack) return $ mkRuntime rt

  parseJSON invalid =
    typeMismatch "Runtime" invalid


type Environment =
  (Text, Text)


data Provider
  = P { name :: Text
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
    <*> o .:? "environment" .!= []

  parseJSON invalid =
    typeMismatch "Provider" invalid


newtype Functions =
  FS { getFunctions :: [Function] }
  deriving Show


instance FromJSON Functions where
  parseJSON =
    withObject "Functions" parseFunctions
    where
      parseFunctions :: Object -> Parser Functions
      parseFunctions fObj =
        -- fmap  FS . for (Map.toList fObj) $ \(n, b) -> parseFunction n b
        fmap FS (for (Map.toList fObj) $ uncurry parseFunction)


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


type FunctionName =
  Text


parseFunction :: FunctionName -> Value -> Parser Function
parseFunction fName =
  withObject "Function" parseFunctionBody

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
      <*> obj .:? "environment" .!= []
      <*> obj .: "events"


type Path =
  Text


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
  | ScheduleEvent { scheduleEventRate :: Text
                  , scheduleEventEnabled :: Bool
                  , scheduleEventInput :: ScheduleEventInput
                  , scheduleEventInputPath :: Maybe Text
                  , scheduleEventName :: Maybe Text
                  , scheduleEventDescription :: Maybe Text
                  }
  | SnsEvent { snsEventTopicName :: Maybe Text
             , snsEventTopicArn :: Maybe Text
             , snsEventDisplayName :: Maybe Text
             }
  | DynamoDBEvent { dynamoDBArn :: Text
                  }
  | KinesisEvent { kinesisArn :: Text
                 , kinesisBatchSize :: Int
                 , kinesisStartingPosition :: Text
                 , kinesisEnabled :: Bool
                 }
  | UnknownEvent Text
  deriving Show


data ScheduleEventInput
  = SEI { key1 :: Text
        , key2 :: Text
        , stageParams :: ScheduleEventInputStageParams
        }
  | EmptySEI
  deriving Show


instance FromJSON ScheduleEventInput where
  parseJSON (Object o) =
    SEI <$> o .: "key1"
    <*> o .: "key2"
    <*> o .: "stageParams"

  parseJSON invalid =
    typeMismatch "Schedule Event Input" invalid


data ScheduleEventInputStageParams =
  SEISP { stage :: Text }
  deriving Show


instance FromJSON ScheduleEventInputStageParams where
  parseJSON (String str) =
    return SEISP { stage = str }

  parseJSON invalid =
    typeMismatch "Schedule Event Stage" invalid


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


type S3EventRuleValue =
  Text


type S3EventRuleKey =
  Text


mkS3EventRule :: S3EventRuleKey -> S3EventRuleValue -> Either ErrorMsg S3EventRule
mkS3EventRule "suffix" suffix =
  Right $ Suffix suffix

mkS3EventRule "prefix" prefix =
  Right $ Prefix prefix

mkS3EventRule _ _ =
  Left "'prefix' or 'suffix' string"


instance FromJSON S3EventRule where
  parseJSON value =
    withObject "S3 Event Rule" (unpackRule . Map.toList) value

    where
      unpackRule :: [(Text, Value)] -> Parser S3EventRule
      unpackRule [(k, String v)] =
        either (fail . T.unpack) return $ mkS3EventRule k v

      unpackRule _ =
        typeMismatch "Invalid S3 Event Rule" value


instance FromJSON Event where
  parseJSON value =
    withObject "Event" (parseEvent . Map.toList)  value

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

              "schedule" ->
                parseScheduleEvent eventConfig

              "sns" ->
                parseSnsEvent eventConfig

              "stream" ->
                parseStreamEvent eventConfig

              _ ->
                return $ UnknownEvent eventName

          _ ->
            typeMismatch "Event" value


data AwsService
  = Sns
  | DynamoDB
  | Kinesis
  deriving (Show, Eq)


type Arn =
  Text


validArn :: AwsService -> Arn -> Bool
validArn awsSer arn =
  Maybe.isJust $ Regex.matchRegex arnRegex (T.unpack arn)
  where
    arnRegex =
      let
        -- see TODO[1]
        snsRegex =
          Regex.mkRegex "arn:aws:sns:(\\*|[a-z]{2}-[a-z]+-[0-9]+):[0-9]{12}:.+"

        dynamoDBRegex =
          Regex.mkRegex "arn:aws:dynamodb:(\\*|[a-z]{2}-[a-z]+-[0-9]+):[0-9]{12}:table/.+"

        kinesisRegex =
          Regex.mkRegex "arn:aws:kinesis:(\\*|[a-z]{2}-[a-z]+-[0-9]+):[0-9]{12}:stream/.+"
      in
        case awsSer of
          -- http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-sns
          Sns ->
            snsRegex

          DynamoDB ->
            dynamoDBRegex

          Kinesis ->
            kinesisRegex


mkDynamoDBEvent :: Arn -> Either Text Event
mkDynamoDBEvent arn
  | not . validArn DynamoDB $ arn =
      Left $ "'" <> arn <> "' is not a valid " <> T.pack (show DynamoDB) <> " arn"

mkDynamoDBEvent arn =
  Right DynamoDBEvent { dynamoDBArn = arn }


mkKinesisEvent :: Arn -> Int -> Text -> Bool -> Either Text Event
mkKinesisEvent arn _ _ _
  | not . validArn Kinesis $ arn =
      Left $ "'" <> arn <> "' is not a valid " <> T.pack (show Kinesis) <> " arn"

mkKinesisEvent arn batchSize startingPosition enabled =
  Right KinesisEvent { kinesisArn = arn
                     , kinesisBatchSize = batchSize
                     , kinesisStartingPosition = startingPosition
                     , kinesisEnabled = enabled
                     }


parseStreamEvent :: Value -> Parser Event
parseStreamEvent (String arn) =
  either (fail . T.unpack) return $ mkDynamoDBEvent arn

parseStreamEvent (Object o) =
  do
    res <- mkKinesisEvent <$> o .: "arn"
      <*> o .: "batchSize"
      <*> o .: "startingPosition"
      <*> o .: "enabled"
    either (fail . T.unpack) return res


parseStreamEvent invalid =
  typeMismatch "Stream event" invalid


mkSnsEvent :: Arn -> Maybe Text -> Event
mkSnsEvent arn _ | validArn Sns arn =
                     SnsEvent { snsEventTopicName = Nothing
                              , snsEventTopicArn = Just arn
                              , snsEventDisplayName = Nothing
                              }

mkSnsEvent topicName Nothing | not . validArn Sns $ topicName =
                               SnsEvent { snsEventTopicName = Just topicName
                                        , snsEventTopicArn = Nothing
                                        , snsEventDisplayName = Nothing
                                        }

mkSnsEvent topicName displayName =
  SnsEvent { snsEventTopicName = Just topicName
           , snsEventTopicArn = Nothing
           , snsEventDisplayName = displayName
           }


parseSnsEvent :: Value -> Parser Event
parseSnsEvent (String arn) =
   return $ mkSnsEvent arn Nothing

parseSnsEvent (Object o) =
  mkSnsEvent <$> o .: "topicName" <*> o .:? "displayName"

parseSnsEvent invalid =
  typeMismatch "Sns Event" invalid


parseScheduleEvent :: Value -> Parser Event
parseScheduleEvent (Object o) =
  ScheduleEvent <$> o .: "rate"
  <*> o .: "enabled"
  <*> o .:? "input" .!= EmptySEI
  <*> o .:? "inputPath"
  <*> o .:? "name"
  <*> o .:? "description"

parseScheduleEvent invalid =
  typeMismatch "Schedule Event" invalid


type S3Bucket =
  Text


type S3Event =
  Text


mkS3Event :: S3Bucket -> S3Event -> [S3EventRule] -> Either ErrorMsg Event
mkS3Event _ event _ | invalidArn =
                      Left $ "'" <> event <> "' is not a valid s3 event arn"
  where
    invalidArn =
      not . isValidS3EventArn $ event

mkS3Event bucket event rules =
  Right S3Event { s3EventBucket = bucket
                , s3EventEvent = event
                , s3EventRules = rules
                }

parseS3Event :: Value -> Parser Event
parseS3Event (Object o) =
  do
    res <- mkS3Event <$> o .: "bucket" <*> o .: "event" <*> o .:? "rules" .!= []
    either (fail . T.unpack) return res

parseS3Event invalid =
  typeMismatch "S3 Event" invalid


type S3EventArn =
  Text


-- http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html#notification-how-to-event-types-and-destinations
isValidS3EventArn :: S3EventArn -> Bool
isValidS3EventArn event =
  Maybe.isJust $ Regex.matchRegex s3EventArnRegex (T.unpack event)
  where
    -- see TODO[1]
    s3EventArnRegex =
      let
        objCreatedRegex =
          "ObjectCreated:(\\*|Put|Post|Copy|CompleteMultipartUpload)"

        objRemovedRegex =
          "ObjectRemoved:(\\*|Delete|DeleteMarkerCreated)"

        regexBuilder =
          TLB.toLazyText $ "s3:(" <> objCreatedRegex <> "|" <> objRemovedRegex <> "|ReducedRedundancyLostObject)"
      in
        Regex.mkRegex . TL.unpack $ regexBuilder


mkHttpEvent :: Path -> HttpMethod -> Maybe Bool -> Maybe Bool -> Event
mkHttpEvent path method cors private =
  HttpEvent { httpEventPath = path
            , httpEventMethod = method
            , httpEventCors = cors
            , httpEventPrivate = private
            }


type HttpEventConfig =
  Text


mkHttpEventFromString :: HttpEventConfig -> Either ErrorMsg Event
mkHttpEventFromString config =
  case T.splitOn " " config of
    [httpMethodStr, httpEndpoint] ->
      case toHttpMethod httpMethodStr of
        Right m ->
          Right $ mkHttpEvent httpEndpoint m Nothing Nothing

        Left err ->
          Left $ "Unknown HTTP method: " <> err

    _ ->
      Left "HTTP string must contain only a HTTP method and a path, i.e. 'http: GET foo'"



parseHttpEvent :: Value -> Parser Event
parseHttpEvent (String config) =
  either (fail . T.unpack) return $ mkHttpEventFromString config

parseHttpEvent (Object obj) =
  mkHttpEvent <$> obj .: "path" <*> obj .: "method" <*> obj .:? "cors" <*> obj .:? "private"

parseHttpEvent invalid =
  typeMismatch "HTTP Event" invalid


toHttpMethod :: Text -> Either ErrorMsg HttpMethod
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
      Left $ T.pack (show str)


instance FromJSON HttpMethod where
  parseJSON (String str) =
    case toHttpMethod str of
      Right m ->
        return m

      Left err ->
        fail $ T.unpack("Unknown HTTP method: " <> err)

  parseJSON invalid =
    typeMismatch "HttpMethod" invalid


parse :: FilePath -> IO (Either ParseException Serverless)
parse =
  YML.decodeFileEither


main :: IO ()
main =
  do
    args <- S.getArgs
    -- let args = [ "fixtures/serverless.yml"
    --            , "fixtures/serverless-bogus.yml"
    --            ]
    case args of
      [] ->
        print $ T.pack "Usage: ./serverless-validator /path/to/serverless.yml [/path/to/another/serverless.yml]"

      xs ->
        forM_ xs checkFile
  where
    checkFile :: FilePath -> IO ()
    checkFile f =
      do
        res <- parse f
        case res of
          Left err ->
            do
              putStrLn $ "'" ++ f ++ "' is not valid"
              print err

          Right serverless ->
            do
              print serverless
              putStrLn $ "'" ++ f ++ "' is valid"
