#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Serverless.yml reference: https://serverless.com/framework/docs/providers/aws/guide/serverless.yml/

{-|

Things learned:

- JSON parsing with dynamic JSON objs
- Modelling data structs from JSON objs
- String handling
- Regex libs

TODOs:

- Testing: https://www.reddit.com/r/haskell/comments/5mrk20/haskell_testing_frameworks_what_do_you_use/
  - Quickcheck
  - https://hackage.haskell.org/package/HUnit
  - https://hspec.github.io/
- create stack project
- run script using Docker : probably need a shell script to first copy the files in the container and then run the validation

-}

module ServerlessValidator ( main
                           , toSemVer
                           , FrameworkVersion(..)
                           , Runtime(..)
                           , Event(..)
                           )
where

import GHC.Base (id)
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
import qualified Data.Maybe as Maybe (fromJust, isJust, maybe)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText, fromString)


data Serverless
  = S { service :: Text
      , frameworkVersion :: FrameworkVersion
      , provider :: Provider
      , functions :: Functions
      }
  deriving Show


mkServerless :: Text -> Maybe FrameworkVersion -> Provider -> Functions -> Either Text Serverless
mkServerless s fv p fs =
  Right $ S { service = s
            , frameworkVersion = Maybe.maybe frameworkVersionLatestSupported id fv
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


mkFrameworkVersion :: Text -> Either Text FrameworkVersion
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
          case all Maybe.isJust [minSemVer, maxSemVer] of
            True ->
              validateFrameworkVersionRange (Maybe.fromJust minSemVer) (Maybe.fromJust maxSemVer)

            False ->
              Left "Framework version must be a string of the form: >=x.x.x <x.x.x"

      _ ->
        Left "Framework version must be a string of the form: >=x.x.x <x.x.x"


validateFrameworkVersionRange :: SemVer -> SemVer -> Either Text FrameworkVersion
validateFrameworkVersionRange semVerMin semVerMax  =
  let
    minRes =
      compare frameworkVersionMinSupported semVerMin

    maxRes =
      compare semVerMax frameworkVersionMaxSupported
  in
    case minRes of
      LT ->
        Left minimumVersionNotSupported

      GT ->
        Left minimumVersionNotSupported

      _ ->
        case maxRes of
          GT ->
            let
              err =
                TLB.toLazyText $ "Maximum version '" <> (TLB.fromString $ show semVerMax) <> "' is not supported, '" <> (TLB.fromString $ show frameworkVersionMaxSupported) <> "' the maximum supported version (exclusive)"
            in
              Left . TL.toStrict $ err

          _ ->
            Right FV { frameworkVersionMin = semVerMin
                     , frameworkVersionMax = semVerMax
                     }
  where
    minimumVersionNotSupported =
      TL.toStrict (TLB.toLazyText $ "Minimum version '" <> (TLB.fromString $ show semVerMin) <> "' is not supported, '" <> (TLB.fromString $ show frameworkVersionMinSupported) <> "' is the minimum supported version (inclusive)")


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
      Just $ SemVer { svMajor = major
                    , svMinor = minor
                    , svPatch = patch
                    }

    _ ->
      Nothing


data Runtime
  = NodeJs
  | Python
  | Java
  deriving Show


instance FromJSON Runtime where
  parseJSON (String r) =
    case toRuntime r of
      Right rt ->
        return rt

      Left err ->
        fail $ T.unpack err
    where
      toRuntime :: Text -> Either Text Runtime
      toRuntime rt =
        case rt of
          "nodejs4.3" ->
            Right NodeJs

          "java8" ->
            Right Java

          "python2.7" ->
            Right Python

          _ ->
            Left $ "Unsupported runtime '" <> rt <> "'"

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
  parseJSON value =
    withObject "Functions" parseFunctions value

    where
      parseFunctions :: Object -> Parser Functions
      parseFunctions fObj =
        -- fmap  FS . for (Map.toList fObj) $ \(n, b) -> parseFunction n b
        fmap FS (for (Map.toList fObj) $ \(n, b) -> parseFunction n b)


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
      <*> obj .:? "environment" .!= []
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
    return $ SEISP { stage = str }

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
  deriving Show


validArn :: AwsService -> Text -> Bool
validArn awsSer t =
  Maybe.isJust $ Regex.matchRegex arnRegex (T.unpack t)
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


mkDynamoDBEvent :: Text -> Either Text Event
mkDynamoDBEvent arn | not . (validArn DynamoDB) $ arn =
                      Left $ "'" <> arn <> "' is not a valid " <> T.pack(show DynamoDB) <> " arn"

mkDynamoDBEvent arn =
  Right $ DynamoDBEvent { dynamoDBArn = arn }


mkKinesisEvent :: Text -> Int -> Text -> Bool -> Either Text Event
mkKinesisEvent arn _ _ _ | not . (validArn Kinesis) $ arn =
                           Left $ "'" <> arn <> "' is not a valid " <> T.pack(show Kinesis) <> " arn"

mkKinesisEvent arn batchSize startingPosition enabled =
  Right $ KinesisEvent { kinesisArn = arn
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


mkSnsEvent :: Text -> Maybe Text -> Event
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


mkS3Event :: Text -> Text -> [S3EventRule] -> Either Text Event
mkS3Event _ event _ | invalidArn =
                      Left $ "'" <> event <> "' is not a valid s3 event arn"
  where
    invalidArn =
      not . isValidS3EventArn $ event

mkS3Event bucket event rules =
  Right $ S3Event { s3EventBucket = bucket
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


-- http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html#notification-how-to-event-types-and-destinations
isValidS3EventArn :: Text -> Bool
isValidS3EventArn event =
  Maybe.maybe False (\_ -> True) $ Regex.matchRegex s3EventArnRegex (T.unpack event)
  where
    objCreatedRegex =
      "ObjectCreated:(\\*|Put|Post|Copy|CompleteMultipartUpload)"

    objRemovedRegex =
      "ObjectRemoved:(\\*|Delete|DeleteMarkerCreated)"

    -- see TODO[1]
    s3EventArnRegex =
      let
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


mkHttpEventFromString :: Text -> Either Text Event
mkHttpEventFromString config =
  case (T.splitOn " " config) of
    [httpMethodStr, httpEndpoint] ->
      case toHttpMethod $ httpMethodStr of
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


toHttpMethod :: Text -> Either Text HttpMethod
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
parse f =
  YML.decodeFileEither f


main :: IO ()
main =
  do
    args <- S.getArgs
    -- let args = [ "fixtures/serverless.yml"
    --            , "fixtures/serverless-bogus.yml"
    --            ]
    case args of
      [] ->
        print $ T.pack "Usage: ./serverless-validator.hs /path/to/serverless.yml [/path/to/another/serverless.yml]"

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
