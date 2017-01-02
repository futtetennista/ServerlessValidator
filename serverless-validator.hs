#!/usr/bin/env stack
-- stack --resolver lts-7.10 --install-ghc runghc --package yaml

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Serverless.yml reference: https://serverless.com/framework/docs/providers/aws/guide/serverless.yml/

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Traversable (for)
import Data.Aeson.Types (Object, typeMismatch, withObject)
import Data.Yaml (FromJSON, Value (String, Object), Parser, ParseException, (.:), (.:?), (.!=))
import Control.Monad (forM_)
import qualified Data.Yaml as YML (decodeFileEither, parseJSON)
import qualified Data.HashMap.Strict as Map (toList)
import qualified Data.Text as T (unpack, splitOn, pack)
import qualified Data.Text.Lazy.Read as TLR (decimal)
import qualified Data.Text.Lazy as TL (Text, fromStrict, empty)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText, fromText, fromString)
import qualified Data.CaseInsensitive as CI (mk)
import qualified System.Environment as S (getArgs)
import qualified Text.Regex as Regex (mkRegex, matchRegex)
import qualified Data.Maybe as Maybe (fromJust, isJust)


data Serverless
  = S { service :: String
      , frameworkVersion :: FrameworkVersion
      , provider :: Provider
      , functions :: Functions
      }
  deriving Show


instance FromJSON Serverless where
  parseJSON (Object o) =
    S <$> o .: "service"
    <*> o .:? "frameworkVersion" .!= frameworkVersionLatestSupported
    <*> o .: "provider"
    <*> o .: "functions"

  parseJSON invalid =
    typeMismatch "Serverless" invalid


data FrameworkVersion =
  FV { frameworkVersionMin :: SemVer
     , frameworkVersionMax :: SemVer
     }
  deriving Show


instance FromJSON FrameworkVersion where
  parseJSON (String v) =
    let
      -- TODO[1]: is this re-compiled each time? Shall it be moved this to the outer scope?!
      fVRegex =
        Regex.mkRegex "^>=([0-9]+\\.[0-9]+\\.[0-9]+) <([0-9]+\\.[0-9]+\\.[0-9]+)$"
    in
      case Regex.matchRegex fVRegex (T.unpack v) of
        Just [minVer, maxVer] ->
          let
            minSemVer =
              toSemVer $ T.pack minVer

            maxSemVer =
              toSemVer $ T.pack maxVer
          in
            case all Maybe.isJust [minSemVer, maxSemVer] of
              True ->
                return $ FV { frameworkVersionMin = Maybe.fromJust minSemVer
                            , frameworkVersionMax = Maybe.fromJust maxSemVer
                            }

              False ->
                fail "Framework version must be a string of the form: >=x.x.x <x.x.x"

        _ ->
          fail "Framework version must be a string of the form: >=x.x.x <x.x.x"

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

         x' ->
           x'

      x ->
        x


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
        fail err
    where
      toRuntime :: Text -> Either String Runtime
      toRuntime rt =
        case rt of
          "nodejs4.3" ->
            Right NodeJs

          "java8" ->
            Right Java

          "python2.7" ->
            Right Python

          _ ->
            Left $ "Unsupported runtime '" ++ T.unpack rt ++ "'"

  parseJSON invalid =
    typeMismatch "Runtime" invalid


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
  FS { getFunctions :: [Function] }
  deriving Show


instance FromJSON Functions where
  parseJSON value =
    withObject "Functions" parseFunctions value

    where
      parseFunctions :: Object -> Parser Functions
      parseFunctions fObj =
        fmap FS . for (Map.toList fObj) $ \(n, b) -> parseFunction n b


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
                 , kinesisStartingPosition :: String
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


parseStreamEvent :: Value -> Parser Event
parseStreamEvent (String str) =
  return DynamoDBEvent { dynamoDBArn = str }

parseStreamEvent (Object o) =
  KinesisEvent <$> o .: "arn"
  <*> o .: "batchSize"
  <*> o .: "startingPosition"
  <*> o .: "enabled"

parseStreamEvent invalid =
  typeMismatch "Stream event" invalid


parseSnsEvent :: Value -> Parser Event
parseSnsEvent (String str) =
  case validArn Sns str of
    True ->
      return SnsEvent { snsEventTopicName = Nothing
                      , snsEventTopicArn = Just str
                      , snsEventDisplayName = Nothing
                      }

    False ->
      return SnsEvent { snsEventTopicName = Just str
                      , snsEventTopicArn = Nothing
                      , snsEventDisplayName = Nothing
                      }


parseSnsEvent (Object o) =
  SnsEvent <$> o .: "topicName"
  <*> return Nothing
  <*> o .: "displayName"

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


parseS3Event :: Value -> Parser Event
parseS3Event (Object o) =
  S3Event <$> o .: "bucket"
  <*> o .: "event"
  <*> o .:? "rules" .!= []

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


parse :: FileName -> IO (Either ParseException Serverless)
parse f =
  YML.decodeFileEither f


validate :: Serverless -> Either [TL.Text] ()
validate config =
  let
    frameworkVersionValidationRes =
      validateFrameworkVersion $ frameworkVersion config

    s3EventsValidationRes =
      validateS3EventArn $ toS3Events config

    streamEventsValidationRes =
      validateStreamEventArn $ toStreamEvents config

    valid =
      all fst [ frameworkVersionValidationRes
              , s3EventsValidationRes
              , streamEventsValidationRes
              ]
  in
    case valid of
      True ->
        Right ()

      False ->
        Left $ concatMap snd [ frameworkVersionValidationRes
                             , s3EventsValidationRes
                             , streamEventsValidationRes
                             ]

  where
    validateFrameworkVersion :: FrameworkVersion -> (Bool, [TL.Text])
    validateFrameworkVersion fv =
      let
        minRes =
          compare frameworkVersionMinSupported (frameworkVersionMin fv)

        maxRes =
          compare frameworkVersionMaxSupported (frameworkVersionMax fv)
      in
        case minRes of
          LT ->
            (False, [ TLB.toLazyText $ "Minimum version '"
                      <> (TLB.fromString $ show (frameworkVersionMin fv))
                      <> "' is not supported, '"
                      <> (TLB.fromString $ show frameworkVersionMinSupported)
                      <> "' is the minimum supported version (inclusive)"
                    ]
            )

          _ ->
            case maxRes of
              GT ->
                (False, [ TLB.toLazyText $ "Maximum version '"
                          <> (TLB.fromString $ show $ frameworkVersionMax fv)
                          <> "' is not supported, '"
                          <> (TLB.fromString $ show frameworkVersionMaxSupported)
                          <> "' the maximum supported version (exclusive)"
                        ]
                )

              _ ->
                (True, [])


    toStreamEvents :: Serverless -> [Event]
    toStreamEvents serverless =
      concatMap (filterStreamEvents . functionEvents) $ getFunctions (functions serverless)
      where
        filterStreamEvents :: [Event] -> [Event]
        filterStreamEvents =
          filter isStreamEvent

        isStreamEvent :: Event -> Bool
        isStreamEvent (DynamoDBEvent _) =
          True

        isStreamEvent (KinesisEvent _ _ _ _) =
          True

        isStreamEvent _ =
          False


    validateStreamEventArn :: [Event] -> (Bool, [TL.Text])
    validateStreamEventArn streamEvents =
      case filter isLeft (flip map streamEvents validateStreamEventArn') of
        [] ->
          (True, [])

        xs ->
          (False, flip map xs $ either id (\_ -> TL.empty))

      where
        validateStreamEventArn' event =
          case event of
            DynamoDBEvent arn ->
              case validArn DynamoDB arn of
                True ->
                  Right ()

                False ->
                 Left $ TLB.toLazyText ("'" <> TLB.fromText arn  <> "' is not a valid dynamoDB arn")

            KinesisEvent arn _ _ _ ->
              case validArn Kinesis arn of
                True ->
                  Right ()

                False ->
                  Left $ TLB.toLazyText ("'" <> TLB.fromText arn <> "' is not a valid kinesis arn")

            _ ->
              Left "Not a stream event"


    toS3Events :: Serverless -> [Event]
    toS3Events serverless =
        concatMap (filterS3Events . functionEvents) $ getFunctions (functions serverless)
      where
        filterS3Events :: [Event] -> [Event]
        filterS3Events =
          filter isS3Event

        isS3Event :: Event -> Bool
        isS3Event (S3Event _ _ _) =
          True

        isS3Event _ =
          False

    -- http://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html#notification-how-to-event-types-and-destinations
    validateS3EventArn :: [Event] -> (Bool, [TL.Text])
    validateS3EventArn s3Events =
      case filter isLeft (flip map s3Events validateS3EventArn') of
        [] ->
          (True, [])

        xs ->
          (False, flip map xs $ either id (\_ -> TL.empty))

      where
        validateS3EventArn' s3Event =
          case s3Event of
            S3Event _ e _ ->
              case Regex.matchRegex s3EventArnRegex (T.unpack e) of
                Nothing ->
                  Left $ TLB.toLazyText ("'" <> TLB.fromText e <> "' is not a valid s3 event arn")

                Just _ ->
                  Right ()

            _ ->
              Left "Not a S3 Event"

        objCreatedRegex =
          "ObjectCreated:(\\*|Put|Post|Copy|CompleteMultipartUpload)"

        objRemovedRegex =
          "ObjectRemoved:(\\*|Delete|DeleteMarkerCreated)"

        -- see TODO[1]
        s3EventArnRegex =
          Regex.mkRegex $ "s3:(" ++ objCreatedRegex ++ "|" ++ objRemovedRegex ++ "|ReducedRedundancyLostObject)"


    isLeft :: Either a b -> Bool
    isLeft res =
      case res of
        Left _ ->
          True

        Right _ ->
          False


type FileName = String


main :: IO ()
main =
  do
    args <- S.getArgs
    let args = [ "fixtures/serverless.yml"
               , "fixtures/serverless-bogus.yml"
               ]
    case args of
      [] ->
        putStrLn "Usage: ./serverless-validator.hs /path/to/serverless.yml [/path/to/another/serverless.yml]"

      xs ->
        forM_ xs check
  where
    check :: FileName -> IO ()
    check f =
      do
        res <- parse f
        case res of
          Left msg ->
            print msg

          Right serverless ->
            do
              print serverless
              case validate serverless of
                Right _ ->
                  putStrLn $ "The provided file '" ++ f ++ "' is valid"

                Left errs ->
                  do
                    putStrLn $ "Validation of file '" ++ f ++ "' failed:"
                    printErrors errs

    printErrors :: [TL.Text] -> IO ()
    printErrors errs =
      forM_ errs (\msg -> print $ "- " <> msg)
