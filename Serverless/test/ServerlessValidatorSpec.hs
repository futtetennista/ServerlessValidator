module ServerlessValidatorSpec (spec)
where

import Test.Hspec
import Protolude
import GHC.Base (String)
import Data.Aeson (eitherDecode)
import qualified ServerlessValidator as S
import qualified Data.Maybe as Maybe (fromJust)


spec :: Spec
spec =
  do
    describe "Serverless Validator" $ do
      it "validates framework version" testFrameworkVersionValidation

      it "validates λ function runtimes" testRuntimeValidation

      it "validates S3 events arn" testS3EventsValidation

      it "validates stream events" $ testStreamEventsValidation

      it "validates SNS events" $ testSnsEventsValidation


testSnsEventsValidation :: Expectation
testSnsEventsValidation =
  do
    decodeString "arn:aws:sns:*:123456789012:my_corporate_topic" `shouldSatisfy` isRight
    decodeString "topic-name" `shouldSatisfy` isRight
    where
      decodeString arn =
        eitherDecode ("{\"sns\":\"" <> arn <> "\"}") :: Either String S.Event


testStreamEventsValidation :: Expectation
testStreamEventsValidation =
  do
    decodeKinesis "arn:aws:kinesis:us-east-1:123456789012:stream/example-stream-name" `shouldSatisfy` (\res -> isRight res && isKinesisEvent res)
    decodeKinesis "foobar" `shouldSatisfy` isLeft

    decodeDynamoDB "arn:aws:dynamodb:us-east-1:123456789012:table/books_table" `shouldSatisfy` (\res -> isRight res && isDynamoDBEvent res)
    decodeDynamoDB "foobar" `shouldSatisfy` isLeft
      where
        isDynamoDBEvent (Right (S.DynamoDBEvent _)) =
          True

        isDynamoDBEvent _ =
          False


        isKinesisEvent (Right (S.KinesisEvent _ _ _ _)) =
          True

        isKinesisEvent _ =
          False


        decodeDynamoDB arn =
          eitherDecode ("{\"stream\":\"" <> arn <> "\"}") :: Either String S.Event


        decodeKinesis arn =
          eitherDecode ("{\"stream\":{\"arn\":\"" <> arn <> "\",\"batchSize\":100,\"startingPosition\":\"LATEST\",\"enabled\":false}}") :: Either String S.Event


testRuntimeValidation :: Expectation
testRuntimeValidation =
  do
    decode "java8" `shouldBe` Right S.Java
    decode "python2.7" `shouldBe` Right S.Python
    decode "nodejs4.3" `shouldBe` Right S.NodeJs
    decode "foobar" `shouldSatisfy` isLeft
      where
        decode rt =
          eitherDecode ("\"" <> rt <> "\"") :: Either String S.Runtime


testS3EventsValidation :: Expectation
testS3EventsValidation =
  do
    decode "foobar" `shouldSatisfy` isLeft
    decode "s3:ObjectRemoved:*" `shouldSatisfy` isRight
    decode "s3:ObjectCreated:Put" `shouldSatisfy` isRight
    decode "s3:ReducedRedundancyLostObject" `shouldSatisfy` isRight
      where
        decode event =
          eitherDecode ("{\"s3\":{\"bucket\":\"test\",\"event\":\""<> event <> "\"}}") :: Either String S.Event


testFrameworkVersionValidation :: Expectation
testFrameworkVersionValidation =
  do
    decode "foobar" `shouldSatisfy` isLeft
    decode ">=0.0.1 <2.0.0" `shouldSatisfy` isLeft
    decode ">=2.0.0 <2.1.0" `shouldSatisfy` isLeft
    decode ">=2.0.0 <1.0.0" `shouldSatisfy` isLeft
    decode ">=1.0.0 <2.0.0" `shouldBe` fv
      where
        decode fv =
          eitherDecode ("\"" <> fv <> "\"") :: Either String S.FrameworkVersion

        fv =
          Right S.FV { S.frameworkVersionMin = Maybe.fromJust . S.toSemVer $ "1.0.0"
                     , S.frameworkVersionMax = Maybe.fromJust . S.toSemVer $ "2.0.0"
                     }
