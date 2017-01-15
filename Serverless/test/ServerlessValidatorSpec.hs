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

      it "validates runtimes" testRuntimeValidation

      it "creates serveless with latest supported version if no version is supplied" $ do
        pending

      it "validates S3 event arn" testS3EventValidation

      it "validates AWS arn" $ do
        pending

      it "validates Kinesis events" $ do
        pending

      it "validates DynamoDB events" $ do
        pending

      it "validates SNS events" $ do
        pending


testRuntimeValidation =
  do
    decode "java8" `shouldBe` Right S.Java
    decode "python2.7" `shouldBe` Right S.Python
    decode "nodejs4.3" `shouldBe` Right S.NodeJs
    decode "foobar" `shouldSatisfy` isLeft
      where
        decode rt =
          eitherDecode ("\"" <> rt <> "\"") :: Either String S.Runtime


testS3EventValidation =
  do
    decode "foobar" `shouldSatisfy` isLeft
    decode "s3:ObjectRemoved:*" `shouldSatisfy` isRight
    decode "s3:ObjectCreated:Put" `shouldSatisfy` isRight
    decode "s3:ReducedRedundancyLostObject" `shouldSatisfy` isRight
      where
        decode event =
          eitherDecode ("{\"s3\":{\"bucket\":\"test\",\"event\":\""<> event <> "\"}}") :: Either String S.Event


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
