module ServerlessValidatorSpec (spec)
where

import Test.Hspec
import Protolude
import qualified ServerlessValidator as S
import qualified Data.Maybe as Maybe (fromJust)

spec :: Spec
spec =
  do
    describe "Serverless Validator" $ do
      it "validates S3 event arn" testS3EventValidation

      it "validates framework version" testFrameworkVersionValidation

      it "validates AWS arn" $ do
        pending

      it "validates Kinesis events" $ do
        pending

      it "validates DynamoDB events" $ do
        pending

      it "validates SNS events" $ do
        pending

      it "validates SNS events" $ do
        pending

      it "validates runtimes" $ do
        pending

      it "creates serveless with latest supported version if no version is supplied" $ do
        pending


testS3EventValidation =
  do
    S.isValidS3EventArn "foobar" `shouldBe` False
    S.isValidS3EventArn "s3:ObjectRemoved:*" `shouldBe` True
    S.isValidS3EventArn "s3:ObjectCreated:Put" `shouldBe` True
    S.isValidS3EventArn "s3:ReducedRedundancyLostObject" `shouldBe` True


testFrameworkVersionValidation =
  do
    S.mkFrameworkVersion "foobar" `shouldBe` Left "Framework version must be a string of the form: >=x.x.x <x.x.x"
    S.mkFrameworkVersion ">=0.0.1 <2.0.0" `shouldBe` Left "Minimum version '0.0.1' is not supported, '1.0.0' is the minimum supported version (inclusive)"
    S.mkFrameworkVersion ">=2.0.0 <2.1.0" `shouldBe` Left "Minimum version '2.0.0' is not supported, '1.0.0' is the minimum supported version (inclusive)"
    S.mkFrameworkVersion ">=2.0.0 <1.0.0" `shouldBe` Left "Minimum version '2.0.0' is not supported, '1.0.0' is the minimum supported version (inclusive)"
    S.mkFrameworkVersion ">=1.0.0 <2.0.0" `shouldBe` fv
      where
        fv =
          Right S.FV { S.frameworkVersionMin = Maybe.fromJust . S.toSemVer $ "1.0.0"
                     , S.frameworkVersionMax = Maybe.fromJust . S.toSemVer $ "2.0.0"
                     }
