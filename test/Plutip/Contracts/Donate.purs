module Test.Plutip.Contracts.Donate where

import Contract.Prelude

import Contract.Test.Plutip (withWallets)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(Milliseconds), delay)
import Effect.Aff.Class (liftAff)
import Fundraising.Donate as Donate
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Test.Plutip.Contracts.CreateFundraising (mkFundraisingDuration, mkFundraisingParams, createTestFundraising)
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Fixtures (aliceBobDistribution, incorrectFundraisingData, minDurationStartProtocolParams)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Donate" do

    test "Should successfully donate" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 100 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 20

    test "Should successfully donate more than fundraising goal" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 100

    test "Should successfully donate by fundraising creator" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet bob $ void $ Donate.contract protocolData frData 50

    test "Should fail if fundraising does not exist" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        frData <- incorrectFundraisingData
        result <- try $ withKeyWallet bob $ void $ Donate.contract protocolData frData 50
        let errMsg = "Fundraising UTxO with given nft not found"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if fundraising goal is already reached" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 80
        result <- try $ withKeyWallet alice $ void $ Donate.contract protocolData frData 20
        let errMsg = "fundraising goal is already completed"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if fundraising time is over" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem minDurationStartProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 1))
        liftAff $ delay $ Milliseconds 60000.0 -- 1 min
        result <- try $ withKeyWallet alice $ void $ Donate.contract protocolData frData 20
        let errMsg = "fundraising time is over"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should successfully donate on 1 minute duration fundraising" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.startSystem minDurationStartProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 1))
        withKeyWallet bob $ void $ Donate.contract protocolData frData 50

