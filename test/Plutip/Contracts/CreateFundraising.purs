module Test.Plutip.Contracts.CreateFundraising where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip (withWallets)
import Contract.Wallet (withKeyWallet, KeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Fundraising.Create as Create
import Fundraising.UserData (CreateFundraisingParams(..), FundraisingData, mapFundraisingInfoToData)
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Protocol.UserData (ProtocolData)
import Shared.Duration (Duration(..))
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Contracts.UpdateProtocol (incorrectProtocol)
import Test.Plutip.Fixtures (aliceBobDistribution, aliceDistribution)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Create Fundraising" do

    test "Should successfully create a new project" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        withKeyWallet bob $ void $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 6))

    test "Should fail if Protocol doesn't exist" do
      withWallets aliceDistribution \alice -> do
        protocol <- incorrectProtocol
        result <- try $ withKeyWallet alice $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 6))
        let errMsg = "Protocol UTxO with given nft not found"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if the project goal is less than the minimal acceptable amount" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        result <- try $ withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 1 (mkFundraisingDuration 0 0 6))
        let errMsg = "Fundraising amount too small. It must be greater than 2000000."
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if the project goal is bigger than the maximal acceptable amount" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        result <- try $ withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 200 (mkFundraisingDuration 0 0 6))
        let errMsg = "Fundraising amount too big. It must be less than 100000000."
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if the project duration is shorter than the minimal acceptable duration" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        result <- try $ withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 0 0 1))
        let errMsg = "Fundraising duration too short. It must be greater than 5."
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if the project duration is longer than the maximal acceptable duration" do
      withWallets aliceBobDistribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        result <- try $ withKeyWallet bob $ Create.contract protocol (mkFundraisingParams 80 (mkFundraisingDuration 300 0 0))
        let errMsg = "Fundraising duration too long. It must be less than 250."
        result `shouldSatisfy` (isExpectedError errMsg)

mkFundraisingParams :: Int -> Duration -> CreateFundraisingParams
mkFundraisingParams amt dur = CreateFundraisingParams
  { title: "Donate to feed stray cats"
  , amount: amt
  , duration: dur
  }

mkFundraisingDuration :: Int -> Int -> Int -> Duration
mkFundraisingDuration d h m = Duration { days: d, hours: h, minutes: m }

createTestFundraising :: KeyWallet -> ProtocolData -> CreateFundraisingParams -> Contract FundraisingData
createTestFundraising wallet protocolData frParams =
  withKeyWallet wallet $ mapFundraisingInfoToData <$> Create.contract protocolData frParams
