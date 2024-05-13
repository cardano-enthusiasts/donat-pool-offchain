module Test.Plutip.Contracts.CloseProtocol where

import Prelude

import Contract.Test.Plutip (withWallets)
import Contract.Wallet (withKeyWallet)
-- import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
-- import Data.Tuple.Nested ((/\))
import Mote (group, test)
import Protocol.CloseProtocol as CloseProtocol
import Protocol.StartProtocol as StartProtocol
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
-- import Test.Plutip.Contracts.UpdateProtocol (incorrectProtocol)
-- import Test.Plutip.Fixtures (aliceBobDistribution, aliceDistribution)
import Test.Plutip.Fixtures (aliceDistribution)

-- import Test.Plutip.Utils (isExpectedError)
-- import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Close Protocol" do

    test "Should successfully close protocol" do
      withWallets aliceDistribution \alice -> do
        withKeyWallet alice $ do
          protocol <- StartProtocol.startSystem startProtocolParams
          void $ CloseProtocol.contract protocol

-- NOTE: commented for faster testing of missingRequiredDatums error
-- test "Should fail if user doesn't have permissions to close Protocol" do
--   withWallets aliceBobDistribution \(alice /\ bob) -> do
--     protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
--     result <- try $ withKeyWallet bob $ CloseProtocol.contract protocol
--     let errMsg = "current user doesn't have permissions to close protocol"
--     result `shouldSatisfy` (isExpectedError errMsg)

-- test "Should fail if Protocol doesn't exist" do
--   withWallets aliceDistribution \alice -> do
--     protocol <- incorrectProtocol
--     result <- try $ withKeyWallet alice $ CloseProtocol.contract protocol
--     let errMsg = "Protocol UTxO with given nft not found"
--     result `shouldSatisfy` (isExpectedError errMsg)
