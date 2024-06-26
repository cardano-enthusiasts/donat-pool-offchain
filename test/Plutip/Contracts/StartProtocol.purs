module Test.Plutip.Contracts.StartProtocol (suite, startProtocolParams) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip (withWallets)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.BigInt as BigInt
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Protocol.UserData (ProtocolConfigParams(..))
import Test.Plutip.Fixtures (aliceDistribution, emptyAliceBobDistribution)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Start Protocol" do

    test "Should successfully start protocol" do
      withWallets aliceDistribution \alice -> withKeyWallet alice $ void $ StartProtocol.startProtocol startProtocolParams
    test "Should fail if user wallet doesn't have any UTxOs" do

      withWallets emptyAliceBobDistribution \(alice /\ _) -> do
        result <- try $ withKeyWallet alice $ startProtocolContract startProtocolParams
        let errMsg = "Failed to get non collateral utxo"
        result `shouldSatisfy` (isExpectedError errMsg)

startProtocolParams :: ProtocolConfigParams
startProtocolParams =
  ProtocolConfigParams
    { minAmountParam: BigInt.fromInt 2_000_000
    , maxAmountParam: BigInt.fromInt 100_000_000
    , minDurationParam: BigInt.fromInt 5 -- minutes
    , maxDurationParam: BigInt.fromInt 250 -- minutes
    , protocolFeeParam: BigInt.fromInt 5 -- percentage
    }

startProtocolContract
  :: ProtocolConfigParams
  -> Contract Unit
startProtocolContract = void <<< StartProtocol.startSystem
