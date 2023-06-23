module Fundraising.Create where

import Contract.Prelude

import Contract.Address (getNetworkId, getWalletAddresses, ownPaymentPubKeysHashes, getWalletAddressesWithNetworkTag, validatorHashBaseAddress, addressToBech32)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder, mustSendChangeToAddress)
import Contract.Chain (currentTime)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContract, liftContractM, liftedM, liftedE)
import Contract.PlutusData (Redeemer(Redeemer), Datum(Datum), toData)
import Contract.ScriptLookups as Lookups
import Contract.Time (POSIXTime(..))
import Contract.Transaction (awaitTxConfirmed, balanceTxWithConstraints, signTransaction, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.ByteArray (byteArrayFromAscii)
import Data.Array (head) as Array
import Data.BigInt (fromInt, toString)
import Data.Lens (view)
import Data.Map (toUnfoldable) as Map
import Data.String (take)
import Effect.Aff (runAff_)
import Effect.Exception (throw, Error, message)
import Fundraising.Datum (PFundraisingDatum(..), descLength)
import Fundraising.FundraisingScript (getFundraisingTokenName, fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.UserData (CreateFundraisingParams(..))
import Info.AppInfo (getProtocolUtxo)
import Info.UserData (FundraisingInfo(..))
import MintingPolicy.NftMinting as NFT
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import MintingPolicy.VerTokenMinting as VerToken
import MintingPolicy.VerTokenRedeemers (PVerTokenRedeemer(..))
import Protocol.Datum (_protocolFee, _minDuration, _maxDuration, _minAmount, _maxAmount, _managerPkh)
import Protocol.Models (PFundriseConfig(..))
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.Duration (durationToMinutes, minutesToPosixTime)
import Shared.MinAda (minAdaValue)
import Shared.TestnetConfig (mkTestnetNamiConfig)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, filterNonCollateral)
import Ext.Contract.Value (currencySymbolToString, mkCurrencySymbol)
import Ext.Contract.Time (addTimes)

runCreateFundraising :: (FundraisingInfo -> Effect Unit) -> (String -> Effect Unit) -> ProtocolData -> CreateFundraisingParams -> Effect Unit
runCreateFundraising onComplete onError protocolData params = do
  testnetNamiConfig <- mkTestnetNamiConfig
  runAff_ handler $ runContract testnetNamiConfig (contract protocolData params)
  where
  handler :: Either Error FundraisingInfo -> Effect Unit
  handler (Right response) = onComplete response
  handler (Left err) = onError $ message err

contract :: ProtocolData -> CreateFundraisingParams -> Contract FundraisingInfo
contract protocolData (CreateFundraisingParams { description, amount, duration }) = do
  logInfo' "Running Create Fundraising contract"
  givenProtocol <- dataToProtocol protocolData
  ownHashes <- ownPaymentPubKeysHashes
  ownPkh <- liftContractM "Impossible to get own PaymentPubkeyHash" $ Array.head ownHashes
  logInfo' $ "Own Payment pkh is: " <> show ownPkh

  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  logInfo' $ "Own address is: " <> show ownAddress
  ownUtxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show ownUtxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (filterNonCollateral $ Map.toUnfoldable ownUtxos))
  logInfo' $ "Desired user UTxO is: " <> show oref
  nftMp /\ nftCs <- mkCurrencySymbol (NFT.mintingPolicy oref)
  logInfo' $ "NFT currency symbol: " <> show nftCs
  nftTn <- getFundraisingTokenName
  logInfo' $ "NFT token name: " <> show nftTn

  verTokenMp /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy givenProtocol)
  logInfo' $ "VerToken currency symbol: " <> show verTokenCs
  verTn <- VerToken.verTokenName
  logInfo' $ "Ver token name: " <> show verTn

  protocolValidator <- protocolValidatorScript givenProtocol
  protocolValidatorHash <- getProtocolValidatorHash givenProtocol
  networkId <- getNetworkId
  protocolAddress <-
    liftContractM "Impossible to get Protocol script address" $ validatorHashBaseAddress networkId protocolValidatorHash
  logInfo' $ "Protocol validator address: " <> show protocolAddress
  protocolUtxos <- utxosAt protocolAddress
  logInfo' $ "Protocol UTxOs list: " <> show protocolUtxos
  protocolUtxo <- getProtocolUtxo givenProtocol protocolUtxos
  logInfo' $ "Desired protocol UTxO: " <> show protocolUtxo
  protocolDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO protocolUtxo
  logInfo' $ "Protocol Datum: " <> show protocolDatum

  let
    minAmount = view _minAmount protocolDatum
    maxAmount = view _maxAmount protocolDatum
    targetAmount = fromInt amount * fromInt 1_000_000

  when (targetAmount < minAmount) $ liftEffect $ throw ("Fundraising amount too small. It must be greater than " <> toString minAmount <> ".")
  when (targetAmount > maxAmount) $ liftEffect $ throw ("Fundraising amount too big. It must be less than " <> toString maxAmount <> ".")

  let
    minDurationMinutes = view _minDuration protocolDatum
    maxDurationMinutes = view _maxDuration protocolDatum
    frDurationMinutes = durationToMinutes duration

  when (frDurationMinutes < minDurationMinutes) $ liftEffect $ throw ("Fundraising duration too short. It must be greater than " <> toString minDurationMinutes <> ".")
  when (frDurationMinutes > maxDurationMinutes) $ liftEffect $ throw ("Fundraising duration too long. It must be less than " <> toString maxDurationMinutes <> ".")

  now@(POSIXTime now') <- currentTime
  let deadline = addTimes now (minutesToPosixTime frDurationMinutes)
  desc <- liftContractM "Impossible to serialize description" $ byteArrayFromAscii (take descLength description)

  let
    initialFrDatum = PFundraisingDatum
      { creatorPkh: ownPkh
      , tokenOrigin: oref
      , frDesc: desc
      , frAmount: targetAmount
      , frDeadline: deadline
      , frFee: view _protocolFee protocolDatum
      , managerPkh: view _managerPkh protocolDatum
      }

  let
    fundraising = Fundraising
      { protocol: givenProtocol
      , verTokenCurrency: verTokenCs
      , verTokenName: verTn
      }

  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising

  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress networkId frValidatorHash

  let
    protocolRedeemerData = PFundriseConfig
      { scriptAddress: frAddress
      , verCurrencySymbol: verTokenCs
      , verTokenName: verTn
      , threadCurrencySymbol: nftCs
      , threadTokenName: nftTn
      , startedAt: now'
      }

    protocolRedeemer = Redeemer $ toData (PStartFundrise protocolRedeemerData)

  let
    nftValue = Value.singleton nftCs nftTn one
    verTokenValue = Value.singleton verTokenCs verTn one
    paymentToProtocol = extractValueFromUTxO protocolUtxo
    paymentToFr = minAdaValue <> minAdaValue <> nftValue <> verTokenValue

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustSpendPubKeyOutput oref
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintNft nftTn)
          nftValue
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ PMintVerToken verTn)
          verTokenValue
        <> Constraints.mustSpendScriptOutput
          (fst protocolUtxo)
          protocolRedeemer
        <> Constraints.mustPayToScriptAddress
          protocolValidatorHash
          (ScriptCredential protocolValidatorHash)
          (Datum $ toData protocolDatum)
          Constraints.DatumInline
          paymentToProtocol
        <> Constraints.mustPayToScriptAddress
          frValidatorHash
          (ScriptCredential frValidatorHash)
          (Datum $ toData initialFrDatum)
          Constraints.DatumInline
          paymentToFr
        <> Constraints.mustBeSignedBy ownPkh

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy nftMp
        <> Lookups.mintingPolicy verTokenMp
        <> Lookups.unspentOutputs ownUtxos
        <> Lookups.unspentOutputs protocolUtxos
        <> Lookups.validator protocolValidator
        <> Lookups.validator frValidator

  unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  addressWithNetworkTag <- liftedM "Failed to get own address with Network Tag" $ Array.head <$> getWalletAddressesWithNetworkTag
  let
    balanceTxConstraints :: BalanceTxConstraintsBuilder
    balanceTxConstraints = mustSendChangeToAddress addressWithNetworkTag

  balancedTx <- liftedE $ balanceTxWithConstraints unbalancedTx balanceTxConstraints
  balancedSignedTx <- signTransaction balancedTx
  txId <- submit balancedSignedTx
  awaitTxConfirmed txId

  logInfo' "Fundraising created successfully"

  logInfo' $ "Current fundraising: " <> show fundraising

  bech32Address <- addressToBech32 frAddress
  logInfo' $ "Current fundraising address: " <> show bech32Address

  pure $ FundraisingInfo
    { creator: ownPkh
    , description: description
    , goal: targetAmount
    , raisedAmt: fromInt 0
    , deadline: deadline
    , threadTokenCurrency: nftCs
    , threadTokenName: nftTn
    , path: currencySymbolToString nftCs
    }

