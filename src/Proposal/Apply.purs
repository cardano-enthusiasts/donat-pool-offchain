module Proposal.Apply where

import Contract.Prelude
import Info.AllProposals

import Config.Protocol (mapToProtocolData, readProtocolConfig)
import Contract.Credential (Credential(ScriptCredential))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, runContract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Internal.Cardano.Types.Value (CurrencySymbol)
import Ctl.Internal.Contract.WaitUntilSlot (currentTime)
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Ctl.Internal.Plutus.Types.Value (flattenNonAdaAssets)
import Ctl.Internal.Types.Datum (Datum(..))
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.Lens (view)
import Data.Lens.Getter ((^.))
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Ext.Data.Boolean (booleanToBigInt)
import Proposal.Datum (PProposalDatum(..))
import Proposal.ProposalScript (proposalTokenName, proposalValidatorScript)
import Proposal.Redeemer (PProposalRedeemer(..))
import Protocol.Datum (PProtocolDatum(..), _managerAddress, _tokenOriginRef)
import Protocol.Models (PProtocolConfig(..), Protocol(..))
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..), getProtocolScriptInfo)
import Protocol.Redeemer (PProtocolRedeemer(..))
import Protocol.UserData (ProtocolConfigParams, ProtocolData, dataToProtocol, getConfigFromProtocolDatum, mapToProtocolConfig)
import Shared.Config (mapToProtocolConfigParams, readDonatPoolConfig)
import Shared.KeyWalletConfig (testnetKeyWalletConfig)
import Shared.OwnCredentials (OwnCredentials(..), getOwnCreds, getPkhSkhFromAddress)
import Shared.ScriptInfo (ScriptInfo(..))
import Shared.Tx (completeTx, toDatum, toRedeemer)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, extractValueFromUTxO, filterByToken)

-- runUpdateProtocol :: Effect Unit
-- runUpdateProtocol = do
--   protocolConfig <- readProtocolConfig
--   let protocolData = mapToProtocolData protocolConfig
--   donatPoolConfig <- readDonatPoolConfig
--   protocolConfigParams <- mapToProtocolConfigParams donatPoolConfig
--   launchAff_ $ runContract testnetKeyWalletConfig (contract protocolData protocolConfigParams)

contract :: ProtocolData -> Contract Unit
contract protocolData = do

  logInfo' "Running process proposals"
  protocol <- dataToProtocol protocolData

  now <- currentTime
  allProposalsInfo <- getAllProposalUtxos protocol
  let finished = Array.filter (isFinished now) allProposalsInfo
  let { yes: reachedQuorumList, no: notReachedQuorumList } = Array.partition hasReachedQuorum finished
  traverse_ processFailedToAchiveQuorumProposal notReachedQuorumList
  let { yes: applyList, no: rejectList } = Array.partition votedToApply reachedQuorumList
  traverse_ applyProposal applyList
  traverse_ rejectProposal rejectList
  logInfo' "Finished to process proposals"

-- utxoTupleToScriptInfo proposal utxo = do
--   proposalThreadTn <- proposalTokenName
--   proposalThreadCs <- liftContractM "Impossible to get threadTokenCs from proposal UTXO" $ getThreadCsByTn utxo proposalThreadTn
--   proposalDatum <-  liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO utxo
--   let proposalValue = extractValueFromUTxO utxo
--   proposalValidator <- proposalValidatorScript proposal
--   proposalValidarHash <- getProposalValidatorHash proposal
--   proposalAddress <-
--     liftContractM ("Impossible to get Proposal script address") $ validatorHashBaseAddress networkId validatorHash
--   utxos <- utxosAt address
--   let scriptRef = PlutusScriptRef (unwrap validator)
--   refScriptUtxo <- getUtxoByScriptRef scriptName scriptRef utxos
--   let refScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst refScriptUtxo) (snd refScriptUtxo)
--   pure $ ScriptInfo
--           { tokenName: proposalThreadTn
--           , validator: proposalValidator
--           , validatorHash: proposalValidarHash
--           , address: proposalAddress
--           , utxos: utxos
--           , utxo: utxo
--           , datum: proposalDatum
--           , value: proposalValue
--           , refScriptUtxo: refScriptUtxo
--           , refScriptInput: refScriptInput
--           }

markAsProcessed :: Protocol -> UtxoTuple -> Boolean -> Contract Unit
markAsProcessed protocol utxo isQuorumReached = do
  -- TODO: pass ScriptInfo instead of UtxoTuple
  -- ownCreds@(OwnCredentials creds) <- getOwnCreds
  -- proposalThreadTn <- proposalTokenName
  -- proposalThreadCs <- liftContractM "Impossible to get threadTokenCs from proposal UTXO" $ getThreadCsByTn utxo proposalThreadTn
  -- (PProposalDatum proposalDatum) <-  liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO utxo
  -- let proposalValue = extractValueFromUTxO utxo

  -- let redeemer = toRedeemer $ PRejectProposal proposalThreadCs (booleanToBigInt isQuorumReached)
  -- let newDatum = toDatum $ PProposalDatum
  --       { proposal: proposalDatum.proposal
  --       , for: proposalDatum.for
  --       , against: proposalDatum.against
  --       , policyRef: proposalDatum.policyRef
  --       , quorum: proposalDatum.quorum
  --       , initiator: proposalDatum.initiator
  --       , cost: proposalDatum.cost
  --       , deadline: proposalDatum.deadline
  --       , processed: fromInt 1
  --       }
  -- let newValue = if isQuorumReached then proposalValue else proposalValue - proposalDatum.cost
  -- let payToManager = 
  --   if isQuorumReached 
  --     then Constraints.mustPayToPubKeyAddress ownCreds.ownPkh ownCreds.ownSkh paymentToVoter
  logInfo' "Proposal is rejected"

rejectProposal :: ScriptInfo PProposalDatum -> Contract Unit
rejectProposal (ScriptInfo proposalScriptInfo) = do

  -- just mark proposal as processed
  logInfo' "Proposal is rejected"

processFailedToAchiveQuorumProposal :: ScriptInfo PProposalDatum -> Contract Unit
processFailedToAchiveQuorumProposal (ScriptInfo proposalScriptInfo) = do
  -- seize proposal fee from the script, because the quorum is not reached
  -- mark Proposal as processed

  -- ownCreds@(OwnCredentials creds) <- getOwnCreds
  -- let
  --   constraints :: Constraints.TxConstraints Void Void
  --   constraints =
  --     Constraints.mustSpendPubKeyOutput ownCreds.nonCollateralORef
  --       <> Constraints.mustMintValueWithRedeemer
  --         voteTokenRedeemer
  --         voteTokensValue

  --       <> Constraints.mustSpendScriptOutputUsingScriptRef
  --         (fst proposalScriptInfo.utxo)
  --         voteRedeemer
  --         proposalRefScriptInput
  --       <> Constraints.mustPayToScriptAddress
  --         proposalScriptInfo.validatorHash
  --         (ScriptCredential proposalScriptInfo.validatorHash)
  --         newProposalDatum
  --         Constraints.DatumInline
  --         paymentToProposal
  --       <> Constraints.mustPayToPubKeyAddress ownCreds.ownPkh ownCreds.ownSkh paymentToVoter
  --       <> Constraints.mustBeSignedBy ownCreds.ownPkh
  --       <> Constraints.mustValidateIn votingTimeRange
  --       <> Constraints.mustReferenceOutput (fst proposalRefScriptUtxo)
  --       <> Constraints.mustReferenceOutput (fst govScriptInfo.utxo)

  --   lookups :: Lookups.ScriptLookups Void
  --   lookups =
  --     Lookups.mintingPolicy voteMp
  --       <> Lookups.unspentOutputs ownCreds.ownUtxos
  --       <> Lookups.unspentOutputs proposalScriptInfo.utxos
  --       <> Lookups.unspentOutputs govScriptInfo.utxos

  logInfo' "Proposal is failed to archive a qourum, marked as processed"

applyProposal :: ScriptInfo PProposalDatum -> Contract Unit
applyProposal (ScriptInfo proposalScriptInfo) = do

  --   (ProtocolScriptInfo protocolInfo) <- getProtocolScriptInfo protocol
  --   ownCreds@(OwnCredentials creds) <- getOwnCreds

  --   manager /\ _ <- getPkhSkhFromAddress $ view _managerAddress protocolInfo.pDatum
  --   when (manager /= creds.ownPkh) $ liftEffect $ throw "Current user doesn't have permissions to update protocol"

  --   let protocolConfig = mapToProtocolConfig protocolConfigParams
  --   let newDatum = makeDatum protocolInfo.pDatum protocolConfig
  --   logInfo' $ "New datum: " <> show newDatum
  --   let newPDatum = Datum $ toData $ newDatum

  --   let updateProtocolRedeemer = Redeemer $ toData $ PUpdateProtocolConfig protocolConfig

  --   let
  --     constraints :: Constraints.TxConstraints Void Void
  --     constraints =
  --       Constraints.mustSpendScriptOutputUsingScriptRef
  --         (fst protocolInfo.pUtxo)
  --         updateProtocolRedeemer
  --         protocolInfo.references.pRefScriptInput
  --         <> Constraints.mustPayToScriptAddress
  --           protocolInfo.pValidatorHash
  --           (ScriptCredential protocolInfo.pValidatorHash)
  --           newPDatum
  --           Constraints.DatumInline
  --           protocolInfo.pValue
  --         <> Constraints.mustReferenceOutput (fst protocolInfo.references.pScriptRef)
  --         <> Constraints.mustBeSignedBy creds.ownPkh
  --   let
  --     lookups :: Lookups.ScriptLookups Void
  --     lookups =
  --       Lookups.unspentOutputs protocolInfo.pUtxos

  --   completeTx lookups constraints ownCreds

  --   pure $ getConfigFromProtocolDatum newDatum

  -- makeDatum ∷ PProtocolDatum -> PProtocolConfig → PProtocolDatum
  -- makeDatum currentDatum (PProtocolConfig { minAmount, maxAmount, minDuration, maxDuration, protocolFee }) =
  --   PProtocolDatum
  --     { minAmount: minAmount
  --     , maxAmount: maxAmount
  --     , minDuration: minDuration
  --     , maxDuration: maxDuration
  --     , protocolFee: protocolFee
  --     , managerAddress: view _managerAddress currentDatum
  --     , tokenOriginRef: view _tokenOriginRef currentDatum
  --     }
  logInfo' "Proposal is applied"