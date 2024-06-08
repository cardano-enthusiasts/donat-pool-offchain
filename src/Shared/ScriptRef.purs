module Shared.ScriptRef where

import Contract.Prelude

import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), PlutusScript)
import Contract.Transaction (ScriptRef(..))
import Contract.TxConstraints as Constraints
import Ctl.Internal.Hashing as Hashing
import Ctl.Internal.Plutus.Types.Transaction (UtxoMap)
import Ctl.Internal.Types.Scripts (ValidatorHash)
import Data.Array as Array
import Data.Map as Map
import Effect.Exception (throw)
import Ext.Contract.Value (mkCurrencySymbol)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.FundraisingScriptInfo (makeFundraising)
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceValidatorScript)
import MintingPolicy.VerTokenMinting as VerToken
import Proposal.Model (mkProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalValidatorScript)
import Protocol.Models (Protocol)
import Protocol.ProtocolScript (getProtocolValidatorHash, protocolValidatorScript)
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.MinAda (sevenMinAdaValue)
import Shared.OwnCredentials (getOwnCreds)
import Shared.ScriptRefDatum (PScriptRefDatum(..))
import Shared.Tx (completeTx, toDatum)
import Shared.Utxo (UtxoTuple)

createRefScriptUtxo ∷ String -> ScriptRef -> ValidatorHash → Contract Unit
createRefScriptUtxo _ (NativeScriptRef _) _ = liftEffect $ throw "Unexpected scriptRef type: waiting for PlutusScriptRef"
createRefScriptUtxo scriptName scriptRef@(PlutusScriptRef _) validatorHash = do
  logInfo' $ "Start to create " <> scriptName <> " reference script"
  ownCreds <- getOwnCreds
  let
    datum = PScriptRefDatum { scriptName: scriptName }

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScriptAddressWithScriptRef
      validatorHash
      (ScriptCredential validatorHash)
      (toDatum datum)
      Constraints.DatumWitness
      scriptRef
      sevenMinAdaValue

    lookups :: Lookups.ScriptLookups Void
    lookups = mempty

  let datumHash = Hashing.datumHash $ toDatum datum
  logInfo' $ "[DATUM_HASH] " <> scriptName <> " reference script datum hash: " <> show datumHash
  completeTx lookups constraints ownCreds

  logInfo' $ scriptName <> " UTxO with reference script created"

mkProtocolRefScript :: ProtocolData -> Contract Unit
mkProtocolRefScript protocolData = do
  protocol <- dataToProtocol protocolData
  protocolValidatorHash <- getProtocolValidatorHash protocol
  protocolValidator <- protocolValidatorScript protocol
  let scriptRef = PlutusScriptRef (unwrap protocolValidator)
  createRefScriptUtxo "Protocol" scriptRef protocolValidatorHash

mkFundraisingRefScript :: ProtocolData -> Contract Unit
mkFundraisingRefScript protocolData = do
  fundraising <- makeFundraising protocolData
  frValidator <- fundraisingValidatorScript fundraising
  frValidatorHash <- getFundraisingValidatorHash fundraising
  let scriptRef = PlutusScriptRef (unwrap frValidator)
  createRefScriptUtxo "Fundraising" scriptRef frValidatorHash

mkProposalRefScript :: Protocol -> Contract Unit
mkProposalRefScript protocol = do
  _ /\ proposalVerTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  let proposal = mkProposal protocol proposalVerTokenCs
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalValidator <- proposalValidatorScript proposal
  let scriptRef = PlutusScriptRef (unwrap proposalValidator)
  createRefScriptUtxo "Proposal" scriptRef proposalValidatorHash

mkGovernanceRefScript :: Protocol -> Contract Unit
mkGovernanceRefScript protocol = do
  governanceValidatorHash <- getGovernanceValidatorHash protocol
  governanceValidator <- governanceValidatorScript protocol
  let scriptRef = PlutusScriptRef (unwrap governanceValidator)
  createRefScriptUtxo "Governance" scriptRef governanceValidatorHash

getPolicyScriptRef :: String -> MintingPolicy → Contract ScriptRef
getPolicyScriptRef _ (NativeMintingPolicy _) = liftEffect $ throw "Unexpected minting policy type"
getPolicyScriptRef mpName (PlutusMintingPolicy policy) = do
  logInfo' $ "Creating UTxO with " <> mpName <> " minting policy reference"
  pure $ PlutusScriptRef policy

createPolicyRefUtxo :: String -> MintingPolicy → ValidatorHash → Contract Unit
createPolicyRefUtxo mpName policy validatorHash = do
  scriptRef <- getPolicyScriptRef mpName policy
  createRefScriptUtxo "VerTokenPolicy" scriptRef validatorHash
  logInfo' $ "UTxO with " <> mpName <> " minting policy reference created"

mkVerTokenPolicyRef :: ProtocolData -> Contract Unit
mkVerTokenPolicyRef protocolData = do
  protocol <- dataToProtocol protocolData
  protocolValidatorHash <- getProtocolValidatorHash protocol
  policy <- VerToken.mintingPolicy protocol
  createPolicyRefUtxo "VerToken" policy protocolValidatorHash

getVerTokenPolicyRef ∷ Protocol → Contract ScriptRef
getVerTokenPolicyRef protocol = do
  policy <- VerToken.mintingPolicy protocol
  getPolicyScriptRef "VerToken" policy

hasExpectedRefScript :: PlutusScript -> UtxoTuple -> Boolean
hasExpectedRefScript plutusScript (_ /\ txOutput) =
  (unwrap txOutput).scriptRef == Just (PlutusScriptRef plutusScript)

findUtxoWithRefScript :: PlutusScript -> UtxoMap -> Maybe UtxoTuple
findUtxoWithRefScript plutusScript utxoMap =
  let
    (utxoArray :: Array UtxoTuple) = Map.toUnfoldable utxoMap
  in
    Array.find (hasExpectedRefScript plutusScript) utxoArray

getUtxoWithRefScript :: PlutusScript -> UtxoMap -> Contract UtxoTuple
getUtxoWithRefScript plutusScript utxoMap =
  liftContractM "UTxO with expected reference script not found" $ findUtxoWithRefScript plutusScript utxoMap
