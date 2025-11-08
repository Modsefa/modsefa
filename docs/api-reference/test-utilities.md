---
title: Test Utilities API
description: Test Utilities API
order: 6
---

# Testing Utilities API Reference

Modsefa provides testing utilities to validate your applications by deliberately corrupting transactions and testing edge cases. Import from `Modsefa.Test.*` modules.

## Overview

The testing framework operates on a key principle: **since Modsefa makes it hard to create invalid transactions, we provide tools to systematically corrupt valid transactions to test validator edge cases**. This ensures comprehensive coverage of failure modes that might otherwise be difficult to test.

## Test Harness

For most common test cases (single-signer, "happy path" actions), Modsefa provides a simple test harness to handle all the boilerplate of loading clients, running actions, and checking results.

### `runScenario`

This is the main helper function for writing simple tests. It handles:

1. Loading the `ModsefaClient` from config and key files.
2. Printing "Running..." and "Complete" banners.
3. Running the client action you provide.
4. Printing the success/failure result.
5. On success, waiting for 5 seconds for the chain to index.
6. Running the verification function you provide.
7. Printing the verification success/failure.

```haskell
-- From Modsefa.Test.Harness
runScenario
  :: forall app.
     String                                  -- ^ Test name for logging.
  -> FilePath                                -- ^ Path to the config.json file.
  -> FilePath                                -- ^ Path to the signer's skey file.
  -> SAppInstance app                        -- ^ The application instance to test.
  -> (ModsefaClient -> IO (Either Text GYTxId)) -- ^ The client action to run.
  -> (ClientEnv -> SAppInstance app -> IO Bool) -- ^ The verification function.
  -> IO ()
```

**Example:**

```haskell
runFeedInitTest :: String -> Integer -> IO ()
runFeedInitTest txHashStr txIndex = do
  let eAppInst = mkFeedAppInstance txHashStr txIndex
  case eAppInst of
    Left err -> putStrLn $ "❌ ERROR: Could not create App Instance: " ++ unpack err
    Right appInstance -> do
      
      -- 1. Define the action
      let actionToRun client = do
            let (ownerPkh, _) = head (mcSigners client)
            let params = "Test Init Feed" +: ownerPkh +: "Content" +: End
            initializeFeedExplicitRef client appInstance params

      -- 2. Define the verification
      let verify clientEnv appInst = do
            mConfig <- queryFeedConfig clientEnv appInst
            dataStates <- queryFeedData clientEnv appInst
            return (isJust mConfig && not (null dataStates))

      -- 3. Run the harness
      runScenario
        "InitializeFeed"
        "config.json"
        "keys/spender.skey"
        appInstance
        actionToRun
        verify
```

## Core Mutation Types

### `ActionMutation`

Defines specific ways to corrupt transactions for testing validator behavior. This is a GADT (Generalized Algebraic Data Type) where each constructor represents a different type of corruption, tailored to Modsefa's concepts like preserved fields, minted tokens, state references, etc..

```haskell
-- Simplified representation - see source for full constraints
data ActionMutation (app :: Type) (action :: TypedActionSpec app) where
  CorruptPreservedFieldInRef :: SStateRef st ref -> Proxy field -> Data -> ActionMutation app action
  CorruptMintedTokenName :: GYTokenName -> GYTokenName -> ActionMutation app action
  CorruptConstantField :: SStateType st -> Proxy field -> Data -> ActionMutation app action
  CorruptCalculatedField :: SStateType st -> (GetStateData st -> GetStateData st) -> ActionMutation app action
  CorruptReferenceInput :: SStateType st -> TxOutRef -> ActionMutation app action
  CorruptConstraintOutput :: SStateType st -> (GYValue -> GYValue) -> ActionMutation app action
  -- ... potentially other constructors ...
```

_(Note: This snippet shows the structure but omits detailed type constraints for brevity. Refer to the source code for the full definition.)_

**Usage Examples:**

```haskell
-- Corrupt a preserved field in a specific state reference
let fieldCorruption = CorruptPreservedFieldInRef 
      (STypedTheOnlyInstance (SStateType @FeedConfigState))
      (Proxy @"feedOwner")
      (toData ("corruptedkey123..." :: PubKeyHash))

-- Corrupt token names in minting operations
let tokenCorruption = CorruptMintedTokenName
      { cmtnOriginalTokenName = "FeedData"
      , cmtnCorruptedTokenName = "CorruptedFeedData"
      }
```

**`ActionMutation` Constructors Explained**

Each constructor targets a specific aspect of the transaction derived from the Modsefa action specification, allowing you to test different parts of the generated validator logic:

- `CorruptPreservedFieldInRef`
  - **Purpose:** Tests the validator's enforcement of the `'Preserve'` field specification during an `'Update'` operation.
  - **How it Works:** Takes a state reference (`SStateRef`) identifying the state instance being updated, the `Proxy` of the field that _should_ be preserved, and incorrect `Data`. It finds the corresponding output UTxO and replaces the value of the specified field in its datum with the incorrect `Data`.
  - **Expected Validator Behavior:** Reject the transaction because the specified field's value changed between the input and output UTxO for the updated state.

- `CorruptMintedTokenName`
  - **Purpose:** Tests the validator's minting policy logic, specifically the validation of token names associated with state creation/deletion using `'OwnPolicy'`.
  - **How it Works:** Takes the expected `GYTokenName` (usually derived from the `StateType` name) and a different, incorrect `GYTokenName`. It finds the minting operation in the transaction skeleton (`gytxMint`) corresponding to the validator's own minting policy and replaces the original token name with the corrupted one. It also performs the same replacement in the `GYValue` of the corresponding output UTxO (`gytxOuts`).
  - **Expected Validator Behavior:** Reject the transaction because the minted/burned token name does not match the expected name derived from the state type or redeemer context.

- `CorruptConstantField`
  - **Purpose:** Tests the validator's validation of fields set to constant values using `'SetTo'` with `'EnumValue'` or `'IntValue'`.
  - **How it Works:** Takes the target `SStateType`, the `Proxy` of the field being set to a constant, and incorrect `Data`. It finds the output UTxO(s) corresponding to the `StateType` and replaces the value of the specified field in the datum with the incorrect `Data`.
  - **Expected Validator Behavior:** Reject the transaction because the datum field's value does not match the constant value expected by the validator logic for that action.

- `CorruptCalculatedField`
  - **Purpose:** Tests the validator's validation of fields whose values are derived from other inputs or context (e.g., using `'StateFieldValue'`, `'CurrentTime'`, or arithmetic operations like `'AddValue'`).
  - **How it Works:** Takes the target `SStateType` and a Haskell function (`GetStateData st -> GetStateData st`). It finds the output UTxO, decodes its datum, applies the provided function to corrupt the Haskell value, re-encodes it, and updates the output datum.
  - **Expected Validator Behavior:** Reject the transaction because the calculated field's value in the output datum does not match the result the validator calculates based on transaction inputs and context.

- `CorruptReferenceInput`
  - **Purpose:** Tests validator logic that relies on reading data from reference inputs (e.g., for signature checks via `MustBeSignedByState` or complex validation rules). It can also test instance consistency checks derived during compilation.
  - **How it Works:** Takes the `SStateType` of the reference input UTxO to target and an incorrect `TxOutRef`. It finds the original `TxOutRef` corresponding to that state type in the skeleton's reference input set (`gytxRefIns`) and replaces it with the incorrect one.
  - **Expected Validator Behavior:** Reject the transaction, either because the required datum cannot be found/decoded at the incorrect reference, or because instance consistency checks (comparing the referenced UTxO's address/origin) fail.

- `CorruptConstraintOutput`
  - **Purpose:** Tests validator logic enforcing constraints that mandate specific outputs, such as `'MustAddToAggregateState'` or `'MustWithdrawFromAggregateState'`.
  - **How it Works:** Takes the `SStateType` of the aggregate state involved in the constraint and a Haskell function (`GYValue -> GYValue`) to modify the value. It finds the output UTxO directed to the managing validator's address (which was created to satisfy the constraint) and applies the function to corrupt its `GYValue` (e.g., changing the amount paid to a treasury).
  - **Expected Validator Behavior:** Reject the transaction because the value in the output UTXO does not match the amount required by the constraint logic within the validator.

### `ActionMutationStrategy`

Groups mutations with a description for organized testing.

```haskell
data ActionMutationStrategy app action = ActionMutationStrategy
  { mutations :: [ActionMutation app action]
  , description :: Text
  }
```

**Example:**

```haskell
let ownershipCorruption = ActionMutationStrategy
  [ CorruptPreservedFieldInRef 
      (STypedTheOnlyInstance (SStateType @ServiceConfigState))
      (Proxy @"serviceConfigProvider")
      (toData ("maliciouskey..." :: PubKeyHash))
  ]
  "Test corruption of service provider field"
```

## Main Testing Function

### `buildRefCorruptedTransaction`

Build a transaction with systematic corruption applied to test validator robustness.

```haskell
buildRefCorruptedTransaction ::
  forall app action.
  ( AutoSingletonActionSpec action
  , KnownSymbol (ActionSpecName action)
  , AllStateTypesGeneric (ExtractStateTypes (ExtractOpsFromAction action))
  , AppValidatorScripts app
  , AppSpec app
  , Typeable (ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app)))
  , ActionConstraintsValid (ExtractConstraintsFromAction action)
  , SingPlutusVersionI (ExtractPlutusVersionFromValidators (Validators app))
  , ProcessActionSteps app action (ActionSpecSteps action) (ActionSpecParameters action) (ExtractPlutusVersion app)
  , AutoSingletonParamDerivationList (ParameterDerivations app)
  ) =>
  SAppInstance app ->
  Proxy action ->
  SParamTuple (ActionSpecParameters action) ->
  ActionMutationStrategy app action ->
  GYNetworkId ->
  GYProviders ->
  IO (Either Text (GYTxSkeleton (ExtractPlutusVersion app)))
```

**Parameters:**

- `SAppInstance app`: Application instance
- `Proxy action`: Action to execute with corruption
- `SParamTuple (ActionSpecParameters action)`: Valid parameters for the action
- `ActionMutationStrategy app action`: Strategy defining which corruptions to apply
- `GYNetworkId`: Target network
- `GYProviders`: Atlas providers

**Returns:** Either an error or a corrupted transaction skeleton

**Example:**

```haskell
-- Test that validator rejects transactions with corrupted owner field
let strategy = ActionMutationStrategy
  [ CorruptPreservedFieldInRef 
      (STypedTheOnlyInstance (SStateType @FeedConfigState))
      (Proxy @"feedOwner")
      (toData ("maliciousowner..." :: PubKeyHash))
  ]
  "Test owner field corruption"

result <- buildRefCorruptedTransaction 
  appInstance 
  (Proxy @UpdateFeedSpec) 
  validParams 
  strategy
  networkId 
  providers

-- The corrupted transaction should fail validation
case result of
  Left err -> putStrLn "✓ Transaction building failed (expected for some corruptions)"
  Right skeleton -> do
    -- Try to submit - should fail at validator level
    submitResult <- trySubmitTransaction skeleton
    case submitResult of
      Left validationError -> putStrLn "✓ On-chain validation caught the corruption"
      Right _ -> putStrLn "✗ Validator failed to catch corruption - bug in validator!"
```

## Generic Corruption System

### `GCorruptField`

Generic typeclass for corrupting fields by name in any data structure.

```haskell
class GCorruptField (rep :: Type -> Type) where
  gCorruptField :: 
    String ->           -- target field name
    Data ->             -- corruption value
    rep p ->            -- original generic representation
    Either Text (rep p) -- corrupted representation or error
```

### `corruptFieldGenerically`

Top-level function to corrupt any field in a state type using Generic machinery.

```haskell
corruptFieldGenerically ::
  forall st.
  ( StateRepresentable st
  , Generic (GetStateData st)
  , GCorruptField (Rep (GetStateData st))
  ) =>
  GetStateData st ->  -- Original state data
  String ->           -- Field name to corrupt
  Data ->             -- Corruption value
  Either Text (GetStateData st) -- Corrupted state data
```

**Example:**

```haskell
-- Corrupt the feedOwner field in FeedConfig
let originalConfig = FeedConfig "My Feed" originalOwnerKey
let corruptionValue = toData ("maliciouskey..." :: PubKeyHash)

result <- corruptFieldGenerically @FeedConfigState 
  originalConfig 
  "feedOwner" 
  corruptionValue

case result of
  Left err -> putStrLn $ "Corruption failed: " ++ show err
  Right corruptedConfig -> putStrLn $ "Successfully corrupted: " ++ show corruptedConfig
```

## Complete Testing Example

Here's a comprehensive example showing how to test a Subscription application:

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Modsefa.Test.RefAwareMutation
import PlutusLedgerApi.V3 (toData, PubKeyHash)

-- Test suite for Subscription application
testSubscriptionCorruption :: String -> Integer -> IO ()
testSubscriptionCorruption txHash txIndex = do
  putStrLn "=== Testing Subscription Application with Corruption ==="
  
  -- Setup
  let bootstrapRef = TxOutRef (fromString txHash) txIndex
  let appInstance = createAppInstance' @SubscriptionApp bootstrapRef
  let validParams = STupleCons ("Test Service Name" :: BuiltinByteString) STupleNil

  -- Test 1: Corrupt service provider field
  putStrLn "Testing service provider corruption..."
  testServiceProviderCorruption appInstance validParams

  -- Test 2: Corrupt token names
  putStrLn "Testing token name corruption..."  
  testTokenNameCorruption appInstance validParams

testServiceProviderCorruption :: SAppInstance SubscriptionApp 
                              -> SParamTuple (ActionSpecParameters UpdateServiceConfigSpec) 
                              -> IO ()
testServiceProviderCorruption appInstance params = do
  let strategy = ActionMutationStrategy
        [ CorruptPreservedFieldInRef 
            (STypedTheOnlyInstance (SStateType @ServiceConfigState))
            (Proxy @"serviceConfigProvider")
            (toData ("maliciouskey0123456789abcdef0123456789abcdef01234567" :: PubKeyHash))
        ]
        "Test corruption of serviceConfigProvider field"
  
  putStrLn $ "Calling buildRefCorruptedTransaction with strategy: " ++ show strategy
  
  -- Load network configuration
  coreCfg <- coreConfigIO "config.json"
  let networkId = cfgNetworkId coreCfg
  
  withCfgProviders coreCfg "corruption-test" $ \providers -> do
    -- Build the corrupted transaction
    result <- buildRefCorruptedTransaction
      appInstance
      (Proxy @UpdateServiceConfigSpec)
      params
      strategy
      networkId
      providers
    
    case result of
      Left err -> putStrLn $ "✓ Transaction building failed with corruption: " ++ show err
      Right skeleton -> do
        putStrLn "✓ Corrupted transaction built - testing submission..."
        submitResult <- trySubmitCorruptedTransaction skeleton
        case submitResult of
          TransactionRejected -> putStrLn "✓ Validator correctly rejected corrupted transaction"
          TransactionAccepted -> putStrLn "✗ BUG: Validator accepted corrupted transaction!"

testTokenNameCorruption :: SAppInstance SubscriptionApp 
                        -> SParamTuple (ActionSpecParameters UpdateServiceConfigSpec) 
                        -> IO ()
testTokenNameCorruption appInstance params = do
  let strategy = ActionMutationStrategy
        [ CorruptMintedTokenName
            { cmtnOriginalTokenName = "ServiceConfig"
            , cmtnCorruptedTokenName = "MaliciousConfig"
            }
        ]
        "Test corruption of minted token names"
  
  -- Similar testing pattern as above...
  putStrLn "Testing token name corruption (implementation similar to above)"

-- Helper function to test transaction submission
data SubmissionResult = TransactionAccepted | TransactionRejected

trySubmitCorruptedTransaction :: GYTxSkeleton v -> IO SubmissionResult
trySubmitCorruptedTransaction skeleton = do
  -- Implementation would attempt to submit and catch validation errors
  -- This is a placeholder showing the expected interface
  putStrLn "Attempting to submit corrupted transaction..."
  -- In real implementation, this would try to submit and return based on success/failure
  return TransactionRejected
```

## Testing Workflow

### Systematic Testing Approach

1. **Build valid transaction first** - Ensure your application works correctly
2. **Apply specific corruptions** - Test individual corruption types
3. **Verify rejection** - Ensure validators reject corrupted transactions
4. **Test combinations** - Some bugs only appear with multiple corruptions

### Key Testing Patterns

#### Field Corruption Testing

```haskell
-- Test that preserved fields are actually preserved
let preservedFieldTest = CorruptPreservedFieldInRef 
      stateRef 
      (Proxy @"criticalField") 
      (toData corruptValue)
```

#### Token Name Corruption Testing

```haskell
-- Test that token names are validated properly
let tokenTest = CorruptMintedTokenName
  { cmtnOriginalTokenName = "ExpectedToken"
  , cmtnCorruptedTokenName = "UnexpectedToken"
  }
```

#### Multiple Corruption Testing

```haskell
-- Test combinations of corruptions
let combinedStrategy = ActionMutationStrategy
  [ CorruptPreservedFieldInRef stateRef (Proxy @"field1") value1
  , CorruptMintedTokenName original corrupted
  ]
  "Combined corruption test"
```

## Best Practices

### Coverage Guidelines

- **Test all critical fields** that should be preserved or validated
- **Test token validation** for minting operations
- **Test state reference integrity**
- **Test both individual and combined corruptions**

### Performance Considerations

- Test with realistic transaction sizes
- Use appropriate timeouts for complex operations
- Cache application instances between tests
- Run expensive tests in CI, focused tests during development

The testing utilities provide confidence that your Modsefa applications correctly reject invalid transactions, ensuring your validators are robust against both accidental errors and malicious attacks.
