---
title: Client API
description: Client API
order: 5
---

# Client Library API Reference

The client library provides functions for interacting with deployed Modsefa applications. Import from `Modsefa.Client.*` modules.

## Application Instance Management

### `createAppInstance'`

Creates an application instance for specific instance parameters.

```haskell
createAppInstance' :: forall app. AppSpec app 
                   => ParamsToValue (ResolveInstanceParamList (AppInstanceParameters app) (Validators app))
                   -> SAppInstance app
```

**Parameters:**

- Instance parameters as defined by your application's `AppInstanceParameters` type

**Returns:** A singleton application instance that can be used for transaction building

**Examples:**

For a simple single-parameter application like FeedApp:

```haskell
-- FeedApp has: type AppInstanceParameters FeedApp = '[ '("FeedValidator", "bootstrapUtxo") ]
let bootstrapRef = TxOutRef "a1b2c3d4..." 0
let appInstance = createAppInstance' @FeedApp bootstrapRef
```

For a multi-parameter application:

```haskell
-- If your app has multiple instance parameters:
-- type AppInstanceParameters MyApp = '[ '("ValidatorA", "paramX"), '("ValidatorB", "paramY") ]
let param1 = someValue
let param2 = anotherValue  
let appInstance = createAppInstance' @MyApp (param1, param2)
```

## State Querying

### `queryStateInstances`

Find all instances of a specific state type for an application.

```haskell
queryStateInstances :: forall st app.
  ( StateRepresentable st
  , FromData (GetStateData st)
  , AppSpec app
  , AppValidatorScripts app
  , StateInApp st app
  )
  => Proxy st
  -> ClientEnv
  -> SAppInstance app
  -> IO [StateInstance st]
```

**Parameters:**

- `Proxy st`: Type proxy for the state you want to query
- `ClientEnv`: The client environment containing providers and network ID.
- `SAppInstance app`: The specific application instance to query.

**Returns:** List of all state instances found on-chain

**Example:**

```haskell
-- First, get a ClientEnv
withClientEnv "config.json" "query-script" $ \clientEnv -> do

  -- In a real app, you would have your app-specific client functions
  -- like 'queryFeedConfig' and 'queryFeedData' that just take 'clientEnv'.

  -- For this example, we'll call the low-level 'queryStateInstances' directly.
  feedConfigs <- queryStateInstances 
    (Proxy @FeedConfigState) 
    clientEnv
    appInstance

  putStrLn $ "Found " ++ show (length feedConfigs) ++ " config(s)"
```

### `StateInstance`

Data type representing a state instance found on-chain.

```haskell
data StateInstance (st :: StateType) = StateInstance 
  { siUtxoRef :: GYTxOutRef
  , siData :: GetStateData st -- Actual parsed Haskell data
  , siValue :: GYValue
  , siTimestamp :: UTCTime
  }
```

**Field Access:**

```haskell
-- Access the parsed data 
let feedName = feedName (siData configInstance)
let owner = feedOwner (siData configInstance)

-- Access UTxO information
let ref = siUtxoRef dataInstance 
let value = siValue dataInstance
```

## Client Setup

You have two ways to set up your client, depending on your goal.

### 1. Query-Only Setup (`ClientEnv`)

For read-only operations, like querying state, you only need a `ClientEnv`, which holds the network connection. The `withClientEnv` helper is the standard way to get one.

```haskell
-- From Modsefa.Client.Types
data ClientEnv = ClientEnv
  { ceProviders :: !GYProviders
  , ceNetworkId :: !GYNetworkId
  }

withClientEnv :: FilePath -- ^ Path to config.json
              -> Text     -- ^ Namespace for logging
              -> (ClientEnv -> IO a) -- Action to run
              -> IO a
```

### 2. Action Setup (`ModsefaClient`)

For running transactions (like in scripts or tests), you need the full `ModsefaClient` handle. This bundles the network connection (`ClientEnv`), a `Wallet` for balancing, and `SigningMethod`s for signing.

```haskell
-- From Modsefa.Client.Types
data ModsefaClient = ModsefaClient
  { mcEnv     :: !ClientEnv
  , mcWallet  :: !Wallet
  , mcSigners :: ![(PubKeyHash, SigningMethod)]
  }

-- From Modsefa.Client.Wallet
data Wallet = Wallet { ... }
data SigningMethod where
  SignWithSKey :: ...
```

#### `loadModsefaClient`

For scripts and tests, the easiest way to create a handle is by using `loadModsefaClient`. This helper reads your config and a single signing key file to build a complete handle.

```haskell
-- From Modsefa.Client.TransactionRunner
loadModsefaClient
  :: FilePath           -- ^ Path to config.json
  -> FilePath           -- ^ Path to the user's signing key file
  -> Text               -- ^ Log namespace
  -> IO ModsefaClient
```

## High-Level Action Execution (`runAction`)

For most use cases like tests and scripts, the `runAction` function is the **recommended** way to execute a transaction. It wraps the entire build, sign, and submit process and allows you to pass action parameters as a standard Haskell tuple (e.g., `(val1, val2)`) thanks to the `ToSParamTuple` typeclass.

```haskell
-- From Modsefa.Client.TransactionRunner
runAction :: forall app action params tuple.
             ( ... -- Numerous type-level constraints
             , ToSParamTuple tuple params
             )
          => ModsefaClient        -- ^ The client session handle.
          -> SAppInstance app     -- ^ The specific application instance.
          -> Proxy action         -- ^ A proxy identifying the action.
          -> tuple                -- ^ The action parameters as a standard Haskell tuple.
          -> IO (Either Text GYTxId)
```

**Example:**

```haskell
-- Given an action spec with params: '[("name", String), ("owner", PKH)]
let paramsTuple = ("My Feed", ownerPKH)

-- runAction converts the tuple to the required SParamTuple automatically
result <- runAction client appInstance (Proxy @InitializeFeedSpec) paramsTuple
```

## Lower-Level Action Execution (`runModsefaAction`)

For simple, end-to-end execution where you have local access to all signing keys (e.g., in a test or a server-side script), `runModsefaAction` is the preferred method. It builds, balances, signs, and submits the transaction in one step.

### `runModsefaAction`

```haskell
runModsefaAction :: forall app action params . (...)
                 => ClientEnv
                 -> SAppInstance app
                 -> Proxy action
                 -> SParamTuple (ActionSpecParameters action)
                 -> Wallet               -- ^ Wallet for balancing & collateral
                 -> [(PubKeyHash, SigningMethod)] -- ^ All available signers
                 -> IO (Either Text GYTxId)
```

**Parameters:**

- `ClientEnv`: The client environment (usually from `mcEnv client`).
- `SAppInstance app`: The application instance.
- `Proxy action`: The action to execute.
- `SParamTuple (...)`: The type-safe parameters for the action.
- `Wallet`: The wallet to use for balancing, change, and collateral (from `mcWallet client`).
- `[(PubKeyHash, SigningMethod)]`: A list of all available signers (from `mcSigners client`).

**Returns:** Either an error message or the submitted `GYTxId`.

## Advanced Action Execution (For dApps & Web)

For complex scenarios like browser wallets, you must separate building the *skeleton* from building the *body* and submitting. The balancing and signing must be done by the end-user's wallet.

### Backend: Endpoint 1 (Build Skeleton)

Your backend's first endpoint uses `buildTransactionDirect` to create the *unbalanced* transaction skeleton. It **does not** need a `Wallet` or signers.

```haskell
-- From Modsefa.Core.Transaction.Builder
buildTransactionDirect :: forall app (action :: TypedActionSpec app). (...)
                       => SAppInstance app
                       -> Proxy action
                       -> SParamTuple (ActionSpecParameters action)
                       -> GYNetworkId
                       -> GYProviders
                       -> IO (Either Text (GYTxSkeleton (ExtractPlutusVersion app)))
```

Your backend runs this, gets the `GYTxSkeleton`, serializes it (e.g., to JSON), and sends it to the frontend.

### Frontend: Balancing & Signing

The user's browser (or wallet) receives the `GYTxSkeleton`. It then:

1. **Balances** the skeleton using the user's own UTxOs, collateral, and change address. This creates the `GYTxBody`.
2. Signs the `GYTxBody` using the user's private key (e.g., via a CIP-30 `wallet.api.signTx` call).
3. Sends the final, signed `GYTx` (or the `GYTxBody` and signatures) back to your backend.

### Backend: Endpoint 2 (Submit)

our backend's second endpoint receives the fully signed `GYTx` and simply submits it to the node.

```haskell
-- From Modsefa.Client.TransactionRunner
submitModsefaTx
  :: ClientEnv
  -> GYTx
  -> IO (Either Text GYTxId)
```

## Utility Functions

### `getPkhFromSKeyFile`

A helper function to read a file and extract its `PubKeyHash`.

```haskell
getPkhFromSKeyFile :: FilePath -> IO PubKeyHash
```

### `queryAndPrintState`

A helper to query for a state and pretty-print the result(s).

```haskell
queryAndPrintState
  :: forall app st f.
     (Show (GetStateData st), KnownSymbol (GetStateName st))
  => (SAppInstance app -> IO (f (StateInstance st))) -- Query function (f is Maybe or [])
  -> (f (StateInstance st) -> [StateInstance st]) -- Function to extract list
  -> SAppInstance app -- The application instance to query
  -> IO ()
```

## Simple Workflow Helpers (Tests & Scripts)

The `runModsefaAction` function is built from three components. You can use these individually if, for example, you want to build a transaction but sign and submit it later.

These are primarily for **server-side use** where the server has access to the wallet and keys for balancing.

### `buildModsefaTxBody`

Builds and *balances* an unsigned transaction. Requires a `Wallet` to provide balancing UTxOs, collateral, and a change address.

```haskell
buildModsefaTxBody :: forall app action params . (...)
                 => ClientEnv
                 -> SAppInstance app
                 -> Proxy action
                 -> SParamTuple params
                 -> Wallet               -- ^ The wallet for balancing
                 -> IO (Either Text GYTxBody)
```

### `signModsefaTxBody`

Signs a `GYTxBody` with all available local keys.

```haskell
signModsefaTxBody
  :: GYTxBody
  -> [(PubKeyHash, SigningMethod)]
  -> IO (Either Text GYTx)
```

## Parameter Construction

Modsefa provides two ways to construct the parameters for an action, depending on which execution function you use.

### 1. Simple Tuples (Recommended)

When using the high-level `runAction` function, you can pass parameters as a standard Haskell tuple. The `ToSParamTuple` typeclass (defined in `Modsefa.Client.ActionParams`) automatically converts the tuple into the required GADT.

This is the **preferred method** as it's simple and type-safe.

```haskell
-- For an action requiring (BuiltinByteString, PubKeyHash, Integer)
let paramsTuple = ("name", pubKeyHash, 42)

-- runAction handles the conversion automatically
runAction client appInstance (Proxy @MyAction) paramsTuple
```

### Manual Construction (Advanced)

When using the lower-level `runModsefaAction` function, or if you need to build parameters dynamically, you must use the `SParamTuple` GADT. This is done using the `(+:)` operator and `End` pattern.

#### Operator `(+:)`

Construct action parameter tuples manually using the `(+:)` operator. This is an alias for `STupleCons`.

```haskell
(+:) :: (Typeable t, ToData t, Eq t)
     => t
     -> SParamTuple rest
     -> SParamTuple ('(name, t) ': rest)
```

#### Pattern `End`

Terminate parameter construction. This is an alias for `STupleNil`.

```haskell
pattern End :: SParamTuple '[]
```

**Usage:**

```haskell
-- For an action requiring (BuiltinByteString, PubKeyHash, Integer)
let paramsGADT = "name" +: pubKeyHash +: 12 +: End

-- Pass the GADT to the lower-level function
runModsefaAction client appInstance (Proxy @MyAction) paramsGADT ...
```

## Type-Safe Parameter Validation

The parameter system prevents runtime errors by validating at compile time:

```haskell
-- This action expects (BuiltinByteString, PubKeyHash, BuiltinByteString)
type InitializeFeedSpec = 'ActionSpec @FeedApp "InitializeFeed" ... 
  '[ '("name", BuiltinByteString)
   , '("owner", PubKeyHash)  
   , '("content", BuiltinByteString)
   ]

-- ✅ Correct usage - compiles successfully
let validParams = "Feed Name" +: ownerKey +: "Content" +: End

-- ❌ Wrong type - compilation error
let invalidParams = "Feed Name" +: "not a key" +: "Content" +: End

-- ❌ Wrong number of parameters - compilation error  
let tooFewParams = "Feed Name" +: ownerKey +: End
```

## Complete Usage Example (Script/Test)

Here is a complete example for a test script that loads a `ModsefaClient` and executes an action using the high-level `runAction` function.

```haskell
import Modsefa.Client
import Modsefa.Examples.Feed.Spec (FeedApp, InitializeFeedSpec)
import PlutusLedgerApi.V3 (PubKeyHash, BuiltinByteString)

-- Your application-specific setup
let bootstrapRef = TxOutRef "a1b2c3d4..." 0
let appInstance = createAppInstance' @FeedApp bootstrapRef

-- 1. Define the action parameters as a standard tuple
let ownerPkh = "..." :: PubKeyHash
let paramsTuple = ("My Feed" :: BuiltinByteString, ownerPkh, "Hello!" :: BuiltinByteString)

-- 2. Load the client handle (this reads config.json and spender.skey)
client <- loadModsefaClient "config.json" "keys/spender.skey" "my-init-script"

-- 3. Call the generic runAction with the tuple
result <- runAction
  client
  appInstance
  (Proxy @InitializeFeedSpec)
  paramsTuple

case result of
  Left err   -> putStrLn $ "Action failed: " ++ unpack err
  Right txId -> putStrLn $ "Action successful! TxId: " ++ show txId
```

## Instance Parameter Types

The type of parameters required by `createAppInstance'` depends on your application's `AppInstanceParameters`:

### Single Parameter Applications

```haskell
-- Application with one instance parameter
type AppInstanceParameters FeedApp = '[ '("FeedValidator", "bootstrapUtxo") ]

-- Takes the parameter directly
createAppInstance' @FeedApp :: TxOutRef -> SAppInstance FeedApp
```

### Multi-Parameter Applications

```haskell
-- Application with multiple instance parameters
type AppInstanceParameters MyApp = 
  '[ '("ValidatorA", "paramX")
   , '("ValidatorB", "paramY") 
   ]

-- Takes a tuple of parameters
createAppInstance' @MyApp :: (TypeX, TypeY) -> SAppInstance MyApp
```

### No Parameter Applications

```haskell
-- Application with no instance parameters
type AppInstanceParameters SimpleApp = '[]

-- Takes unit
createAppInstance' @SimpleApp :: () -> SAppInstance SimpleApp
```

The type system ensures you provide exactly the parameters your application requires, with correct types and in the right order.
