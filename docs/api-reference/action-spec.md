---
title: Action Spec API
description: Action Spec API
order: 1
---

# Action Specification API Reference

Action specifications define what users can do with your application. They are type-level declarations that generate both validator logic and transaction builders.

## Action Structure

### `ActionSpec`

The core type for defining actions.

```haskell
data TypedActionSpec where
  ActionSpec
    :: Symbol                    -- ^ The unique name identifying this action.
    -> [ActionStep]              -- ^ A sequence of 'ActionStep's defining the action's logic.
    -> [TypedConstraint]         -- ^ Validation constraints applied to the overall transaction for this action.
    -> [(Symbol, Type)]          -- ^ Parameter specifications (name and type) required by this action.
    -> TypedActionSpec
```

**Type Parameters:**

- `name`: Action name as a type-level string
- `operations`: List of operations this action performs
- `constraints`: List of validation constraints
- `parameters`: Required user parameters with their types

**Example:**

```haskell
type CreateUserSpec =
  'ActionSpec "CreateUser"
    '[ 'Op ('Create @UserState
         '[ 'SetTo "userName" ('ParamValue "name")
          , 'SetTo "userEmail" ('ParamValue "email")
          ]
         '[])
     ]
    '[ 'MustBeSignedByParam "userKey"
     ]
    '[ '("name", BuiltinByteString)
     , '("email", BuiltinByteString)
     , '("userKey", PubKeyHash)
     ]
```

## Operations

Operations define how your action modifies application state.

### `Op` - Single Operation

Perform one operation on a state type.

```haskell
'Op (operation :: TypedOperation)
```

### `Let` - Named Operation

Perform an operation and give its result a label. This label can then be used by subsequent operations within the same action to either **extract field values** from the referenced state or to **perform further operations** on that exact state instance.

```haskell
'Let (label :: Symbol) (operation :: TypedOperation)
```

**Example:**

In this example, a `CouponState` is first referenced and given a label `"selectedCoupon"`. A subsequent `Delete` operation then uses this exact label via a `TypedByLabel` to remove that specific coupon from the ledger. 

```haskell
type SubscribeWithCouponSpec =
  'ActionSpec "SubscribeWithCoupon"
    '[ 'Let "selectedTier" ('Reference @PricingTierState ...) 
     , 'Let "selectedCoupon" ('Reference @CouponState ...) 
     , 'Op ('Create @CustomerSubscriptionState
         '[ 'SetTo "customerSubscriptionPrice"
              ('SubtractValue
                 ('StateFieldValue "selectedTier" "pricingTierPrice")
                 ('DivideValue
                    ('MultiplyValue
                       ('StateFieldValue "selectedTier" "pricingTierPrice")
                       ('StateFieldValue "selectedCoupon" "couponDiscountPercent")
                    )
                    ('IntValue 100)
                 )
              )
          -- ... other fields are set here
          ] 
         '[])
     , 'Op ('Delete @CouponState ('TypedByLabel "selectedCoupon") '[])
     ] 
    -- ... constraints and parameters
```

### `Map` - Batch Operations

Perform the same operation on multiple items from a parameter.

```haskell
'Map (operation :: TypedOperation)
     (parameter :: Symbol)
     (constraints :: [CollectionConstraint])
```

**Example:**

```haskell
-- Create multiple coupons from a list parameter
'[ 'Map 
     ('Create @CouponState
        '[ 'SetTo "couponId" ('ParamValue "couponId")
         , 'SetTo "discount" ('ParamValue "discount")
         ]
        '[])
     "couponList"
     '[ 'MustHaveUniqueField "couponId" ]
 ]
```

## Operation Types

### `Create` - Create New State

Create a new instance of a state type.

```haskell
'Create @s (fieldSpecs :: [FieldSpec]) (constraints :: [TypedConstraint])
```

**Parameters:**

- `@s`: The state tag type to create (e.g., `@FeedConfigState`). The framework uses this tag to find the corresponding `StateSpec` instance.
- `fieldSpecs`: How to set each field in the new state
- `constraints`: Additional constraints for this operation

**Example:**

```haskell
'Op ('Create @FeedDataState
   '[ 'SetTo "feedData" ('ParamValue "content")
    , 'SetTo "feedStatus" ('EnumValue FeedStatus "Active")
    ]
   '[])
```

### `Update` - Modify Existing State

Update an existing state instance.

```haskell
'Update (stateRef :: TypedStateRef st) (fieldSpecs :: [FieldSpec]) (constraints :: [TypedConstraint])
```

**Parameters:**

- `stateRef`: How to find the state instance to update
- `fieldSpecs`: Which fields to modify and how
- `constraints`: Additional constraints for this operation

**Example:**

```haskell
-- Archive the currently active feed entry
'Op ('Update @FeedDataState
   ('TypedUniqueWhere ('FieldEquals "feedStatus" ('EnumValue FeedStatus "Active")))
   '[ 'Preserve "feedData"
    , 'SetTo "feedStatus" ('EnumValue FeedStatus "Archived")  
    ]
   '[])
```

### `Delete` - Remove State

Delete an existing state instance.

```haskell
'Delete (stateRef :: TypedStateRef st) (constraints :: [TypedConstraint])
```

**Example:**

```haskell
'Op ('Delete @UserState
   ('TypedUniqueWhere ('FieldEquals "userId" ('ParamValue "targetUser")))
   '[ 'MustBeSignedByState @AdminState 'TypedTheOnlyInstance "adminKey"
    ])
```

### `Reference` - Read-Only Access

Reference a state without modifying it (useful for constraints).

```haskell
'Reference (stateRef :: TypedStateRef st) (constraints :: [TypedConstraint])
```

## Field Specifications

Field specifications define how to set values in state instances.

### `SetTo` - Set Field Value

Set a field to a specific value.

```haskell
'SetTo (fieldName :: Symbol) (value :: TypedValue)
```

**Value Types:**

- `'ParamValue "paramName"`: Use a user-provided parameter
- `'EnumValue EnumType "VariantName"`: Use a specific enum variant
- `'IntValue n`: Use a type-level natural number
- `'StateFieldValue "label" "fieldName"`: Use a field value from a previous `'Let` operation
- `'CurrentTime`: Use the transaction's validity interval start time (POSIXTime). 
- `'AddValue v1 v2'`: The result of adding two `TypedValue`s. 
- `'SubtractValue v1 v2'`: The result of subtracting two `TypedValue`s. 
- `'MultiplyValue v1 v2'`: The result of multiplying two `TypedValue`s. 
- `'DivideValue v1 v2'`: The result of integer-dividing two `TypedValue`s.

**Examples:**

```haskell
-- Set from user parameter
'SetTo "userName" ('ParamValue "name")

-- Set to enum value
'SetTo "status" ('EnumValue UserStatus "Active")

-- Set to numeric literal
'SetTo "version" ('IntValue 1)

-- Reference a field from a previous Let operation
'SetTo "previousOwner" ('StateFieldValue "oldConfig" "owner")

-- Timestamp when the operation occurs
'SetTo "createdAt" 'CurrentTime

-- Use arithmetic to calculate a new value based on other states
'SetTo "customerSubscriptionPrice"
  ('SubtractValue
     ('StateFieldValue "selectedTier" "pricingTierPrice")
     ('DivideValue
        ('MultiplyValue
           ('StateFieldValue "selectedTier" "pricingTierPrice")
           ('StateFieldValue "selectedCoupon" "couponDiscountPercent")
        ) 
        ('IntValue 100)
     )
  )
```

### `Preserve` - Keep Current Value

Keep a field's current value unchanged (only valid in `Update` operations).

```haskell
'Preserve (fieldName :: Symbol)
```

**Example:**

```haskell
-- Update user email but keep the name unchanged
'Update @UserState userRef
  '[ 'Preserve "userName"
   , 'SetTo "userEmail" ('ParamValue "newEmail")
   ]
  '[]
```

## State References

State references specify which state instances an operation should target.

### `TypedTheOnlyInstance`

Reference the unique instance of a state type (only valid for states with `'OnlyAsUnique` reference strategy).

```haskell
'TypedTheOnlyInstance @s
```

**Example:**

```haskell
-- Reference the single feed configuration
'Update @FeedConfigState 'TypedTheOnlyInstance
  '[ 'SetTo "feedName" ('ParamValue "newName") ]
  '[]
```

### `TypedUniqueWhere`

Reference the unique instance that matches a specific condition.

```haskell
'TypedUniqueWhere (predicate :: TypedPredicate st)
```

**Example:**

```haskell
-- Find the currently active feed entry
'TypedUniqueWhere ('FieldEquals "feedStatus" ('EnumValue FeedStatus "Active"))
```

### `TypedAny`

References any single instance of the state type. The specific instance chosen might depend on the transaction building context or off-chain selection logic. Valid for states with `'AnyRef'` strategy.

```haskell
'TypedAny @s
```

### `TypedAnyWhere`

Reference any single instance that matches a specific condition (predicate). Similar to `TypedUniqueWhere`, but doesn't require the result to be unique. Valid for states with `'OnlyByProperty'` or `'AnyRef'` strategies.

```haskell
'TypedAnyWhere (predicate :: TypedPredicate st)
```

**Example:**

```haskell
-- Reference any available coupon (doesn't matter which one)
'TypedAnyWhere @CouponState ('FieldEquals "status" ('EnumValue CouponStatus "Available"))
```

### `TypedByLabel`

Reference a state instance that was previously labeled in a `'Let'` operation. This is useful for performing a sequence of operations on the *exact same* UTxO.

```hasekll
'TypedByLabel (label :: Symbol)
```

**Example:**

```haskell
-- First, find and label a coupon 
'Let "couponToBurn" ('Reference @CouponState ('TypedUniqueWhere ...))

-- Later, delete that exact coupon by its label 
'Op ('Delete @CouponState ('TypedByLabel "couponToBurn") '[])
```

## Collection Constraints

Collection constraints apply to `'Map` operations and validate properties across all items in a collection.

### `MustHaveUniqueField`

Ensures that a specific field has unique values across all items in a collection being mapped over.

```haskell
'MustHaveUniqueField (fieldName :: Symbol)
```

**Example:**

```haskell
'Map 
  ('Create @ItemState 
     '[ 'SetTo "itemId" ('ParamValue "id") 
      , 'SetTo "quantity" ('ParamValue "qty") 
      ] 
     '[]) 
  "items" 
  '[ 'MustHaveUniqueField "itemId" ]
```

## Predicates

### `FieldEquals`

Match instances where a field equals a specific value.

```haskell
'FieldEquals (fieldName :: Symbol) (value :: TypedValue)
```

**Examples:**

```haskell
-- Match active entries
'FieldEquals "status" ('EnumValue EntryStatus "Active")

-- Match by user parameter
'FieldEquals "ownerId" ('ParamValue "userId")
```

### Other Predicates

Additional predicates for complex matching (exact availability depends on implementation):

```haskell
-- Numeric comparisons
'FieldLessThan "amount" ('ParamValue "maxAmount")
'FieldGreaterThan "timestamp" ('ParamValue "minTime")

-- Boolean tests
'FieldTrue "isActive"
'FieldFalse "isArchived"
```

## Constraints

Constraints define validation rules that must be satisfied for the action to succeed.

### State-Based Constraints

#### `MustBeSignedByState`

Require a signature from an address stored in a state field.

```haskell
'MustBeSignedByState @s (stateRef :: TypedStateRef st) (fieldName :: Symbol)
```

**Example:**

```haskell
-- Must be signed by the feed owner
'MustBeSignedByState @FeedConfigState 'TypedTheOnlyInstance "feedOwner"
```

#### `MustExist` / `MustNotExist`

Require that a state instance exists or doesn't exist.

```haskell
'MustExist (stateRef :: TypedStateRef st)
'MustNotExist (stateRef :: TypedStateRef st)
```

**Example:**

```haskell
-- Ensure no user with this name already exists
'MustNotExist ('TypedUniqueWhere ('FieldEquals "userName" ('ParamValue "name")))
```

#### `PreserveStateField`

Ensure a field in a state remains unchanged across the transaction.

```haskell
'PreserveStateField (stateRef :: TypedStateRef st) (fieldPath :: TypedFieldPath record field fieldType)
```

#### `RequireStateValue`

Require a state field to have a specific value.

```haskell
'RequireStateValue (stateRef :: TypedStateRef st) (fieldPath :: TypedFieldPath record field fieldType) (value :: TypedValue)
```

### Parameter-Based Constraints

#### `MustBeSignedByParam`

Require signature from a parameter-provided key.

```haskell
'MustBeSignedByParam (paramName :: Symbol)
```

**Example:**

```haskell
-- Transaction must be signed by the user's key
'MustBeSignedByParam "userKey"
```

#### `MustSpendValidatorParam`

Require spending a UTxO parameter (typically for initialization).

```haskell
'MustSpendValidatorParam (validatorName :: Symbol) (paramName :: Symbol)
```

**Example:**

```haskell
-- Spend the bootstrap UTxO to initialize the application
'MustSpendValidatorParam "FeedValidator" "bootstrapUtxo"
```

#### `MustBeSignedByValidatorParam`

Requires the transaction to be signed by a key whose `PubKeyHash` is provided as a validator parameter.

```haskell
'MustBeSignedByValidatorParam (validatorName :: Symbol) (paramName :: Symbol)
```

#### `MustSpendParam` (Deprecated)

**Note:** This constraint is deprecated. Use `MustSpendActionParam` or `MustSpendValidatorParam` instead.

Requires the transaction to spend a specific UTxO provided as an action parameter ('Symbol').

```haskell
'MustSpendParam (paramName :: Symbol)
```

### Counting Constraints

#### `ExactlyN` / `AtLeastN` / `AtMostN`

Constrain the number of state instances.

```haskell
'ExactlyN (n :: Nat) (stateRef :: TypedStateRef st)
'AtLeastN (n :: Nat) (stateRef :: TypedStateRef st) 
'AtMostN (n :: Nat) (stateRef :: TypedStateRef st)
```

**Example:**

```haskell
-- Ensure exactly one active feed entry exists
'ExactlyN 1 ('TypedAllWhere ('FieldEquals "status" ('EnumValue FeedStatus "Active")))
```

### Payment and State Aggregation Constraints

These constraints handle payment and value aggregation, typically used for treasuries or aggregated state management.

#### `MustAddToAggregateState`

Requires the transaction to pay a specified value to the validator that manages an aggregate state (like a treasury).

```haskell
'MustAddToAggregateState (stateType :: Type) (value :: TypedValue)
```

**Example:**

```haskell
-- Requires the transaction to pay the price of a subscription tier into the treasury
'MustAddToAggregateState TreasuryAdaState ('StateFieldValue "selectedTier" "pricingTierPrice")
```

```haskell
'MustWithdrawFromAggregateState (stateType :: Type) (value :: TypedValue) (address :: TypedValue)
```

**Example:**

```haskell
-- Withdraws a specific amount from the treasury to an address provided as a parameter 
'MustWithdrawFromAggregateState TreasuryAdaState 
  ('ParamValue "amount")
  ('ParamValue "destination")
```

#### `MustWithdrawFromAggregateState`

Requires the transaction to withdraw a specified value from an aggregate state and send it to a destination address. The transaction must spend one or more UTxOs from the managing validator that collectively have sufficient funds.

#### `MustSpendActionParam`

Requires the transaction to spend a UTxO that is provided as an action parameter. This is useful for ensuring uniqueness or for "one-time-use" actions.

```haskell
'MustSpendActionParam (paramName :: Symbol)
```

**Example:**

```haskell
-- The transaction must spend the UTxO provided in the "batchIdUtxo" parameter 
'MustSpendActionParam "batchIdUtxo"
```

## Action Examples

### Simple Create Action

```haskell
type CreatePostSpec =
  'ActionSpec "CreatePost"
    '[ 'Op ('Create @BlogPostState
         '[ 'SetTo "title" ('ParamValue "postTitle")
          , 'SetTo "content" ('ParamValue "postContent")
          , 'SetTo "author" ('ParamValue "authorKey")
          , 'SetTo "status" ('EnumValue PostStatus "Draft")
          ]
         '[])
     ]
    '[ 'MustBeSignedByParam "authorKey"
     ]
    '[ '("postTitle", BuiltinByteString)
     , '("postContent", BuiltinByteString)
     , '("authorKey", PubKeyHash)
     ]
```

### Update with Archive Pattern

```haskell
type UpdateFeedSpec =
  'ActionSpec "UpdateFeed"
    '[ 'Op ('Create @FeedDataState
         '[ 'SetTo "feedData" ('ParamValue "newContent")
          , 'SetTo "feedStatus" ('EnumValue FeedStatus "Active")
          ]
         '[])
     , 'Op ('Update @FeedDataState
          ('TypedUniqueWhere ('FieldEquals "feedStatus" ('EnumValue FeedStatus "Active")))
          '[ 'Preserve "feedData"
           , 'SetTo "feedStatus" ('EnumValue FeedStatus "Archived")
           ]
          '[])
     ]
    '[ 'MustBeSignedByState @FeedConfigState 'TypedTheOnlyInstance "feedOwner"
     ]
    '[ '("newContent", BuiltinByteString)
     ]
```

### Batch Operations

```haskell
type BatchCreateCouponsSpec =
  'ActionSpec "BatchCreateCoupons"
    '[ 'Map
         ('Create @CouponState
            '[ 'SetTo "couponId" ('ParamValue "couponId")
             , 'SetTo "couponDiscountPercent" ('ParamValue "couponDiscountPercent")
             ]
            '[])
         "newCoupons"
     ]
    '[ 'MustBeSignedByState @ServiceConfigState 'TypedTheOnlyInstance "serviceConfigProvider"
     ]
    '[ '("newCoupons", [Coupon])
     ]
```

### Complex Multi-Operation Action

```haskell
type TransferOwnershipSpec =
  'ActionSpec "TransferOwnership" 
    '[ 'Op ('Update @FeedConfigState 'TypedTheOnlyInstance
         '[ 'Preserve "feedName"
          , 'SetTo "feedOwner" ('ParamValue "newOwner")
          ]
         '[])
     , 'Op ('Create @OwnershipLogState
         '[ 'SetTo "previousOwner" ('ParamValue "currentOwner")
          , 'SetTo "newOwner" ('ParamValue "newOwner")
          , 'SetTo "timestamp" ('ParamValue "transferTime")
          ]
         '[])
     ]
    '[ 'MustBeSignedByState @FeedConfigState 'TypedTheOnlyInstance "feedOwner"
     , 'MustBeSignedByParam "newOwner"
     ]
    '[ '("currentOwner", PubKeyHash)
     , '("newOwner", PubKeyHash)
     , '("transferTime", POSIXTime)
     ]
```

This action:

1. **Updates** the feed config to change the owner
2. **Creates** a log entry recording the ownership transfer
3. **Requires** signatures from both current and new owners
4. **Takes** three parameters: current owner, new owner, and timestamp

## Type Safety

The action specification system provides extensive compile-time validation:

- **Field references** are checked against state type definitions
- **Parameter usage** must match declared parameter types
- **State references** must be valid for the target state type
- **Constraint targets** must reference existing states or parameters
- **Operation compatibility** ensures operations make sense for their targets

This prevents entire categories of runtime errors and ensures your specifications are internally consistent.
