||| MC Standard Function: FeatureToggle
||| Idris2 port of metacontract/mc src/std/functions/protected/FeatureToggle.sol
|||
||| Allows admin to enable/disable function selectors
module Main

-- =============================================================================
-- EVM Primitives (FFI)
-- =============================================================================

%foreign "evm:caller"
prim__caller : PrimIO Integer

%foreign "evm:calldataload"
prim__calldataload : Integer -> PrimIO Integer

%foreign "evm:calldatasize"
prim__calldatasize : PrimIO Integer

%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

%foreign "evm:sstore"
prim__sstore : Integer -> Integer -> PrimIO ()

%foreign "evm:mstore"
prim__mstore : Integer -> Integer -> PrimIO ()

%foreign "evm:return"
prim__return : Integer -> Integer -> PrimIO ()

%foreign "evm:revert"
prim__revert : Integer -> Integer -> PrimIO ()

%foreign "evm:keccak256"
prim__keccak256 : Integer -> Integer -> PrimIO Integer

-- =============================================================================
-- Wrapped Primitives
-- =============================================================================

caller : IO Integer
caller = primIO prim__caller

calldataload : Integer -> IO Integer
calldataload off = primIO (prim__calldataload off)

calldatasize : IO Integer
calldatasize = primIO prim__calldatasize

sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)

sstore : Integer -> Integer -> IO ()
sstore slot val = primIO (prim__sstore slot val)

mstore : Integer -> Integer -> IO ()
mstore off val = primIO (prim__mstore off val)

evmReturn : Integer -> Integer -> IO ()
evmReturn off len = primIO (prim__return off len)

evmRevert : Integer -> Integer -> IO ()
evmRevert off len = primIO (prim__revert off len)

keccak256 : Integer -> Integer -> IO Integer
keccak256 off len = primIO (prim__keccak256 off len)

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for admin address
||| Using ERC-7201 namespaced storage pattern
||| keccak256("mc.std.admin") - 1
SLOT_ADMIN : Integer
SLOT_ADMIN = 0

||| Base slot for feature toggle mapping
||| Actual slot = keccak256(selector . SLOT_FEATURE_TOGGLE_BASE)
||| Original Solidity: 0xfbe5942bf8b77a2e1fdda5ac4fad2514a8894a997001808038d8cb6785c1d500
SLOT_FEATURE_TOGGLE_BASE : Integer
SLOT_FEATURE_TOGGLE_BASE = 0xfbe5942bf8b77a2e1fdda5ac4fad2514a8894a997001808038d8cb6785c1d500

-- =============================================================================
-- Storage Helpers
-- =============================================================================

||| Calculate storage slot for a feature's disabled status
||| slot = keccak256(abi.encode(selector, baseSlot))
getFeatureSlot : Integer -> IO Integer
getFeatureSlot selector = do
  -- Store selector at memory 0
  mstore 0 selector
  -- Store base slot at memory 32
  mstore 32 SLOT_FEATURE_TOGGLE_BASE
  -- keccak256(0, 64) = hash of (selector, baseSlot)
  keccak256 0 64

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if caller is admin
isAdmin : IO Bool
isAdmin = do
  admin <- sload SLOT_ADMIN
  callerAddr <- caller
  pure (admin == callerAddr)

||| Revert if caller is not admin
requireAdmin : IO ()
requireAdmin = do
  adminCheck <- isAdmin
  if adminCheck
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- FeatureToggle Functions
-- =============================================================================

||| Check if a feature (selector) is disabled
||| Returns True if disabled, False if enabled
isFeatureDisabled : Integer -> IO Bool
isFeatureDisabled selector = do
  slot <- getFeatureSlot selector
  val <- sload slot
  pure (val == 1)

||| Check if feature should be active, revert if disabled
||| Library function: FeatureToggle.shouldBeActive(bytes4)
shouldBeActive : Integer -> IO ()
shouldBeActive selector = do
  disabled <- isFeatureDisabled selector
  if disabled
    then evmRevert 0 0  -- FeatureNotActive error
    else pure ()

||| Toggle a feature's enabled/disabled status
||| Only admin can call
||| Contract function: featureToggle(bytes4)
featureToggle : Integer -> IO ()
featureToggle selector = do
  requireAdmin
  slot <- getFeatureSlot selector
  currentVal <- sload slot
  -- Toggle: 0 -> 1 (disable), 1 -> 0 (enable)
  let newVal = if currentVal == 0 then 1 else 0
  sstore slot newVal

||| Get admin address
getAdmin : IO Integer
getAdmin = sload SLOT_ADMIN

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- featureToggle(bytes4) -> 0x...
-- keccak256("featureToggle(bytes4)") first 4 bytes
SEL_FEATURE_TOGGLE : Integer
SEL_FEATURE_TOGGLE = 0xa7e24c12

-- admin() -> 0x...
SEL_ADMIN : Integer
SEL_ADMIN = 0xf851a440

-- isDisabled(bytes4) -> 0x...
SEL_IS_DISABLED : Integer
SEL_IS_DISABLED = 0x108a27a5

-- =============================================================================
-- Entry Point
-- =============================================================================

getSelector : IO Integer
getSelector = do
  data_ <- calldataload 0
  pure (data_ `div` (256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256))

returnValue : Integer -> IO ()
returnValue val = do
  mstore 0 val
  evmReturn 0 32

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_FEATURE_TOGGLE
    then do
      -- featureToggle(bytes4 selector)
      funcSelector <- calldataload 4
      -- Mask to 4 bytes (bytes4)
      let funcSelector4 = funcSelector `div` (256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256)
      featureToggle funcSelector4

    else if selector == SEL_ADMIN
    then do
      -- admin() view
      admin <- getAdmin
      returnValue admin

    else if selector == SEL_IS_DISABLED
    then do
      -- isDisabled(bytes4) view
      funcSelector <- calldataload 4
      let funcSelector4 = funcSelector `div` (256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256 *
                                              256 * 256 * 256 * 256)
      disabled <- isFeatureDisabled funcSelector4
      returnValue (if disabled then 1 else 0)

    else evmRevert 0 0
