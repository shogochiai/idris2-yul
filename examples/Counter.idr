||| Simple Counter Contract
||| Demonstrates basic storage and function dispatch
module Main

-- =============================================================================
-- EVM Primitives (FFI)
-- =============================================================================

-- For EVM, we use Integer to represent 256-bit values
-- The EVM backend handles the representation

%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

%foreign "evm:sstore"
prim__sstore : Integer -> Integer -> PrimIO ()

%foreign "evm:caller"
prim__caller : PrimIO Integer

%foreign "evm:calldataload"
prim__calldataload : Integer -> PrimIO Integer

%foreign "evm:calldatasize"
prim__calldatasize : PrimIO Integer

%foreign "evm:return"
prim__return : Integer -> Integer -> PrimIO ()

%foreign "evm:revert"
prim__revert : Integer -> Integer -> PrimIO ()

%foreign "evm:mstore"
prim__mstore : Integer -> Integer -> PrimIO ()

-- =============================================================================
-- Wrapped Primitives
-- =============================================================================

sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)

sstore : Integer -> Integer -> IO ()
sstore slot val = primIO (prim__sstore slot val)

calldataload : Integer -> IO Integer
calldataload off = primIO (prim__calldataload off)

evmReturn : Integer -> Integer -> IO ()
evmReturn off len = primIO (prim__return off len)

evmRevert : Integer -> Integer -> IO ()
evmRevert off len = primIO (prim__revert off len)

mstore : Integer -> Integer -> IO ()
mstore off val = primIO (prim__mstore off val)

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for counter value
COUNTER_SLOT : Integer
COUNTER_SLOT = 0

-- =============================================================================
-- Contract Logic
-- =============================================================================

||| Get current counter value
getCount : IO Integer
getCount = sload COUNTER_SLOT

||| Increment counter by 1
increment : IO ()
increment = do
  current <- sload COUNTER_SLOT
  sstore COUNTER_SLOT (current + 1)

||| Decrement counter by 1
decrement : IO ()
decrement = do
  current <- sload COUNTER_SLOT
  if current > 0
    then sstore COUNTER_SLOT (current - 1)
    else pure ()

||| Set counter to specific value
setCount : Integer -> IO ()
setCount val = sstore COUNTER_SLOT val

-- =============================================================================
-- Function Selectors (keccak256 first 4 bytes)
-- =============================================================================

-- getCount() -> 0xa87d942c
SEL_GET_COUNT : Integer
SEL_GET_COUNT = 0xa87d942c

-- increment() -> 0xd09de08a
SEL_INCREMENT : Integer
SEL_INCREMENT = 0xd09de08a

-- decrement() -> 0x2baeceb7
SEL_DECREMENT : Integer
SEL_DECREMENT = 0x2baeceb7

-- setCount(uint256) -> 0xd14e62b8
SEL_SET_COUNT : Integer
SEL_SET_COUNT = 0xd14e62b8

-- =============================================================================
-- Entry Point (Dispatcher)
-- =============================================================================

||| Extract function selector from calldata
getSelector : IO Integer
getSelector = do
  data_ <- calldataload 0
  pure (data_ `div` (256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256 * 256 * 256 * 256 * 256 *
                     256 * 256 * 256 * 256))  -- shift right 224 bits

||| Return uint256 value
returnUint256 : Integer -> IO ()
returnUint256 val = do
  mstore 0 val
  evmReturn 0 32

||| Main dispatch function
main : IO ()
main = do
  selector <- getSelector
  if selector == SEL_GET_COUNT
    then do
      count <- getCount
      returnUint256 count
    else if selector == SEL_INCREMENT
    then increment
    else if selector == SEL_DECREMENT
    then decrement
    else if selector == SEL_SET_COUNT
    then do
      arg <- calldataload 4
      setCount arg
    else evmRevert 0 0  -- Unknown function
