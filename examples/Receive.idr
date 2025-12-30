||| MC Standard Function: Receive
||| Idris2 port of metacontract/mc src/std/functions/Receive.sol
|||
||| Emits Received event when ETH is sent to the contract
module Main

-- =============================================================================
-- EVM Primitives (FFI)
-- =============================================================================

%foreign "evm:caller"
prim__caller : PrimIO Integer

%foreign "evm:callvalue"
prim__callvalue : PrimIO Integer

%foreign "evm:calldatasize"
prim__calldatasize : PrimIO Integer

%foreign "evm:mstore"
prim__mstore : Integer -> Integer -> PrimIO ()

%foreign "evm:log2"
prim__log2 : Integer -> Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:stop"
prim__stop : PrimIO ()

-- =============================================================================
-- Wrapped Primitives
-- =============================================================================

caller : IO Integer
caller = primIO prim__caller

callvalue : IO Integer
callvalue = primIO prim__callvalue

calldatasize : IO Integer
calldatasize = primIO prim__calldatasize

mstore : Integer -> Integer -> IO ()
mstore off val = primIO (prim__mstore off val)

||| LOG2: Emit event with 2 indexed topics
||| log2(offset, size, topic1, topic2)
log2 : Integer -> Integer -> Integer -> Integer -> IO ()
log2 off size topic1 topic2 = primIO (prim__log2 off size topic1 topic2)

stop : IO ()
stop = primIO prim__stop

-- =============================================================================
-- Event Signature
-- =============================================================================

||| Event: Received(address indexed from, uint256 amount)
||| Signature: keccak256("Received(address,uint256)")
||| = 0x88a5966d370b9919b20f3e2c13ff65706f196a4e32cc2c12bf57088f88525874
EVENT_RECEIVED : Integer
EVENT_RECEIVED = 0x88a5966d370b9919b20f3e2c13ff65706f196a4e32cc2c12bf57088f88525874

-- =============================================================================
-- Event Emission
-- =============================================================================

||| Emit Received event
||| Topics: [EVENT_RECEIVED, from (indexed)]
||| Data: amount (non-indexed, stored in memory)
emitReceived : Integer -> Integer -> IO ()
emitReceived from amount = do
  -- Store amount in memory at offset 0
  mstore 0 amount
  -- log2(offset, size, topic1, topic2)
  -- topic1 = event signature
  -- topic2 = indexed 'from' address
  log2 0 32 EVENT_RECEIVED from

-- =============================================================================
-- Receive Function (fallback for ETH transfers)
-- =============================================================================

||| Handle incoming ETH transfers
||| Equivalent to Solidity's receive() external payable
receive : IO ()
receive = do
  from <- caller
  amount <- callvalue
  emitReceived from amount

-- =============================================================================
-- Entry Point
-- =============================================================================

||| Main dispatcher
||| If calldata is empty, treat as receive()
main : IO ()
main = do
  size <- calldatasize
  if size == 0
    then do
      receive
      stop
    else stop  -- Unknown function, just stop
