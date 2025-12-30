||| MC Core: Minimal Proxy Contract
||| Idris2 implementation of EIP-1167 minimal proxy pattern
|||
||| Forwards all calls to a dictionary (implementation) contract via DELEGATECALL
module Main

-- =============================================================================
-- EVM Primitives (FFI)
-- =============================================================================

%foreign "evm:calldatasize"
prim__calldatasize : PrimIO Integer

%foreign "evm:calldatacopy"
prim__calldatacopy : Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:delegatecall"
prim__delegatecall : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO Integer

%foreign "evm:returndatasize"
prim__returndatasize : PrimIO Integer

%foreign "evm:returndatacopy"
prim__returndatacopy : Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:return"
prim__return : Integer -> Integer -> PrimIO ()

%foreign "evm:revert"
prim__revert : Integer -> Integer -> PrimIO ()

%foreign "evm:gas"
prim__gas : PrimIO Integer

%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

-- =============================================================================
-- Wrapped Primitives
-- =============================================================================

calldatasize : IO Integer
calldatasize = primIO prim__calldatasize

calldatacopy : Integer -> Integer -> Integer -> IO ()
calldatacopy destOff srcOff len = primIO (prim__calldatacopy destOff srcOff len)

||| DELEGATECALL: Call another contract in this contract's context
||| delegatecall(gas, address, argsOffset, argsSize, retOffset, retSize) -> success
delegatecall : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
delegatecall g addr argsOff argsSize retOff retSize =
  primIO (prim__delegatecall g addr argsOff argsSize retOff retSize)

returndatasize : IO Integer
returndatasize = primIO prim__returndatasize

returndatacopy : Integer -> Integer -> Integer -> IO ()
returndatacopy destOff srcOff len = primIO (prim__returndatacopy destOff srcOff len)

evmReturn : Integer -> Integer -> IO ()
evmReturn off len = primIO (prim__return off len)

evmRevert : Integer -> Integer -> IO ()
evmRevert off len = primIO (prim__revert off len)

gas : IO Integer
gas = primIO prim__gas

sload : Integer -> IO Integer
sload slot = primIO (prim__sload slot)

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Storage slot for dictionary (implementation) address
||| Using EIP-1967 style slot: keccak256("eip1967.proxy.implementation") - 1
||| For simplicity, we use slot 0
DICTIONARY_SLOT : Integer
DICTIONARY_SLOT = 0

-- =============================================================================
-- Proxy Logic
-- =============================================================================

||| Forward call to dictionary via DELEGATECALL
||| 1. Copy calldata to memory
||| 2. DELEGATECALL to dictionary with all gas
||| 3. Copy return data to memory
||| 4. Return or revert based on success
forwardCall : IO ()
forwardCall = do
  -- Get dictionary address from storage
  dictionary <- sload DICTIONARY_SLOT

  -- Get calldata size
  cdSize <- calldatasize

  -- Copy calldata to memory at offset 0
  calldatacopy 0 0 cdSize

  -- Get available gas (minus some overhead)
  availableGas <- gas

  -- DELEGATECALL to dictionary
  -- delegatecall(gas, address, argsOffset, argsSize, retOffset, retSize)
  -- We don't pre-allocate return buffer, will use returndatacopy
  success <- delegatecall availableGas dictionary 0 cdSize 0 0

  -- Get return data size
  rdSize <- returndatasize

  -- Copy return data to memory at offset 0
  returndatacopy 0 0 rdSize

  -- Return or revert based on success
  if success == 1
    then evmReturn 0 rdSize
    else evmRevert 0 rdSize

-- =============================================================================
-- Entry Point
-- =============================================================================

main : IO ()
main = forwardCall
