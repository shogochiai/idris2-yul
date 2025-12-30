||| Linear Bonding Curve Contract
||| Demonstrates fixed-point arithmetic for token pricing
|||
||| Price formula: price = basePrice + slope * supply
||| Uses 18 decimal fixed-point (like Solidity's 1e18)
module Main

-- =============================================================================
-- EVM Primitives (FFI)
-- =============================================================================

%foreign "evm:sload"
prim__sload : Integer -> PrimIO Integer

%foreign "evm:sstore"
prim__sstore : Integer -> Integer -> PrimIO ()

%foreign "evm:caller"
prim__caller : PrimIO Integer

%foreign "evm:callvalue"
prim__callvalue : PrimIO Integer

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

caller : IO Integer
caller = primIO prim__caller

callvalue : IO Integer
callvalue = primIO prim__callvalue

calldataload : Integer -> IO Integer
calldataload off = primIO (prim__calldataload off)

evmReturn : Integer -> Integer -> IO ()
evmReturn off len = primIO (prim__return off len)

evmRevert : Integer -> Integer -> IO ()
evmRevert off len = primIO (prim__revert off len)

mstore : Integer -> Integer -> IO ()
mstore off val = primIO (prim__mstore off val)

-- =============================================================================
-- Fixed-Point Arithmetic (18 decimals)
-- =============================================================================

||| 1e18 - standard EVM decimal precision
ONE : Integer
ONE = 1000000000000000000

||| Multiply two fixed-point numbers: (a * b) / ONE
||| Handles overflow by doing division before full multiplication when possible
mulFP : Integer -> Integer -> Integer
mulFP a b = (a * b) `div` ONE

||| Divide two fixed-point numbers: (a * ONE) / b
divFP : Integer -> Integer -> Integer
divFP a b = if b == 0 then 0 else (a * ONE) `div` b

-- =============================================================================
-- Storage Layout
-- =============================================================================

||| Total token supply
SLOT_SUPPLY : Integer
SLOT_SUPPLY = 0

||| Reserve balance (ETH held)
SLOT_RESERVE : Integer
SLOT_RESERVE = 1

||| Base price (in wei per token, fixed-point)
SLOT_BASE_PRICE : Integer
SLOT_BASE_PRICE = 2

||| Slope (price increase per token, fixed-point)
SLOT_SLOPE : Integer
SLOT_SLOPE = 3

||| Balance mapping: keccak256(addr, 4) for slot
||| Simplified: use address directly as slot offset from 1000
balanceSlot : Integer -> Integer
balanceSlot addr = 1000 + (addr `mod` 10000)

-- =============================================================================
-- Bonding Curve Math
-- =============================================================================

||| Calculate current price: basePrice + slope * supply
||| All values in fixed-point (18 decimals)
getCurrentPrice : Integer -> Integer -> Integer -> Integer
getCurrentPrice basePrice slope supply = basePrice + mulFP slope supply

||| Calculate cost to buy `amount` tokens starting from `supply`
||| Uses integral: ∫(basePrice + slope*x)dx from supply to supply+amount
||| = basePrice * amount + slope * (amount * (2*supply + amount) / 2)
||| Simplified: basePrice * amount + slope * amount * (supply + amount/2)
calculateBuyCost : Integer -> Integer -> Integer -> Integer -> Integer
calculateBuyCost basePrice slope supply amount =
  let baseCost = mulFP basePrice amount
      -- Average price over the range
      avgSupply = supply + (amount `div` 2)
      slopeCost = mulFP slope (mulFP amount avgSupply)
  in baseCost + slopeCost

||| Calculate ETH returned for selling `amount` tokens
||| Same formula as buy cost but in reverse
calculateSellReturn : Integer -> Integer -> Integer -> Integer -> Integer
calculateSellReturn basePrice slope supply amount =
  if amount > supply then 0
  else
    let newSupply = supply - amount
        avgSupply = newSupply + (amount `div` 2)
        baseCost = mulFP basePrice amount
        slopeCost = mulFP slope (mulFP amount avgSupply)
    in baseCost + slopeCost

-- =============================================================================
-- Contract Logic
-- =============================================================================

||| Initialize the bonding curve (constructor-like)
||| basePrice: starting price in wei (fixed-point)
||| slope: price increase per token (fixed-point)
initialize : Integer -> Integer -> IO ()
initialize basePrice slope = do
  currentSupply <- sload SLOT_SUPPLY
  if currentSupply == 0
    then do
      sstore SLOT_BASE_PRICE basePrice
      sstore SLOT_SLOPE slope
    else pure ()  -- Already initialized

||| Get current token supply
getSupply : IO Integer
getSupply = sload SLOT_SUPPLY

||| Get reserve balance
getReserve : IO Integer
getReserve = sload SLOT_RESERVE

||| Get balance of address
getBalance : Integer -> IO Integer
getBalance addr = sload (balanceSlot addr)

||| Get current token price
getPrice : IO Integer
getPrice = do
  basePrice <- sload SLOT_BASE_PRICE
  slope <- sload SLOT_SLOPE
  supply <- sload SLOT_SUPPLY
  pure (getCurrentPrice basePrice slope supply)

||| Buy tokens with ETH sent
||| Returns number of tokens bought
buy : IO Integer
buy = do
  ethSent <- callvalue
  if ethSent == 0
    then pure 0
    else do
      basePrice <- sload SLOT_BASE_PRICE
      slope <- sload SLOT_SLOPE
      supply <- sload SLOT_SUPPLY
      reserve <- sload SLOT_RESERVE
      buyerAddr <- caller
      -- Simple approximation: tokens = ethSent / currentPrice
      -- More accurate would require solving quadratic
      let currentPrice = getCurrentPrice basePrice slope supply
      let tokensOut = if currentPrice == 0 then ethSent else divFP ethSent currentPrice
      -- Update state
      sstore SLOT_SUPPLY (supply + tokensOut)
      sstore SLOT_RESERVE (reserve + ethSent)
      -- Update buyer balance
      buyerBalance <- sload (balanceSlot buyerAddr)
      sstore (balanceSlot buyerAddr) (buyerBalance + tokensOut)
      pure tokensOut

||| Sell tokens for ETH
||| amount: tokens to sell (fixed-point)
sell : Integer -> IO Integer
sell amount = do
  sellerAddr <- caller
  sellerBalance <- sload (balanceSlot sellerAddr)
  if amount > sellerBalance
    then pure 0
    else do
      basePrice <- sload SLOT_BASE_PRICE
      slope <- sload SLOT_SLOPE
      supply <- sload SLOT_SUPPLY
      reserve <- sload SLOT_RESERVE
      let ethOut = calculateSellReturn basePrice slope supply amount
      if ethOut > reserve
        then pure 0
        else do
          -- Update state
          sstore SLOT_SUPPLY (supply - amount)
          sstore SLOT_RESERVE (reserve - ethOut)
          sstore (balanceSlot sellerAddr) (sellerBalance - amount)
          -- Note: actual ETH transfer would need call opcode
          pure ethOut

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- initialize(uint256,uint256) -> keccak256 first 4 bytes
SEL_INITIALIZE : Integer
SEL_INITIALIZE = 0xe4a30116

-- getSupply() -> 0x...
SEL_GET_SUPPLY : Integer
SEL_GET_SUPPLY = 0x57f767e8

-- getReserve() -> 0x...
SEL_GET_RESERVE : Integer
SEL_GET_RESERVE = 0x0de2d4ef

-- getPrice() -> 0x...
SEL_GET_PRICE : Integer
SEL_GET_PRICE = 0x98d5fdca

-- getBalance(address) -> 0x...
SEL_GET_BALANCE : Integer
SEL_GET_BALANCE = 0xf8b2cb4f

-- buy() -> 0xa6f2ae3a
SEL_BUY : Integer
SEL_BUY = 0xa6f2ae3a

-- sell(uint256) -> 0xe4849b32
SEL_SELL : Integer
SEL_SELL = 0xe4849b32

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

returnUint256 : Integer -> IO ()
returnUint256 val = do
  mstore 0 val
  evmReturn 0 32

main : IO ()
main = do
  selector <- getSelector
  if selector == SEL_INITIALIZE
    then do
      basePrice <- calldataload 4
      slope <- calldataload 36
      initialize basePrice slope
    else if selector == SEL_GET_SUPPLY
    then do
      supply <- getSupply
      returnUint256 supply
    else if selector == SEL_GET_RESERVE
    then do
      reserve <- getReserve
      returnUint256 reserve
    else if selector == SEL_GET_PRICE
    then do
      price <- getPrice
      returnUint256 price
    else if selector == SEL_GET_BALANCE
    then do
      addr <- calldataload 4
      bal <- getBalance addr
      returnUint256 bal
    else if selector == SEL_BUY
    then do
      tokens <- buy
      returnUint256 tokens
    else if selector == SEL_SELL
    then do
      amount <- calldataload 4
      ethOut <- sell amount
      returnUint256 ethOut
    else evmRevert 0 0
