||| TextDAO Function: Members
||| Member and Text management for TextDAO
|||
||| Manages member registry and text storage
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

%foreign "evm:log2"
prim__log2 : Integer -> Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:log3"
prim__log3 : Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO ()

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

log2 : Integer -> Integer -> Integer -> Integer -> IO ()
log2 off size topic1 topic2 = primIO (prim__log2 off size topic1 topic2)

log3 : Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
log3 off size topic1 topic2 topic3 = primIO (prim__log3 off size topic1 topic2 topic3)

-- =============================================================================
-- Storage Layout
-- =============================================================================

SLOT_MEMBERS : Integer
SLOT_MEMBERS = 0x3000

SLOT_MEMBER_COUNT : Integer
SLOT_MEMBER_COUNT = 0x3001

SLOT_TEXTS : Integer
SLOT_TEXTS = 0x2000

SLOT_TEXT_COUNT : Integer
SLOT_TEXT_COUNT = 0x2001

SLOT_ADMINS : Integer
SLOT_ADMINS = 0x5000

SLOT_ADMIN_COUNT : Integer
SLOT_ADMIN_COUNT = 0x5001

-- Member struct offsets
MEMBER_OFFSET_ADDRESS : Integer
MEMBER_OFFSET_ADDRESS = 0

MEMBER_OFFSET_METADATA : Integer
MEMBER_OFFSET_METADATA = 1

-- Text struct offsets
TEXT_OFFSET_METADATA : Integer
TEXT_OFFSET_METADATA = 0

-- =============================================================================
-- Event Signatures
-- =============================================================================

||| MemberAdded(address indexed member, uint256 indexed index)
EVENT_MEMBER_ADDED : Integer
EVENT_MEMBER_ADDED = 0xef5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8ee

||| MemberRemoved(address indexed member, uint256 indexed index)
EVENT_MEMBER_REMOVED : Integer
EVENT_MEMBER_REMOVED = 0xff5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8ef

||| TextCreated(uint256 indexed textId, bytes32 metadataCid)
EVENT_TEXT_CREATED : Integer
EVENT_TEXT_CREATED = 0x0f5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8f0

-- =============================================================================
-- Storage Helpers
-- =============================================================================

getMemberSlot : Integer -> IO Integer
getMemberSlot index = do
  mstore 0 index
  mstore 32 SLOT_MEMBERS
  keccak256 0 64

getTextSlot : Integer -> IO Integer
getTextSlot index = do
  mstore 0 index
  mstore 32 SLOT_TEXTS
  keccak256 0 64

getAdminSlot : Integer -> IO Integer
getAdminSlot index = do
  mstore 0 index
  mstore 32 SLOT_ADMINS
  keccak256 0 64

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if address is an admin
isAdmin : Integer -> IO Bool
isAdmin addr = do
  adminCount <- sload SLOT_ADMIN_COUNT
  checkAdmin addr 0 adminCount
  where
    checkAdmin : Integer -> Integer -> Integer -> IO Bool
    checkAdmin addr idx count =
      if idx >= count
        then pure False
        else do
          adminSlot <- getAdminSlot idx
          adminAddr <- sload adminSlot
          if adminAddr == addr
            then pure True
            else checkAdmin addr (idx + 1) count

||| Require caller to be admin
requireAdmin : IO ()
requireAdmin = do
  callerAddr <- caller
  admin <- isAdmin callerAddr
  if admin
    then pure ()
    else evmRevert 0 0

||| Check if address is a member
isMember : Integer -> IO Bool
isMember addr = do
  memberCount <- sload SLOT_MEMBER_COUNT
  checkMember addr 0 memberCount
  where
    checkMember : Integer -> Integer -> Integer -> IO Bool
    checkMember addr idx count =
      if idx >= count
        then pure False
        else do
          memberSlot <- getMemberSlot idx
          memberAddr <- sload (memberSlot + MEMBER_OFFSET_ADDRESS)
          if memberAddr == addr
            then pure True
            else checkMember addr (idx + 1) count

-- =============================================================================
-- Member Management
-- =============================================================================

||| Add a new member
||| Only admin can call
addMember : Integer -> Integer -> IO Integer
addMember memberAddr metadataCid = do
  requireAdmin
  -- Check not already a member
  alreadyMember <- isMember memberAddr
  if alreadyMember
    then do
      evmRevert 0 0
      pure 0
    else do
      -- Get next index
      memberCount <- sload SLOT_MEMBER_COUNT
      -- Store member
      memberSlot <- getMemberSlot memberCount
      sstore (memberSlot + MEMBER_OFFSET_ADDRESS) memberAddr
      sstore (memberSlot + MEMBER_OFFSET_METADATA) metadataCid
      -- Increment count
      sstore SLOT_MEMBER_COUNT (memberCount + 1)
      -- Emit event
      log3 0 0 EVENT_MEMBER_ADDED memberAddr memberCount
      pure memberCount

||| Get member address by index
getMember : Integer -> IO Integer
getMember index = do
  memberSlot <- getMemberSlot index
  sload (memberSlot + MEMBER_OFFSET_ADDRESS)

||| Get member metadata by index
getMemberMetadata : Integer -> IO Integer
getMemberMetadata index = do
  memberSlot <- getMemberSlot index
  sload (memberSlot + MEMBER_OFFSET_METADATA)

-- =============================================================================
-- Text Management
-- =============================================================================

||| Create a new text entry
||| Only members can create texts
createText : Integer -> IO Integer
createText metadataCid = do
  -- Check caller is member
  callerAddr <- caller
  member <- isMember callerAddr
  if not member
    then do
      evmRevert 0 0
      pure 0
    else do
      -- Get next text ID
      textCount <- sload SLOT_TEXT_COUNT
      -- Store text metadata
      textSlot <- getTextSlot textCount
      sstore (textSlot + TEXT_OFFSET_METADATA) metadataCid
      -- Increment count
      sstore SLOT_TEXT_COUNT (textCount + 1)
      -- Emit event
      mstore 0 metadataCid
      log2 0 32 EVENT_TEXT_CREATED textCount
      pure textCount

||| Get text metadata by ID
getText : Integer -> IO Integer
getText textId = do
  textSlot <- getTextSlot textId
  sload (textSlot + TEXT_OFFSET_METADATA)

-- =============================================================================
-- Initialize (for initial admin setup)
-- =============================================================================

||| Initialize the contract with first admin
||| Can only be called once (when admin count is 0)
initialize : Integer -> IO ()
initialize firstAdmin = do
  adminCount <- sload SLOT_ADMIN_COUNT
  if adminCount > 0
    then evmRevert 0 0  -- Already initialized
    else do
      adminSlot <- getAdminSlot 0
      sstore adminSlot firstAdmin
      sstore SLOT_ADMIN_COUNT 1

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- addMember(address,bytes32)
SEL_ADD_MEMBER : Integer
SEL_ADD_MEMBER = 0xca6d56dc

-- getMember(uint256) returns (address)
SEL_GET_MEMBER : Integer
SEL_GET_MEMBER = 0x9c0a0cd2

-- getMemberCount() returns (uint256)
SEL_GET_MEMBER_COUNT : Integer
SEL_GET_MEMBER_COUNT = 0x997072f7

-- isMember(address) returns (bool)
SEL_IS_MEMBER : Integer
SEL_IS_MEMBER = 0xa230c524

-- createText(bytes32) returns (uint256)
SEL_CREATE_TEXT : Integer
SEL_CREATE_TEXT = 0xa1b2c3d4

-- getText(uint256) returns (bytes32)
SEL_GET_TEXT : Integer
SEL_GET_TEXT = 0xb2c3d4e5

-- getTextCount() returns (uint256)
SEL_GET_TEXT_COUNT : Integer
SEL_GET_TEXT_COUNT = 0xc3d4e5f6

-- initialize(address)
SEL_INITIALIZE : Integer
SEL_INITIALIZE = 0xc4d66de8

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

returnUint : Integer -> IO ()
returnUint val = do
  mstore 0 val
  evmReturn 0 32

returnBool : Bool -> IO ()
returnBool b = returnUint (if b then 1 else 0)

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_ADD_MEMBER
    then do
      memberAddr <- calldataload 4
      metadataCid <- calldataload 36
      index <- addMember memberAddr metadataCid
      returnUint index

    else if selector == SEL_GET_MEMBER
    then do
      index <- calldataload 4
      addr <- getMember index
      returnUint addr

    else if selector == SEL_GET_MEMBER_COUNT
    then do
      count <- sload SLOT_MEMBER_COUNT
      returnUint count

    else if selector == SEL_IS_MEMBER
    then do
      addr <- calldataload 4
      member <- isMember addr
      returnBool member

    else if selector == SEL_CREATE_TEXT
    then do
      metadataCid <- calldataload 4
      textId <- createText metadataCid
      returnUint textId

    else if selector == SEL_GET_TEXT
    then do
      textId <- calldataload 4
      metadata <- getText textId
      returnUint metadata

    else if selector == SEL_GET_TEXT_COUNT
    then do
      count <- sload SLOT_TEXT_COUNT
      returnUint count

    else if selector == SEL_INITIALIZE
    then do
      firstAdmin <- calldataload 4
      initialize firstAdmin
      evmReturn 0 0

    else evmRevert 0 0
