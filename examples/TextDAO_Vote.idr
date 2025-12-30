||| TextDAO Function: Vote
||| Idris2 port of textdao-monorepo/.../functions/onlyReps/Vote.sol
|||
||| Allows representatives to cast ranked-choice votes on proposals
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

%foreign "evm:log3"
prim__log3 : Integer -> Integer -> Integer -> Integer -> Integer -> PrimIO ()

%foreign "evm:timestamp"
prim__timestamp : PrimIO Integer

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

log3 : Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
log3 off size topic1 topic2 topic3 = primIO (prim__log3 off size topic1 topic2 topic3)

timestamp : IO Integer
timestamp = primIO prim__timestamp

-- =============================================================================
-- Storage Layout
-- =============================================================================

SLOT_DELIBERATION : Integer
SLOT_DELIBERATION = 0x1000

-- Meta field offsets
META_OFFSET_EXPIRATION : Integer
META_OFFSET_EXPIRATION = 1

META_OFFSET_HEADER_COUNT : Integer
META_OFFSET_HEADER_COUNT = 3

META_OFFSET_CMD_COUNT : Integer
META_OFFSET_CMD_COUNT = 4

-- Vote storage: 6 ranked IDs (3 header, 3 command)
VOTE_OFFSET_HEADER_RANK_0 : Integer
VOTE_OFFSET_HEADER_RANK_0 = 0

VOTE_OFFSET_HEADER_RANK_1 : Integer
VOTE_OFFSET_HEADER_RANK_1 = 1

VOTE_OFFSET_HEADER_RANK_2 : Integer
VOTE_OFFSET_HEADER_RANK_2 = 2

VOTE_OFFSET_CMD_RANK_0 : Integer
VOTE_OFFSET_CMD_RANK_0 = 3

VOTE_OFFSET_CMD_RANK_1 : Integer
VOTE_OFFSET_CMD_RANK_1 = 4

VOTE_OFFSET_CMD_RANK_2 : Integer
VOTE_OFFSET_CMD_RANK_2 = 5

-- =============================================================================
-- Event Signatures
-- =============================================================================

||| Voted(uint256 indexed pid, address indexed rep, Vote vote)
EVENT_VOTED : Integer
EVENT_VOTED = 0xbf5d3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8e82d9d3b3e3f3b3d3e3b79e8eb

-- =============================================================================
-- Storage Helpers
-- =============================================================================

getProposalSlot : Integer -> IO Integer
getProposalSlot pid = do
  mstore 0 pid
  mstore 32 SLOT_DELIBERATION
  keccak256 0 64

getProposalMetaSlot : Integer -> IO Integer
getProposalMetaSlot pid = do
  baseSlot <- getProposalSlot pid
  pure (baseSlot + 0x30)

||| Get vote slot for a representative on a proposal
||| slot = keccak256(repAddr . (metaSlot + 0x10))
getVoteSlot : Integer -> Integer -> IO Integer
getVoteSlot pid repAddr = do
  metaSlot <- getProposalMetaSlot pid
  let votesBaseSlot = metaSlot + 0x10
  mstore 0 repAddr
  mstore 32 votesBaseSlot
  keccak256 0 64

||| Get representatives base slot for proposal
||| Reps are stored at metaSlot + 0x08
getRepsSlot : Integer -> IO Integer
getRepsSlot pid = do
  metaSlot <- getProposalMetaSlot pid
  pure (metaSlot + 0x08)

-- =============================================================================
-- Access Control
-- =============================================================================

||| Check if address is a representative for this proposal
isRep : Integer -> Integer -> IO Bool
isRep pid addr = do
  repsSlot <- getRepsSlot pid
  -- Check up to 5 representative slots (configurable)
  checkRep addr repsSlot 0 5
  where
    checkRep : Integer -> Integer -> Integer -> Integer -> IO Bool
    checkRep addr baseSlot idx maxReps =
      if idx >= maxReps
        then pure False
        else do
          repAddr <- sload (baseSlot + idx)
          if repAddr == addr
            then pure True
            else if repAddr == 0
              then pure False  -- No more reps
              else checkRep addr baseSlot (idx + 1) maxReps

||| Require caller to be a representative
requireRep : Integer -> IO ()
requireRep pid = do
  callerAddr <- caller
  isRepresentative <- isRep pid callerAddr
  if isRepresentative
    then pure ()
    else evmRevert 0 0

-- =============================================================================
-- Proposal Validation
-- =============================================================================

||| Check if proposal has expired
isExpired : Integer -> IO Bool
isExpired pid = do
  metaSlot <- getProposalMetaSlot pid
  expiration <- sload (metaSlot + META_OFFSET_EXPIRATION)
  now <- timestamp
  pure (now > expiration)

||| Require proposal to not be expired
requireNotExpired : Integer -> IO ()
requireNotExpired pid = do
  expired <- isExpired pid
  if expired
    then evmRevert 0 0  -- ProposalAlreadyExpired
    else pure ()

-- =============================================================================
-- Vote Validation
-- =============================================================================

||| Validate that header IDs are within bounds
validateHeaderIds : Integer -> Integer -> Integer -> Integer -> IO Bool
validateHeaderIds pid h0 h1 h2 = do
  metaSlot <- getProposalMetaSlot pid
  headerCount <- sload (metaSlot + META_OFFSET_HEADER_COUNT)
  let maxId = headerCount - 1
  pure (h0 <= maxId && h1 <= maxId && h2 <= maxId)

||| Validate that command IDs are within bounds
validateCmdIds : Integer -> Integer -> Integer -> Integer -> IO Bool
validateCmdIds pid c0 c1 c2 = do
  metaSlot <- getProposalMetaSlot pid
  cmdCount <- sload (metaSlot + META_OFFSET_CMD_COUNT)
  -- Command count can be 0 (no commands)
  if cmdCount == 0
    then pure (c0 == 0 && c1 == 0 && c2 == 0)
    else do
      let maxId = cmdCount - 1
      pure (c0 <= maxId && c1 <= maxId && c2 <= maxId)

-- =============================================================================
-- Vote Function
-- =============================================================================

||| Cast a ranked-choice vote
||| vote(pid, h0, h1, h2, c0, c1, c2)
||| h0, h1, h2 = ranked header IDs (1st, 2nd, 3rd choice)
||| c0, c1, c2 = ranked command IDs (1st, 2nd, 3rd choice)
vote : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO ()
vote pid h0 h1 h2 c0 c1 c2 = do
  -- Access control
  requireRep pid
  -- Check proposal not expired
  requireNotExpired pid
  -- Validate vote
  validHeaders <- validateHeaderIds pid h0 h1 h2
  validCmds <- validateCmdIds pid c0 c1 c2
  if not validHeaders
    then evmRevert 0 0  -- InvalidHeaderId
    else if not validCmds
      then evmRevert 0 0  -- InvalidCommandId
      else do
        -- Store vote
        repAddr <- caller
        voteSlot <- getVoteSlot pid repAddr
        sstore (voteSlot + VOTE_OFFSET_HEADER_RANK_0) h0
        sstore (voteSlot + VOTE_OFFSET_HEADER_RANK_1) h1
        sstore (voteSlot + VOTE_OFFSET_HEADER_RANK_2) h2
        sstore (voteSlot + VOTE_OFFSET_CMD_RANK_0) c0
        sstore (voteSlot + VOTE_OFFSET_CMD_RANK_1) c1
        sstore (voteSlot + VOTE_OFFSET_CMD_RANK_2) c2
        -- Emit Voted event
        mstore 0 h0
        mstore 32 h1
        mstore 64 h2
        mstore 96 c0
        mstore 128 c1
        mstore 160 c2
        log3 0 192 EVENT_VOTED pid repAddr

-- =============================================================================
-- Function Selectors
-- =============================================================================

-- vote(uint256,uint256,uint256,uint256,uint256,uint256,uint256)
SEL_VOTE : Integer
SEL_VOTE = 0xb3c0aedd

-- getVote(uint256 pid, address rep) returns (uint256[6])
SEL_GET_VOTE : Integer
SEL_GET_VOTE = 0x9e7b8d61

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

main : IO ()
main = do
  selector <- getSelector

  if selector == SEL_VOTE
    then do
      -- vote(pid, h0, h1, h2, c0, c1, c2)
      pid <- calldataload 4
      h0 <- calldataload 36
      h1 <- calldataload 68
      h2 <- calldataload 100
      c0 <- calldataload 132
      c1 <- calldataload 164
      c2 <- calldataload 196
      vote pid h0 h1 h2 c0 c1 c2
      -- Return success (empty return for void function)
      evmReturn 0 0

    else if selector == SEL_GET_VOTE
    then do
      -- getVote(uint256 pid, address rep)
      pid <- calldataload 4
      repAddr <- calldataload 36
      voteSlot <- getVoteSlot pid repAddr
      h0 <- sload (voteSlot + VOTE_OFFSET_HEADER_RANK_0)
      h1 <- sload (voteSlot + VOTE_OFFSET_HEADER_RANK_1)
      h2 <- sload (voteSlot + VOTE_OFFSET_HEADER_RANK_2)
      c0 <- sload (voteSlot + VOTE_OFFSET_CMD_RANK_0)
      c1 <- sload (voteSlot + VOTE_OFFSET_CMD_RANK_1)
      c2 <- sload (voteSlot + VOTE_OFFSET_CMD_RANK_2)
      mstore 0 h0
      mstore 32 h1
      mstore 64 h2
      mstore 96 c0
      mstore 128 c1
      mstore 160 c2
      evmReturn 0 192

    else evmRevert 0 0
