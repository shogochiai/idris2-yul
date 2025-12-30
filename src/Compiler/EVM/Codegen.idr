||| EVM Backend Code Generator
||| Compiles Idris2 ANF to Yul
module Compiler.EVM.Codegen

import Compiler.EVM.YulIR
import Compiler.EVM.Memory
import Compiler.EVM.Primitives
import Compiler.EVM.Foreign

import Core.CompileExpr
import Core.CompileExpr.Pretty
import Core.Context
import Core.Core
import Core.Name
import Core.TT

import Compiler.ANF
import Compiler.Common

import Data.List
import Data.List1
import Data.SortedMap
import Data.SortedSet
import Data.String
import Data.Vect
import Control.Monad.Either
import System.File

%default covering

-- =============================================================================
-- Name Mangling
-- =============================================================================

||| Show namespace as underscore-separated string
showNS : Namespace -> String
showNS ns = joinBy "_" (reverse $ unsafeUnfoldNamespace ns)

||| Extract EVM opcode from foreign string directly
||| Handles format like "evm:sload" by splitting on ":"
extractEvmOp : String -> Maybe (String, List String)
extractEvmOp s =
  if isPrefixOf "evm:" s
    then Just (substr 4 (length s) s, [])
    else Nothing

||| Sanitize string for Yul identifier (remove invalid characters)
sanitize : String -> String
sanitize s = pack $ map sanitizeChar (unpack s)
  where
    sanitizeChar : Char -> Char
    sanitizeChar c =
      if isAlphaNum c || c == '_'
        then c
        else '_'

||| Convert Idris name to valid Yul identifier
export
mangleName : Name -> YulId
mangleName (NS ns n) = showNS ns ++ "_" ++ mangleName n
mangleName (UN (Basic n)) = "u_" ++ sanitize n
mangleName (UN (Field n)) = "f_" ++ sanitize n
mangleName (UN Underscore) = "_"
mangleName (MN n i) = "m_" ++ sanitize n ++ "_" ++ show i
mangleName (PV n i) = "pv_" ++ mangleName n ++ "_" ++ show i
mangleName (DN _ n) = mangleName n
mangleName (Nested (i, j) n) = "n_" ++ show i ++ "_" ++ show j ++ "_" ++ mangleName n
mangleName (CaseBlock outer i) = "case_" ++ sanitize outer ++ "_" ++ show i
mangleName (WithBlock outer i) = "with_" ++ sanitize outer ++ "_" ++ show i
mangleName (Resolved i) = "r_" ++ show i

||| Convert variable index to Yul variable name
varName : Int -> YulId
varName i = "v" ++ show i

||| Convert AVar to Yul expression
compileAVar : AVar -> YulExpr
compileAVar (ALocal i) = yulVar (varName i)
compileAVar ANull = yulNum 0

-- =============================================================================
-- Function Arity Tracking
-- =============================================================================

||| Map from mangled function name to its arity
FnArityMap : Type
FnArityMap = SortedMap String Nat

||| Set of functions that don't return values
NoReturnSet : Type
NoReturnSet = SortedSet String

||| Map from function name to unique closure ID
ClosureIdMap : Type
ClosureIdMap = SortedMap String Integer

||| Collect function arities from all definitions
collectArities : List (Name, ANFDef) -> FnArityMap
collectArities defs = foldl addArity empty defs
  where
    addArity : FnArityMap -> (Name, ANFDef) -> FnArityMap
    addArity m (n, MkAFun args _) = insert (mangleName n) (length args) m
    addArity m (n, MkACon _ arity _) = insert (mangleName n) (cast arity) m
    addArity m (n, MkAForeign _ fargs _) = insert (mangleName n) (length fargs) m
    addArity m (n, MkAError _) = insert (mangleName n) 0 m

||| Collect functions that don't return values
collectNoReturn : List (Name, ANFDef) -> NoReturnSet
collectNoReturn defs = foldl addNoReturn empty defs
  where
    addNoReturn : NoReturnSet -> (Name, ANFDef) -> NoReturnSet
    addNoReturn s (n, MkAForeign _ _ ret) =
      case ret of
        CFUnit => insert (mangleName n) s
        CFIORes CFUnit => insert (mangleName n) s
        _ => s
    addNoReturn s (n, MkAError _) = insert (mangleName n) s
    addNoReturn s _ = s

||| Pad arguments to match expected arity
padArgs : FnArityMap -> String -> List YulExpr -> List YulExpr
padArgs arities fnName args =
  case lookup fnName arities of
    Nothing => args  -- Unknown function, use as-is
    Just expectedArity =>
      let currentLen = length args
          needed = minus expectedArity currentLen
      in args ++ replicate needed (yulNum 0)

-- =============================================================================
-- Closure Collection (for dispatch table generation)
-- =============================================================================

mutual
  ||| Collect functions used as closures in an ANF expression
  collectClosuresExpr : ANF -> SortedSet String
  collectClosuresExpr (AV _ _) = empty
  collectClosuresExpr (AAppName _ _ _ _) = empty
  collectClosuresExpr (AUnderApp _ n _ _) = singleton (mangleName n)
  collectClosuresExpr (AApp _ _ _ _) = empty
  collectClosuresExpr (ACon _ _ _ _ _) = empty
  collectClosuresExpr (AOp _ _ _ _) = empty
  collectClosuresExpr (AExtPrim _ _ _ _) = empty
  collectClosuresExpr (APrimVal _ _) = empty
  collectClosuresExpr (AErased _) = empty
  collectClosuresExpr (ACrash _ _) = empty
  collectClosuresExpr (ALet _ _ val body) =
    union (collectClosuresExpr val) (collectClosuresExpr body)
  collectClosuresExpr (AConCase _ _ alts def) =
    let altClosures = foldl (\acc, alt => union acc (collectClosuresAlt alt)) empty alts
        defClosures = maybe empty collectClosuresExpr def
    in union altClosures defClosures
  collectClosuresExpr (AConstCase _ _ alts def) =
    let altClosures = foldl (\acc, alt => union acc (collectClosuresConstAlt alt)) empty alts
        defClosures = maybe empty collectClosuresExpr def
    in union altClosures defClosures

  collectClosuresAlt : AConAlt -> SortedSet String
  collectClosuresAlt (MkAConAlt _ _ _ _ body) = collectClosuresExpr body

  collectClosuresConstAlt : AConstAlt -> SortedSet String
  collectClosuresConstAlt (MkAConstAlt _ body) = collectClosuresExpr body

||| Collect all closure targets from definitions
collectClosureTargets : List (Name, ANFDef) -> SortedSet String
collectClosureTargets defs = foldl addClosures empty defs
  where
    addClosures : SortedSet String -> (Name, ANFDef) -> SortedSet String
    addClosures s (_, MkAFun _ body) = union s (collectClosuresExpr body)
    addClosures s _ = s

||| Build closure ID map from set of closure targets
buildClosureIdMap : SortedSet String -> ClosureIdMap
buildClosureIdMap targets =
  let names = SortedSet.toList targets
      indexed = zip (take (length names) [1..]) names
  in foldl (\m, (idx, name) => insert name idx m) empty indexed

-- =============================================================================
-- ANF to Yul Compilation
-- =============================================================================

||| Compilation context
record CompileCtx where
  constructor MkCompileCtx
  arities : FnArityMap
  closureIds : ClosureIdMap

mutual
  ||| Compile ANF expression to (statements, expression)
  ||| Returns statements needed to compute the expression, plus the expression itself
  compileANFExprWithStmts : CompileCtx -> ANF -> Core (List YulStmt, YulExpr)

  -- Variable reference
  compileANFExprWithStmts ctx (AV _ var) = pure ([], compileAVar var)

  -- Application to named function
  compileANFExprWithStmts ctx (AAppName _ _ n args) = do
    let fnName = mangleName n
    let compiledArgs = map compileAVar args
    let paddedArgs = padArgs ctx.arities fnName compiledArgs
    pure ([], yulCall fnName paddedArgs)

  -- Partial application - create a closure
  compileANFExprWithStmts ctx (AUnderApp _ n missing args) = do
    let fnName = mangleName n
    let compiledArgs = map compileAVar args
    case lookup fnName ctx.closureIds of
      Just closureId =>
        pure ([], yulCall "mk_closure" ([yulNum closureId, yulNum (cast missing)] ++ compiledArgs))
      Nothing =>
        let paddedArgs = padArgs ctx.arities fnName compiledArgs
        in pure ([], yulCall fnName paddedArgs)

  -- Application of closure to argument
  compileANFExprWithStmts ctx (AApp _ _ closure arg) = do
    pure ([], yulCall "apply_closure" [compileAVar closure, compileAVar arg])

  -- Constructor application
  compileANFExprWithStmts ctx (ACon _ n ci (Just tag) args) = do
    pure ([], yulNum (cast tag))

  compileANFExprWithStmts ctx (ACon _ n ci Nothing args) = do
    pure ([], yulNum 0)

  -- Primitive operation
  compileANFExprWithStmts ctx (AOp _ _ fn args) = do
    let compiledArgs = map compileAVar args
    pure ([], compilePrimFn fn compiledArgs)

  -- External primitive (FFI)
  compileANFExprWithStmts ctx (AExtPrim _ _ n args) = do
    let compiledArgs = map compileAVar args
    case parseEvmForeign (show n) of
      Just (op, _) => pure ([], evmOpcodeToYul op compiledArgs)
      Nothing =>
        let fnName = mangleName n
            paddedArgs = padArgs ctx.arities fnName compiledArgs
        in pure ([], yulCall fnName paddedArgs)

  -- Constant
  compileANFExprWithStmts ctx (APrimVal _ c) = pure ([], compileConstant c)

  -- Erased value
  compileANFExprWithStmts ctx (AErased _) = pure ([], yulNum 0)

  -- Crash
  compileANFExprWithStmts ctx (ACrash _ msg) = pure ([], yulCall "revert" [yulNum 0, yulNum 0])

  -- Let binding - THIS IS THE KEY FIX
  -- Generate the assignment statement, then compile the body
  -- This handles nested lets by hoisting inner assignments before the outer one
  compileANFExprWithStmts ctx (ALet _ var val body) = do
    (valStmts, valExpr) <- compileANFExprWithStmts ctx val
    (bodyStmts, bodyExpr) <- compileANFExprWithStmts ctx body
    -- The assignment for this let binding
    let assignStmt = yulAssign (varName var) valExpr
    pure (valStmts ++ [assignStmt] ++ bodyStmts, bodyExpr)

  -- Case expression - just return the scrutinee variable
  compileANFExprWithStmts ctx (AConCase _ sc alts def) = do
    pure ([], compileAVar sc)

  compileANFExprWithStmts ctx (AConstCase _ sc alts def) = do
    pure ([], compileAVar sc)

||| Compile ANF expression to Yul expression (legacy interface)
compileANFExpr : CompileCtx -> ANF -> Core YulExpr
compileANFExpr ctx anf = do
  (_, expr) <- compileANFExprWithStmts ctx anf
  pure expr

||| Compile ANF expression (with arity map for proper call padding)
compileANFWithArities : FnArityMap -> ANF -> Core YulExpr
compileANFWithArities arities = compileANFExpr (MkCompileCtx arities empty)

||| Legacy wrapper (for backward compatibility)
export
compileANF : ANF -> Core YulExpr
compileANF = compileANFWithArities empty

||| Compile ANF to Yul statement (for let bindings and cases)
compileANFStmtCtx : CompileCtx -> ANF -> Core (List YulStmt)
compileANFStmtCtx ctx (ALet _ var val body) = do
  -- Use compileANFExprWithStmts to capture any nested let bindings
  (valStmts, valExpr) <- compileANFExprWithStmts ctx val
  bodyStmts <- compileANFStmtCtx ctx body
  -- Include hoisted statements, then the assignment, then body
  pure $ valStmts ++ [yulAssign (varName var) valExpr] ++ bodyStmts

compileANFStmtCtx ctx (AConCase _ sc alts mdef) = do
  let scExpr = compileAVar sc
  cases <- traverse (compileConAlt scExpr) alts
  defStmt <- case mdef of
    Just d => do
      stmts <- compileANFStmtCtx ctx d
      pure $ Just (YBlock stmts)
    Nothing => pure Nothing
  pure [YSwitch (readTag scExpr) cases defStmt]
  where
    -- Generate field extraction for constructor arguments
    extractFields : YulExpr -> List Int -> List YulStmt
    extractFields ptr args =
      zipWith (\idx, var => yulAssign (varName var) (readField ptr idx))
              (take (length args) [0..]) args

    compileConAlt : YulExpr -> AConAlt -> Core YulCase
    compileConAlt scPtr (MkAConAlt n ci (Just tag) args body) = do
      let fieldExtracts = extractFields scPtr args
      bodyStmts <- compileANFStmtCtx ctx body
      pure $ MkCase (YulNum (cast tag)) (YBlock (fieldExtracts ++ bodyStmts))
    compileConAlt scPtr (MkAConAlt n ci Nothing args body) = do
      let fieldExtracts = extractFields scPtr args
      bodyStmts <- compileANFStmtCtx ctx body
      pure $ MkCase (YulNum 0) (YBlock (fieldExtracts ++ bodyStmts))

compileANFStmtCtx ctx (AConstCase _ sc alts mdef) = do
  let scExpr = compileAVar sc
  cases <- traverse compileConstAlt alts
  defStmt <- case mdef of
    Just d => do
      stmts <- compileANFStmtCtx ctx d
      pure $ Just (YBlock stmts)
    Nothing => pure Nothing
  pure [YSwitch scExpr cases defStmt]
  where
    constToLit : Constant -> YulLiteral
    constToLit (I x) = YulNum (cast x)
    constToLit (BI x) = YulNum x
    constToLit (B8 x) = YulNum (cast x)
    constToLit (B16 x) = YulNum (cast x)
    constToLit (B32 x) = YulNum (cast x)
    constToLit (B64 x) = YulNum (cast x)
    constToLit (Ch c) = YulNum (cast (ord c))
    constToLit _ = YulNum 0

    compileConstAlt : AConstAlt -> Core YulCase
    compileConstAlt (MkAConstAlt c body) = do
      bodyStmts <- compileANFStmtCtx ctx body
      pure $ MkCase (constToLit c) (YBlock bodyStmts)

compileANFStmtCtx ctx anf = do
  -- Use compileANFExprWithStmts to capture any nested let bindings
  (stmts, expr) <- compileANFExprWithStmts ctx anf
  -- Include hoisted statements, then the final expression/assignment
  let finalStmt = case expr of
        YCall fname _ =>
          if isVoidOpcode fname
            then YExprStmt expr
            else yulAssign "result" expr
        _ => yulAssign "result" expr
  pure $ stmts ++ [finalStmt]
  where
    isVoidOpcode : String -> Bool
    isVoidOpcode "revert" = True
    isVoidOpcode "return" = True
    isVoidOpcode "stop" = True
    isVoidOpcode "selfdestruct" = True
    isVoidOpcode "log0" = True
    isVoidOpcode "log1" = True
    isVoidOpcode "log2" = True
    isVoidOpcode "log3" = True
    isVoidOpcode "log4" = True
    isVoidOpcode "sstore" = True
    isVoidOpcode "mstore" = True
    isVoidOpcode "mstore8" = True
    isVoidOpcode _ = False

||| Compile ANF to Yul statement (for let bindings and cases)
compileANFStmtWithArities : FnArityMap -> ANF -> Core (List YulStmt)
compileANFStmtWithArities arities = compileANFStmtCtx (MkCompileCtx arities empty)

||| Legacy wrapper
export
compileANFStmt : ANF -> Core (List YulStmt)
compileANFStmt = compileANFStmtWithArities empty

-- =============================================================================
-- Definition Compilation
-- =============================================================================

||| Get variable index from AVar
avarIdx : AVar -> Int
avarIdx (ALocal i) = i
avarIdx ANull = -1

||| Find the maximum variable index used in an ANF expression
maxVarIndex : ANF -> Int
maxVarIndex (AV _ v) = avarIdx v
maxVarIndex (AAppName _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (AUnderApp _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (AApp _ _ c a) = max (avarIdx c) (avarIdx a)
maxVarIndex (ACon _ _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (AOp _ _ _ args) = foldl max (-1) (toList $ map avarIdx args)
maxVarIndex (AExtPrim _ _ _ args) = foldl max (-1) (map avarIdx args)
maxVarIndex (APrimVal _ _) = -1
maxVarIndex (AErased _) = -1
maxVarIndex (ACrash _ _) = -1
maxVarIndex (ALet _ var val body) = max var (max (maxVarIndex val) (maxVarIndex body))
maxVarIndex (AConCase _ sc alts def) =
  let scIdx = avarIdx sc
      altMax = foldl max (-1) (map altIdx alts)
      defMax = maybe (-1) maxVarIndex def
  in max scIdx (max altMax defMax)
  where
    altIdx : AConAlt -> Int
    altIdx (MkAConAlt _ _ _ args body) = max (foldl max (-1) args) (maxVarIndex body)
maxVarIndex (AConstCase _ sc alts def) =
  let scIdx = avarIdx sc
      altMax = foldl max (-1) (map constAltIdx alts)
      defMax = maybe (-1) maxVarIndex def
  in max scIdx (max altMax defMax)
  where
    constAltIdx : AConstAlt -> Int
    constAltIdx (MkAConstAlt _ body) = maxVarIndex body

||| Initialize undefined variables (those used but not in params or let-bound)
initUndefinedVars : List Int -> ANF -> List YulStmt
initUndefinedVars params body =
  let maxIdx = maxVarIndex body
      paramSet = SortedSet.fromList params
      allVars = if maxIdx >= 0 then [0..maxIdx] else []
      undefined = filter (\i => i >= 0 && not (contains i paramSet)) allVars
  in map (\i => yulLet (varName i) (yulNum 0)) undefined

||| Compile an ANF definition to a Yul function (with context)
compileDefCtx : CompileCtx -> (Name, ANFDef) -> Core (Maybe YulFun)
compileDefCtx ctx (n, MkAFun args body) = do
  let initStmts = initUndefinedVars args body
  bodyStmts <- compileANFStmtCtx ctx body
  -- The body already assigns to 'result' via compileANFStmt, no need to append extra assignment
  pure $ Just $ MkYulFun
    { name = mangleName n
    , params = map varName args
    , returns = ["result"]
    , body = YBlock (initStmts ++ bodyStmts)
    }

compileDefCtx ctx (n, MkACon tag arity nt) = do
  -- Constructor: generate allocation function
  let argNames = map (\i => "arg" ++ show i) (take arity [0..])
  pure $ Just $ MkYulFun
    { name = mangleName n
    , params = argNames
    , returns = ["ptr"]
    , body = YBlock $
        allocWords (cast $ 1 + arity) ++
        [YExprStmt $ mstore (yulVar "ptr") (yulNum (cast (fromMaybe 0 tag)))] ++
        zipWith (\i, arg => YExprStmt $ mstore
          (yulCall "add" [yulVar "ptr", yulNum ((i + 1) * 32)])
          (yulVar arg)) (take (length argNames) [0..]) argNames
    }

compileDefCtx ctx (n, MkAForeign ccs fargs ret) = do
  -- Foreign function: generate FFI wrapper
  -- ccs contains the %foreign annotations, e.g. ["evm:sload"]
  let ccStr = fromMaybe "" (head' ccs)
  -- Try direct opcode extraction if parseEvmForeign fails
  let mbOp = parseEvmForeign ccStr <|> extractEvmOp ccStr
  let argNames = map (\i => "arg" ++ show i) (take (length fargs) [0..])
  -- Determine if the opcode returns a value
  let hasReturn = case ret of
                    CFUnit => False
                    CFIORes CFUnit => False
                    _ => True
  -- All functions return "result" for consistency - void functions return 0
  case mbOp of
    Just (op, _) => do
      pure $ Just $ MkYulFun
        { name = mangleName n
        , params = argNames
        , returns = ["result"]  -- Always return result
        , body = YBlock $
            if hasReturn
              then [yulAssign "result" (evmOpcodeToYul op (map yulVar argNames))]
              else [YExprStmt (evmOpcodeToYul op (map yulVar argNames))
                   , yulAssign "result" (yulNum 0)]  -- Return 0 for void
        }
    Nothing =>
      -- Try to extract opcode from function name if foreign spec failed
      -- Look for patterns like prim__sload -> sload
      let fnName = show n
          mbBuiltin = extractBuiltinOp fnName
      in case mbBuiltin of
        Just op =>
          pure $ Just $ MkYulFun
            { name = mangleName n
            , params = argNames
            , returns = ["result"]  -- Always return result
            , body = YBlock $
                if hasReturn
                  then [yulAssign "result" (evmOpcodeToYul op (map yulVar argNames))]
                  else [YExprStmt (evmOpcodeToYul op (map yulVar argNames))
                       , yulAssign "result" (yulNum 0)]  -- Return 0 for void
            }
        Nothing =>
          -- Generate a stub that calls invalid() for unrecognized foreign
          pure $ Just $ MkYulFun
            { name = mangleName n
            , params = argNames
            , returns = ["result"]
            , body = YBlock [yulAssign "result" (yulCall "invalid" [])]
            }
  where
    extractBuiltinOp : String -> Maybe String
    extractBuiltinOp name =
      -- Extract opcode from names like "Main.prim__sload" or "prim__sload"
      let parts = Data.List1.forget $ Data.String.split (== '_') name
          filtered = filter (\p => p /= "" && p /= "prim") parts
      in case filtered of
           [] => Nothing
           (x :: xs) => Just (fromMaybe x (last' xs))

compileDefCtx ctx (n, MkAError exp) = do
  -- Error case: generate revert
  pure $ Just $ MkYulFun
    { name = mangleName n
    , params = []
    , returns = []
    , body = YBlock [YExprStmt $ yulCall "revert" [yulNum 0, yulNum 0]]
    }

||| Compile an ANF definition to a Yul function (with arity map)
compileDefWithArities : FnArityMap -> (Name, ANFDef) -> Core (Maybe YulFun)
compileDefWithArities arities = compileDefCtx (MkCompileCtx arities empty)

||| Compile an ANF definition to a Yul function
export
compileDef : (Name, ANFDef) -> Core (Maybe YulFun)
compileDef = compileDefWithArities empty

-- =============================================================================
-- Main Code Generation
-- =============================================================================

||| Find the main function from definitions
findMain : List (Name, ANFDef) -> Maybe Name
findMain [] = Nothing
findMain ((n, MkAFun _ _) :: rest) =
  case n of
    NS _ (UN (Basic "main")) => Just n
    UN (Basic "main") => Just n
    _ => findMain rest
findMain (_ :: rest) = findMain rest

||| Deduplicate functions by name (keep first occurrence)
deduplicateFuns : List YulFun -> List YulFun
deduplicateFuns funs = go SortedSet.empty funs
  where
    go : SortedSet String -> List YulFun -> List YulFun
    go seen [] = []
    go seen (f :: fs) =
      if contains f.name seen
        then go seen fs  -- Skip duplicate
        else f :: go (insert f.name seen) fs

||| Generate complete Yul object from ANF definitions
||| Creates proper EVM contract structure with constructor and runtime
export
generateYul : String -> List (Name, ANFDef) -> Core YulObject
generateYul name defs = do
  -- First pass: collect all function arities and closure targets
  let arities = collectArities defs
  let closureTargets = collectClosureTargets defs
  let closureIds = buildClosureIdMap closureTargets
  let ctx = MkCompileCtx arities closureIds
  -- Second pass: compile with context
  allFuns <- catMaybes <$> traverse (compileDefCtx ctx) defs
  -- Deduplicate functions (ANF may produce duplicates from typeclass instances)
  let dedupedFuns = deduplicateFuns allFuns
  let mainFn = fromMaybe (UN (Basic "main")) (findMain defs)
  let mainName = mangleName mainFn
  -- Generate closure support functions
  let closureFuns = mkClosureFunction :: applyClosureFunction closureIds arities :: []
  -- Add built-in functions
  let builtinFuns = allocatorFunction :: closureFuns
  let allWithBuiltins = builtinFuns ++ dedupedFuns
  -- Dead code elimination: only keep functions reachable from main
  let reachableFuns = eliminateDeadCode [mainName] allWithBuiltins

  -- Create runtime object (the actual contract logic)
  let runtimeObject = MkYulObject
        { name = "runtime"
        , code = initMemory ++ [YExprStmt $ yulCall "pop" [yulCall mainName [yulNum 0]]]
        , functions = reachableFuns
        , subObjects = []
        }

  -- Constructor: copy runtime code to memory and return it
  -- datacopy(0, dataoffset("runtime"), datasize("runtime"))
  -- return(0, datasize("runtime"))
  let constructorCode =
        [ YExprStmt $ yulCall "datacopy"
            [ yulNum 0
            , yulCall "dataoffset" [YLit (YulStr "runtime")]
            , yulCall "datasize" [YLit (YulStr "runtime")]
            ]
        , YExprStmt $ yulCall "return"
            [ yulNum 0
            , yulCall "datasize" [YLit (YulStr "runtime")]
            ]
        ]

  pure $ MkYulObject
    { name = name
    , code = constructorCode
    , functions = []  -- No functions in constructor
    , subObjects = [runtimeObject]
    }
  where
    ||| Create a closure in memory
    ||| Layout: [funcId, arity, arg0, arg1, ...]
    ||| For simplicity, we allocate a fixed-size closure (funcId + arity + up to 8 args)
    mkClosureFunction : YulFun
    mkClosureFunction = MkYulFun
      { name = "mk_closure"
      , params = ["func_id", "arity", "arg0", "arg1", "arg2", "arg3"]
      , returns = ["ptr"]
      , body = YBlock
          [ yulAssign "ptr" (mload (yulNum FREE_MEM_PTR))
          , YExprStmt $ mstore (yulNum FREE_MEM_PTR)
              (yulCall "add" [yulVar "ptr", yulNum (6 * 32)])  -- 6 words
          , YExprStmt $ mstore (yulVar "ptr") (yulVar "func_id")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 32]) (yulVar "arity")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 64]) (yulVar "arg0")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 96]) (yulVar "arg1")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 128]) (yulVar "arg2")
          , YExprStmt $ mstore (yulCall "add" [yulVar "ptr", yulNum 160]) (yulVar "arg3")
          ]
      }

    ||| Generate dispatch case for a single closure target
    mkDispatchCase : FnArityMap -> (String, Integer) -> YulCase
    mkDispatchCase arities (fnName, closureId) =
      let arity = fromMaybe 1 (lookup fnName arities)
          -- Generate call with captured args from closure + new arg
          -- Closure layout: [funcId, arity, arg0, arg1, arg2, arg3]
          capturedArgs = map (\i => mload (yulCall "add" [yulVar "closure", yulNum ((i + 2) * 32)]))
                             (take arity [0..])
          -- The new arg replaces the last captured slot based on remaining arity
          callArgs = capturedArgs ++ [yulVar "arg"]
      in MkCase (YulNum closureId) (YBlock
          -- For arity 1: call function directly with all args
          -- For arity > 1: create new closure with arity - 1
          [ YIf (yulCall "eq" [mload (yulCall "add" [yulVar "closure", yulNum 32]), yulNum 1])
              (YBlock
                [ yulAssign "result" (yulCall fnName (take (cast arity) callArgs))
                , YLeave
                ])
          , yulAssign "result" (yulCall "mk_closure"
              [ mload (yulVar "closure")  -- same func_id
              , yulCall "sub" [mload (yulCall "add" [yulVar "closure", yulNum 32]), yulNum 1]  -- arity - 1
              , mload (yulCall "add" [yulVar "closure", yulNum 64])   -- existing arg0
              , mload (yulCall "add" [yulVar "closure", yulNum 96])   -- existing arg1
              , mload (yulCall "add" [yulVar "closure", yulNum 128])  -- existing arg2
              , yulVar "arg"  -- new arg becomes arg3
              ])
          ])

    ||| Apply closure: dispatch based on function ID
    applyClosureFunction : ClosureIdMap -> FnArityMap -> YulFun
    applyClosureFunction closureIdMap arities =
      let entries = SortedMap.toList closureIdMap
          cases = map (mkDispatchCase arities) entries
      in MkYulFun
        { name = "apply_closure"
        , params = ["closure", "arg"]
        , returns = ["result"]
        , body = YBlock
            [ yulLet "func_id" (mload (yulVar "closure"))
            , YSwitch (yulVar "func_id") cases
                (Just $ YBlock [yulAssign "result" (yulNum 0)])  -- default: return 0
            ]
        }

||| Write Yul output to file
export
writeYulFile : String -> YulObject -> Core ()
writeYulFile path obj = do
  let content = showObject obj
  Right () <- coreLift $ writeFile path content
    | Left err => throw (FileErr path err)
  pure ()
