{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Anchovy.Codegen where

import Data.Map (Map)
import qualified Data.Map as Map

import LLVM.General.AST

-- for now, all variables are of this type
double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState {
	currentBlock :: Name
	, blocks :: Map Name BlockState
	, symtab :: SymbolTable
	, blockCount :: Int
	, count :: Word
	, names :: Names
} deriving Show

data BlockState = BlockState {
	idx :: Int
	, stack :: [Named Instruction]
	, term :: Maybe (Named Terminator)
} deriving Show

newtype Codegen a = Codegen { runCodegen :: State Codegen a}
	deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM { unLLVM :: State Module a }

runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
	defns <- gets moduleDefinitions
	modify $ \s -> s { moduleDefinitions = defns ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retTy label argTys body = addDefn $
	GlobalDefinition $ functionDefaults {
		name = Name label
		, parameters = ([Parameter ty nm [] | (ty, nm) <- argTys], False)
		, returnType = retTy
		, basicBlocks = body
	}

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retTy label argTys = addDefn $
	GlobalDefinition $ functionDefaults {
		name = Name label
		, parameters = ([Parameter ty nm [] | (ty, nm) <- argTys], False)
		, returnType = retTy
		, basicBlocks = []
	}

entry :: Codegen Name
entry = get currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
	bls <- gets blocks
	ix <- gets blockCount
	nms <- gets names

	let
		new = emptyBlock ix
		(qname, supply) = uniqueName bname nms

	modify $ \s -> s {
		blocks = Map.insert (Name qname) new bls
		, blockCount = ix + 1
		, names = supply
	}
	return $ Name qname

setBlock :: Name -> Codegen Name
setBlock bname = do
	modify $ \s -> s { currentBlock = bname }
	return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
	active <- gets currentBlock
	modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
	c <- gets currentBlock
	blks <- gets blocks
	case Map.lookup


type Names = Map String Int


