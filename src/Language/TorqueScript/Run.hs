{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.TorqueScript.Run where

import Control.Lens
import Control.Monad.State

import Data.Char(toLower)
import qualified Data.List as L
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Sequence(Seq)
import qualified Data.Sequence as S

import Language.TorqueScript.AST
-- import Language.TorqueScript.Run.Utils

import Text.Read(readMaybe)

data TSValue = TSString String
             | TSDouble Double
             deriving Show

class ToTSValue a where
  toTsValue :: a -> TSValue

instance ToTSValue Bool where
  toTsValue x = TSDouble $ if x then 1 else 0

instance ToTSValue Int where
  toTsValue = TSDouble . fromIntegral

instance ToTSValue Double where
  toTsValue = TSDouble

instance ToTSValue String where
  toTsValue = TSString

type ObjectId = Int
type ObjectName = String
type TaggedStringId = Int

data Object = Object
              { _objectId :: ObjectId
              , _objectName :: Maybe ObjectName
              , _objectFields :: Map FieldName TSValue
              } deriving Show
makeLenses ''Object

data StackFrame = StackFram
                { _frameVars :: Map VariableName TSValue
                } deriving Show
makeLenses ''StackFrame

data Environment = Environment
                   { _envObjects :: Seq Object
                   , _envGlobalVars :: Map VariableName TSValue
                   , _envStack :: [StackFrame]
                   , _envTaggedStrings :: Seq String
                   } deriving Show
makeLenses ''Environment

mkEnv :: Environment
mkEnv = Environment
        { _envObjects = S.empty
        , _envGlobalVars = M.empty
        , _envStack = []
        , _envTaggedStrings = S.empty
        }

runTS :: TS a -> IO a
runTS m = evalStateT m mkEnv

usePreview :: MonadState s f => Getting (Monoid.First a) s a -> f (Maybe a)
usePreview l = preview l <$> get

type TS = StateT Environment IO

tsToString :: TSValue -> TS String
tsToString (TSString x) = return $ toLower <$> x
tsToString (TSDouble x) = return $ show x

tsToStringKeepCase :: TSValue -> TS String
tsToStringKeepCase (TSString x) = return x
tsToStringKeepCase (TSDouble x) = return $ show x

tsToDouble :: TSValue -> TS Double
tsToDouble (TSDouble x) = return x
tsToDouble (TSString x) = case readMaybe x of
  Just num -> return num
  Nothing -> fromIntegral . fromMaybe 0 . fmap _objectId . L.find ((== Just x) . _objectName) . _envObjects <$> get

tsToInt :: TSValue -> TS Int
tsToInt = fmap floor . tsToDouble

tsToBool :: TSValue -> TS Bool
tsToBool = fmap (/= 0) . tsToInt

storeTaggedString :: String -> TS TaggedStringId
storeTaggedString str = do
  let lowerStr = toLower <$> str
  envTaggedStrings %= \strs -> case S.elemIndexL (lowerStr) strs of
    Just _ -> strs
    Nothing -> strs |> lowerStr
  fromJust . S.elemIndexL lowerStr <$> use envTaggedStrings


-- evalRef :: Reference -> Lens' Environment (Maybe TSValue)
evalRef :: Functor f => Reference -> TS ((Maybe TSValue -> f (Maybe TSValue)) -> Environment -> f Environment)
evalRef (LocalVarReference x) = return $ envStack.singular _head.frameVars.at x
evalRef (GlobalVarReference x) = return $ envGlobalVars.at x
evalRef (FieldReference objExpr name) = do
  objId <- evalExpr (wspValue objExpr) >>= tsToInt
  return $ envObjects.singular (ix objId).objectFields.at name
evalRef (IndexReference baseRef indexExprs) = do
  indexValues <- sequence $ evalExpr . wspValue <$> indexExprs
  indexStrs <- sequence $ tsToString <$> indexValues
  let mergedIndex = L.intercalate "_" indexStrs
  evalRef $ case baseRef of
    (LocalVarReference x) -> LocalVarReference $ x ++ mergedIndex
    (GlobalVarReference x) -> GlobalVarReference $ x ++ mergedIndex
    (FieldReference objExpr name) -> FieldReference objExpr $ name ++ mergedIndex
    (IndexReference _ _) -> error "Double indexing"

evalBinOp :: ToTSValue b => (TSValue -> TS a) -> Expression -> Expression -> (a -> a -> b) -> TS (TSValue)
evalBinOp tsTo lhs rhs f = do
  lhsValue <- evalExpr lhs >>= tsTo
  rhsValue <- evalExpr rhs >>= tsTo
  return $ toTsValue $ f lhsValue rhsValue

evalDoubleBinOp :: ToTSValue b => Expression -> Expression -> (Double -> Double -> b) -> TS TSValue
evalDoubleBinOp = evalBinOp tsToDouble

evalStringBinOp :: ToTSValue b => Expression -> Expression -> (String -> String -> b) -> TS TSValue
evalStringBinOp = evalBinOp tsToString

callFunction :: Call -> TS TSValue
callFunction = undefined

evalExpr :: Expression -> TS TSValue
evalExpr (StrLiteralExpression x) = return $ TSString x
evalExpr (TaggedStrLiteralExpression x) = TSDouble . fromIntegral <$> storeTaggedString x
evalExpr (NumberLiteralExpression x) = return $ TSDouble $ read x
evalExpr (BoolLiteralExpression x) = return $ TSDouble $ if x then 1 else 0
evalExpr (ReferenceExpression x) = fromMaybe (TSString "") <$> (evalRef x >>= use)
evalExpr (CallExpression x) = callFunction x
evalExpr (AssignExpression lhs rhs) = do
  rhsValue <- evalExpr $ wspValue rhs
  lhsRef <- evalRef lhs
  lhsRef .= Just rhsValue
  return rhsValue
evalExpr (TernaryExpression cond true false) = do
  -- No, we can't use tsToBool here
  -- Somehow, values between (0,1) (exclusive) are true in if conditions
  -- but false in boolean operators (|| or &&)
  -- You couldn't make this crap up...
  condValue <- evalExpr (wspValue cond) >>= tsToDouble
  evalExpr . wspValue $ if condValue /= 0
    then true
    else false
evalExpr (NumberEqualsExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (==)
evalExpr (NumberNoEqualsExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (/=)
evalExpr (NumberLessThanExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (>)
evalExpr (NumberGreaterThanExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (<)
evalExpr (NumberLessThanOrEqualsExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (>=)
evalExpr (NumberGreaterThanOrEqualsExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (<=)
evalExpr (BoolOrExpression lhs rhs) = do
  lhsValue <- evalExpr $ wspValue lhs
  lhsBool <- tsToBool lhsValue
  if lhsBool
    then return lhsValue
    else evalExpr $ wspValue rhs
evalExpr (BoolAndExpression lhs rhs) = do
  lhsValue <- evalExpr $ wspValue lhs
  lhsBool <- tsToBool lhsValue
  if lhsBool
    then evalExpr $ wspValue rhs
    else return $ toTsValue False
evalExpr (BoolInvertExpression x) = toTsValue . not <$> (evalExpr (wspValue x) >>= tsToBool)
evalExpr (StringEqualsExpression lhs rhs) = evalStringBinOp (wspValue lhs) (wspValue rhs) (==)
evalExpr (StringNoEqualsExpression lhs rhs) = evalStringBinOp (wspValue lhs) (wspValue rhs) (/=)
evalExpr (StringAppendExpression lhs rhs) = evalBinOp tsToStringKeepCase (wspValue lhs) (wspValue rhs) (++)
evalExpr (NumberAddExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (+)
evalExpr (NumberSubtractExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (-)
evalExpr (NumberMultiplyExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (*)
evalExpr (NumberDivideExpression lhs rhs) = evalDoubleBinOp (wspValue lhs) (wspValue rhs) (/)
evalExpr (NumberModuloExpression lhs rhs) = evalBinOp tsToInt (wspValue lhs) (wspValue rhs) (mod)

