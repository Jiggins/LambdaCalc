module Eval where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map

import Lambda

type Eval a = WriterT [Step] (State EvalState) a
type Scope = Map.Map String Value
type Step = (Int, Expr)

data EvalState = EvalState { depth :: Int }
  deriving (Show)

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr Scope

instance Show Value where
  show (VInt x) = show x
  show (VBool x) = show x
  show VClosure{} = "<<closure>>"

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)

eval :: Eval.Scope -> Expr -> Eval Value
eval env expr = case expr of

  Literal (LInt x) -> do
    return $ VInt (fromIntegral x)

  Literal (LBool x) -> do
    return $ VBool x

  Variable x -> do
    reduce expr
    return $ env Map.! x

  Lambda x body -> inc $ do
    return (VClosure x body env)

  Application a b -> inc $ do
    x <- eval env a
    reduce a
    y <- eval env b
    reduce b
    apply x y

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = depth s + 1 }
  out <- m
  modify $ \s -> s { depth = depth s - 1 }
  return out

reduce :: Expr -> Eval ()
reduce x = do
  d <- gets depth
  tell [(d, x)]
  return ()

extend :: Scope -> String -> Value -> Scope
extend env value term = Map.insert value term env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = do
  eval (extend clo n ex) e
apply _ _  = error "Tried to apply non-closure"

emptyScope :: Scope
emptyScope = Map.empty
