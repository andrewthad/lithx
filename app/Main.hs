{-# language OverloadedStrings #-}

import Control.Monad ((<=<))
import Data.Eexpr.Text.Normal (normalText)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Proxy
import Data.Foldable (fold)

import qualified Data.ByteString as B
import qualified Data.Eexpr.Text as E
import qualified Data.Eexpr.Types as E
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text.Short as TS

import Lith.Parse (parseFunction)
import Lith.Resolve (resolveFunctionDeclaration)
import Lith.Datasolve (datasolveFunctionDeclaration)
import Lith.Typesynth (typesynthFunctionDeclaration)

import qualified Lith.Syntax.Show
import qualified Lith.Datasolved.Show
import qualified Lith.Resolved.Show
import qualified Lith.Syntax.Out as Syntax
import qualified Lith.Resolved as Resolved
import qualified Lith.Datasolved as Datasolved
import qualified Lith.Typed as Typed
import qualified Lith.Demand.Compute
import qualified Lith.Demand.FloatIn
import qualified Lith.Demand.Analyzed.Show
import qualified Lith.ConstantFold.Run
import qualified Lith.ConstantFold.Decorate
import qualified Lith.ConstantFold.Show
import qualified Lith.Term.Eq

import qualified System.IO as IO
import qualified C
import qualified ToC

main :: IO ()
main = do
  allInput <- B.getContents
  let (warns, res) = E.parse allInput
  mapM_ print warns 
  case res of
    Left errs -> mapM_ print errs
    Right eexprs -> case parseFunction eexprs of
      Left e -> putStrLn e
      Right (funDecl,_)  -> do
        let (line1,line2) = Lith.Syntax.Show.showFunction funDecl
        TIO.putStrLn "================"
        TIO.putStrLn "Original Program"
        TIO.putStrLn "================"
        TIO.putStrLn (normalText line1)
        TIO.putStrLn (normalText line2)
        resolved <- step2 funDecl
        datasolved <- step3 resolved
        _ <- step4 datasolved
        decl <- optFixedPoint datasolved
        TIO.putStrLn "================"
        TIO.putStrLn "Compile to C"
        TIO.putStrLn "================"
        B.putStr (TS.toByteString (fold (C.encode 0 (ToC.compileFunction decl))))
        pure ()

optFixedPoint :: Datasolved.FunctionDeclaration -> IO Datasolved.FunctionDeclaration
optFixedPoint decl0 = do
  let go n decl = case n of
        0 -> fail "Did not reach a fixed point after 50 iterations"
        _ -> do
          decl' <- optimize decl
          if Lith.Term.Eq.eqFunctionDeclaration decl decl'
            then pure decl'
            else go (n - 1) decl'
  go (50 :: Int) decl0

-- TODO: Add float-in analysis back after it supports loops
optimize :: Datasolved.FunctionDeclaration -> IO Datasolved.FunctionDeclaration
optimize = stepConstantFold
-- optimize = stepFloatIn <=< stepConstantFold

step2 :: Syntax.FunctionDeclaration -> IO Resolved.FunctionDeclaration
step2 decl = case evalStateT (resolveFunctionDeclaration decl) 0 of
  Left err -> do
    IO.hPutStrLn IO.stderr "Error during name resolution:"
    fail err
  Right decl' -> do
    TIO.putStrLn "================="
    TIO.putStrLn "Labeled Program"
    TIO.putStrLn "================="
    let (optLine1,optLine2) = Lith.Resolved.Show.showFunction decl'
    TIO.putStrLn (normalText optLine1)
    TIO.putStrLn (normalText optLine2)
    pure decl'

step3 :: Resolved.FunctionDeclaration -> IO Datasolved.FunctionDeclaration
step3 decl = case datasolveFunctionDeclaration Map.empty decl of
  Left err -> do
    IO.hPutStrLn IO.stderr "Error during data constructor resolution:"
    fail err
  Right decl' -> do
    TIO.putStrLn "=================="
    TIO.putStrLn "Datasolved Program"
    TIO.putStrLn "=================="
    TIO.putStrLn "(suppressed)"
    pure decl'

step4 :: Datasolved.FunctionDeclaration -> IO Typed.FunctionDeclaration
step4 decl = case typesynthFunctionDeclaration Proxy IntMap.empty decl of
  Left err -> do
    IO.hPutStrLn IO.stderr "Error during type synthesis"
    fail err
  Right decl' -> do
    TIO.putStrLn "=================="
    TIO.putStrLn "Typed Program"
    TIO.putStrLn "=================="
    TIO.putStrLn "(suppressed)"
    pure decl'

stepFloatIn :: Datasolved.FunctionDeclaration -> IO Datasolved.FunctionDeclaration
stepFloatIn decl = do
  let annotated = Lith.Demand.Compute.computeFunctionDeclaration $ decl
  TIO.putStrLn "=================="
  TIO.putStrLn "Use Annotations"
  TIO.putStrLn "=================="
  printExprPair (Lith.Demand.Analyzed.Show.showFunction annotated)
  let decl' = Lith.Demand.FloatIn.run annotated
  TIO.putStrLn "=================="
  TIO.putStrLn "Float In and DCE"
  TIO.putStrLn "=================="
  printExprPair (Lith.Datasolved.Show.showFunction decl')
  pure decl'

stepConstantFold :: Datasolved.FunctionDeclaration -> IO Datasolved.FunctionDeclaration
stepConstantFold decl0 = do
  let ann = Lith.ConstantFold.Decorate.decorateFunctionDeclaration decl0
  TIO.putStrLn "===================="
  TIO.putStrLn "Constant Folding Ann"
  TIO.putStrLn "===================="
  printExprPair (Lith.ConstantFold.Show.showFunction ann)
  let decl1 = Lith.ConstantFold.Run.run ann
  TIO.putStrLn "=================="
  TIO.putStrLn "Constant Folding"
  TIO.putStrLn "=================="
  printExprPair (Lith.Datasolved.Show.showFunction decl1)
  pure decl1

printExprPair :: (E.Eexpr (), E.Eexpr ()) -> IO ()
printExprPair (a,b) = do
  TIO.putStrLn (normalText a)
  TIO.putStrLn (normalText b)
