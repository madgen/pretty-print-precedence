{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Main where

import Prelude hiding ((<>))

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Test.QuickCheck

import Criterion.Main

#ifndef STRING

import Text.PrettyPrint

#else

type Doc = String

text = id

s1 <> s2  = s1 ++ s2
s1 <+> s2 = s1 ++ ' ' : s2

parens str = '(' : str ++ ")"

#endif

--------------------------------------------------------------------------------
-- AST in the spirit of Hutton's Razor
--------------------------------------------------------------------------------

data Prop =
    PVar String
  | PUnary  Op Prop
  | PBinary Op Prop Prop

data Op = Neg | And deriving (Eq, Ord)

data SomeOp = NoOp | SomeOp Op deriving (Eq, Ord)

operator :: Prop -> SomeOp
operator PVar{}           = NoOp
operator (PUnary  op _)   = SomeOp op
operator (PBinary op _ _) = SomeOp op

-- Use Template Haskell to generate a version of Prop that is suitable for
-- recursion schemes.
makeBaseFunctor ''Prop

--------------------------------------------------------------------------------
-- Pretty print without recursion schemes
--------------------------------------------------------------------------------

prettyOp :: Op -> Doc
prettyOp Neg = "!"
prettyOp And = "&&"

wrongPrint :: Prop -> Doc
wrongPrint = go
  where
  go :: Prop -> Doc
  go (PVar v)                   = text v
  go (PUnary  op child)         = prettyOp op <> go child
  go (PBinary op child1 child2) = go child1 <+> prettyOp op <+> go child2

redundantPrint :: Prop -> Doc
redundantPrint = go
  where
  go :: Prop -> Doc
  go (PVar v)                   = text v
  go (PUnary  op child)         = prettyOp op <> parens (go child)
  go (PBinary op child1 child2) = go child1 <+> prettyOp op <+> go child2

smartPrint :: Prop -> Doc
smartPrint = go
  where
  go :: Prop -> Doc
  go (PVar v) = text v
  go (PUnary  op child) =
    prettyOp op <> precParens (SomeOp op) (operator child) (go child)
  go (PBinary op child1 child2) =
    precParens (SomeOp op) (operator child1) (go child1) <+>
    prettyOp op <+>
    precParens (SomeOp op) (operator child2) (go child2)

precParens :: SomeOp -> SomeOp -> Doc -> Doc
precParens op1 op2 doc =
  if op1 >= op2
    then doc
    else parens doc

--------------------------------------------------------------------------------
-- Pretty print with recursion schemes
--------------------------------------------------------------------------------

-- Erroneously, pretty print without parantheses
wrongPrintF :: Prop -> Doc
wrongPrintF = cata alg
  where
  alg :: Base Prop Doc -> Doc
  alg (PVarF v)                = text v
  alg (PUnaryF  op doc)        = prettyOp op <> doc
  alg (PBinaryF op doc1 doc2)  = doc1 <+> prettyOp op <+> doc2

-- Correctly, pretty print with parantheses but very conservatively
redundantPrintF :: Prop -> Doc
redundantPrintF = cata alg
  where
  alg :: Base Prop Doc -> Doc
  alg (PVarF v)               = text v
  alg (PUnaryF  op doc)       = prettyOp op <> parens doc
  alg (PBinaryF op doc1 doc2) = doc1 <+> prettyOp op <+> doc2

-- Correctly, pretty print just the necessary amount of parantheses
smartPrintF :: Prop -> Doc
smartPrintF = para alg
  where
  alg :: Base Prop (Prop, Doc) -> Doc
  alg (PVarF v) = text v
  alg (PUnaryF  op (child, doc)) =
    prettyOp op <> precParens (SomeOp op) (operator child) doc
  alg (PBinaryF op (child1, doc1) (child2, doc2)) =
    precParens (SomeOp op) (operator child1) doc1 <+>
    prettyOp op <+>
    precParens (SomeOp op) (operator child2) doc2

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

pattern PNot p     = PUnary  Neg p
pattern PAnd p1 p2 = PBinary And p1 p2

-- X && Y
ex1 :: Prop
ex1 = PNot (PAnd (PVar "X") (PVar "Y"))

-- ! (!!X && Y)
ex2 :: Prop
ex2 = PNot (PAnd (PNot (PNot (PVar "X"))) (PVar "Y"))

--------------------------------------------------------------------------------
-- Benchmarking
--------------------------------------------------------------------------------

instance Arbitrary Prop where
  arbitrary = oneof
    [ return (PVar "x")
    , PAnd <$> arbitrary <*> arbitrary
    , PNot <$> arbitrary ]

propDepth :: Prop -> Int
propDepth = go
  where
  go PVar{}                     = 0
  go (PUnary _ child)           = 1 + go child
  go (PBinary  _ child1 child2) = 1 + max (go child1) (go child2)

propDepthF :: Prop -> Int
propDepthF = cata alg
  where
  alg PVarF{}                    = 0
  alg (PUnaryF  _ depth)         = 1 + depth
  alg (PBinaryF _ depth1 depth2) = 1 + max depth1 depth2

#if defined(PROFILING)

main :: IO ()
main = do
  !prop <- head . filter ((> 50) . propDepth) <$> generate infiniteList
  putStrLn . render $ {-# SCC vanilla_recursion #-} smartPrint  prop
  putStrLn . render $ {-# SCC recursion_schemes #-} smartPrintF prop

#else

main :: IO ()
main = do
  !propsNDepths <- take 5
                 . filter ((< 200) . fst)
                 . filter ((> 100) . fst)
                 . map (\prop -> (propDepth prop, prop))
               <$> generate infiniteList
  defaultMain $
    flip map (zip [1..] propsNDepths) $ \(id, (depth, prop)) ->
      bgroup (show id ++ '-' : show depth)
        [ bench "vanilla-smart"       $ nf smartPrint prop
        , bench "para-smart"          $ nf smartPrintF prop
        , bench "vanilla-redundant"   $ nf redundantPrint prop
        , bench "cata-reundant"       $ nf redundantPrintF prop
        , bench "vanilla-propDepth"   $ nf propDepth prop
        , bench "cata-propDepth"      $ nf propDepthF prop
        ]

#endif
