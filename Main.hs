{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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
  | PAnd Prop Prop
  | PNot Prop

-- Use Template Haskell to generate a version of Prop that is suitable for
-- recursion schemes.
makeBaseFunctor ''Prop

--------------------------------------------------------------------------------
-- Pretty print without recursion schemes
--------------------------------------------------------------------------------

wrongPrint :: Prop -> Doc
wrongPrint = go
  where
  go :: Prop -> Doc
  go (PVar v)             = text v
  go (PAnd child1 child2) = go child1 <+> "&&" <+> go child2
  go (PNot child)         = "!" <> go child

redundantPrint :: Prop -> Doc
redundantPrint = go
  where
  go :: Prop -> Doc
  go (PVar v)             = text v
  go (PAnd child1 child2) = go child1 <+> "&&" <+> go child2
  go (PNot child)         = "!" <> parens (go child)

smartPrint :: Prop -> Doc
smartPrint = go
  where
  go :: Prop -> Doc
  go (PVar v) = text v
  go parent@(PAnd child1 child2) =
    precParens parent child1 (go child1) <+> "&&" <+> precParens parent child2 (go child2)
  go parent@(PNot child) = "!" <> precParens parent child (go child)

  precedence :: Prop -> Int
  precedence PVar{} = 1
  precedence PNot{} = 2
  precedence PAnd{} = 3

  precParens :: Prop -> Prop -> Doc -> Doc
  precParens parent child doc =
    if precedence parent >= precedence child
      then doc
      else parens doc

--------------------------------------------------------------------------------
-- Pretty print with recursion schemes
--------------------------------------------------------------------------------

-- Erroneously, pretty print without parantheses
wrongPrint' :: Prop -> Doc
wrongPrint' = cata alg
  where
  alg :: Base Prop Doc -> Doc
  alg (PVarF v)         = text v
  alg (PAndF doc1 doc2) = doc1 <+> "&&" <+> doc2
  alg (PNotF doc)       = "!" <> doc

-- Correctly, pretty print with parantheses but very conservatively
redundantPrint' :: Prop -> Doc
redundantPrint' = cata alg
  where
  alg :: Base Prop Doc -> Doc
  alg (PVarF v)         = text v
  alg (PAndF doc1 doc2) = doc1 <+> "&&" <+> doc2
  alg (PNotF doc)       = "!" <> parens doc

-- Correctly, pretty print just the necessary amount of parantheses
smartPrint' :: Prop -> Doc
smartPrint' = para alg
  where
  alg :: Base Prop (Prop, Doc) -> Doc
  alg (PVarF v) = text v
  alg parent@(PAndF (child1, doc1) (child2, doc2)) =
    precParens parent child1 doc1 <+> "&&" <+> precParens parent child2 doc2
  alg parent@(PNotF (child, doc)) = "!" <> precParens parent child doc

  precedence :: Base Prop a -> Int
  precedence PVarF{} = 1
  precedence PNotF{} = 2
  precedence PAndF{} = 3

  precParens :: Base Prop a -> Prop -> Doc -> Doc
  precParens parentF child doc =
    if precedence parentF >= (precedence . project) child
      then doc
      else parens doc

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

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
propDepth = cata alg
  where
  alg PVarF{}               = 0
  alg (PAndF depth1 depth2) = 1 + max depth1 depth2
  alg (PNotF depth)         = 1 + depth

propDepth' :: Prop -> Int
propDepth' = go
  where
  go PVar{}               = 0
  go (PAnd child1 child2) = 1 + max (go child1) (go child2)
  go (PNot child)         = 1 + go child

#if defined(PROFILING)

main :: IO ()
main = do
  !prop <- head . filter ((> 50) . propDepth) <$> generate infiniteList
  putStrLn . render $ {-# SCC vanilla_recursion #-} smartPrint  prop
  putStrLn . render $ {-# SCC recursion_schemes #-} smartPrint' prop

#elif defined(PROP_DEPTH)

main :: IO ()
main = do
  !propsNDepths <- take 10
                 . filter ((< 200) . fst)
                 . filter ((> 100) . fst)
                 . map (\prop -> (propDepth prop, prop))
               <$> generate infiniteList
  defaultMain $
    flip map (zip [1..] propsNDepths) $ \(id, (depth, prop)) ->
      bgroup (show id ++ '-' : show depth)
        [ bench "vanilla-propDepth" $ whnf propDepth prop
        , bench "para-propDepth" $ whnf propDepth prop
        ]

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
        [ bench "vanilla-smart" $ whnf smartPrint prop
        , bench "para-smart" $ whnf smartPrint' prop
        , bench "vanilla-redundant" $ whnf redundantPrint prop
        , bench "cata-reundant" $ whnf redundantPrint' prop
        ]

#endif
