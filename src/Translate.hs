module Translate where

-- translate a generalized regexp into a SMT formula

import Z3.Monad

import GRegexp

-- prepare an array of n integers in range [0, size-1]
-- returns (array, bounds formula)
prepare :: AST -> Int -> Z3 (AST, AST)
prepare vsize n = do
  int <- mkIntSort
  _0 <- mkInt (-1) int
  ary <- makeConstantArray int i
  bounds <- mkTrue
  let loop ary bounds i = 
        if i == n 
        then return (ary, bounds)
        else do
          fint <- mkFreshIntVar ('w' : show i)
          fint_lower <- mkLt _0 fint
          fint_upper <- mkLt fint vsize
          bounds' <- mkAnd [bounds, fint_lower, fint_upper]
          vi <- mkInt i int
          ary' <- mkStore ary vi fint
          loop ary' bound' (i+1)
  in  loop ary 0

prepareWord :: Z3 AST
prepareWord = do
  int <- mkIntSort
  intIntArray <- mkArraySort int int
  mkFreshConst "word" intIntArray

tr :: AST -> AST -> AST -> GRE Int -> Z3 AST
tr word i j re = case re of
  Zero -> mkFalse
  One  -> mkEq i j
  Atom t -> do
    int <- mkIntSort
    _1 <- mkInt 1 int
    i1 <- mkAdd [i, _1]
    ij <- mkEq j i1
    wi <- mkSelect word i
    wit <- mkEq wi t
    mkAnd [ij, wit]
  Dot r s -> do
    int <- mkIntSort
    k <- mkFreshIntVar "k"
    phi_r <- tr word i k r
    phi_s <- tr word k j s
    mkAnd [phi_r, phi_s]
  Or r s -> do
    phi_r <- tr word i j r
    phi_s <- tr word i j s
    mkOr [phi_r, phi_s]
  And r s -> do
    phi_r <- tr word i j r
    phi_s <- tr word i j s
    mkAnd [phi_r, phi_s]
  Not r -> do
    phi_r <- tr word i j r
    mkNot phi_r
  Star r -> do
    int <- mkIntSort
    idxes <- makeConstantArray int i
