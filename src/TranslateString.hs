module TranslateString where

-- translate a generalized regexp into a SMT formula as a string

import GRegexp

mkFalse = showString "false\n"
mkEq i j = showString "(= " . showString i . showString " " . showString j . showString ")\n"
mkAt i ch = showString "(at " . showString i . showString " " . showString ch . showString ")\n"

showPath [] = id
showPath (i:is) = showString "_" . shows i . showPath is
showTemp k p = showString k . showPath p

tr :: String -> String -> GRE Int -> [Int] -> (ShowS, ShowS)
tr i j re p = case re of
  Zero -> (id, mkFalse)
  One  -> (id, mkEq i j)
  Atom t -> (id,
    showString "(and " .
    mkAt i (show t) .
    mkEq j ("(+ 1 " ++ i ++ ")") .
    showString ")\n")
  Dot r s -> 
    let k = showTemp "k" p ""
        (decls_r, body_r) = tr i k r (0:p)
        (decls_s, body_s) = tr k j s (1:p)
    in
      (showString "(declare-const " . showString k . showString " Int)\n" . decls_r . decls_s,
       showString "(and " . body_r . showString " " . body_s . showString ")\n")
  Or r s ->
    let (decls_r, body_r) = tr i j r (0:p)
        (decls_s, body_s) = tr i j s (1:p)
    in
      (decls_r . decls_s,
       showString "(or " . body_r . showString " " . body_s . showString ")\n")
  And r s ->
    let (decls_r, body_r) = tr i j r (0:p)
        (decls_s, body_s) = tr i j s (1:p)
    in
      (decls_r . decls_s,
       showString "(and " . body_r . showString " " . body_s . showString ")\n")
  Not r -> 
    let (decls_r, body_r) = tr i j r (0:p)
    in
      (decls_r,
       showString "(not " . body_r . showString ")\n")
  Star re -> 
    let n = showTemp "n" p ""
        r = showTemp "r" p ""
        idx = showTemp "idx" p ""
        i' = showTemp "i" p ""
        j' = showTemp "j" p ""
        (decls_r, body_r) = tr i' j' re (0:p)
    in
      (showString "(declare-const " . showString n . showString " Int)\n" .
       showString "(declare-const " . showString idx . showString " (Array Int Int))\n" .
       decls_r,
       showString "(or\n" .
       showString "(and\n" .
      showString "(forall ((" . 
      showString r . showString " Int) (" .
      showString i' . showString " Int) (" .
      showString j' . showString " Int))\n" .
      showString "(and " .
      showString "(<= 0 " . showString r . showString ") " .
      showString "(< " . showString r . showString " " . showString n . showString ") " .
      showString "(= (select " . showString idx . showString " " . showString r . showString ") " . showString i' . showString ") " .
      showString "(= (select " . showString idx . showString " (+ 1 " . showString r . showString ")) " . showString j' . showString ") " .
      showString "\n" .
      body_r .
      showString "))\n" .
      showString "(= (select " . showString idx . showString " 0) " . showString i . showString ") " .
      showString "(= (select " . showString idx . showString " " . showString n . showString ") " . showString j . showString ")" .
      showString ")\n" .
      showString "(and " .
      showString "(= 0 " . showString n . showString ") " .
      showString "(= " . showString i . showString " " . showString j . showString ")" .
      showString "))"
      )

runtr :: GRE Int -> IO ()
runtr re =
    let
      (decls, body) = tr "0" "len" re []
      output_decls =
        showString "(declare-const len Int)\n" .
        showString "(declare-const word (Array Int Int))\n" .
        showString "(define-fun at ((i Int) (ch Int)) Bool (= (select word i) ch))\n" .
        decls
      output_body = showString "(assert " . body . showString ")\n"
    in do
      putStrLn (output_decls "")
      putStrLn (output_body "")

      
