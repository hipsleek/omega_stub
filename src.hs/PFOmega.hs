module PFOmega where

import Omega
import Omega_types
import Omega_parser
import Foreign
import Foreign.C
import qualified Omega_stub
import Prelude hiding ((&&),(||))

class Function a b c where
    apply :: a -> b -> c

instance Function RFormula [Variable] RFormula where
    apply rf [] = rf
    apply (RFormula rf) (var:vars) = (apply (rf var) vars)

to_formula :: RFormula -> Formula
to_formula (Formula f) = f	  
to_formula (RFormula rf) =
    let (str, _) = omega_show (RFormula rf) 1
    in error ("ERROR: You are trying to to_Formula-ing a RFormula not implemented!\n" ++ str)

f_apply :: RFormula -> [Variable] -> Formula
f_apply r vars = to_formula (apply r vars)

transitive_closure :: Relation -> IO RFormula
transitive_closure (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
       ptr_tc_r <- Omega_stub.transitive_closure1 ptr_r
       relation_extract_rformula ptr_tc_r


subset :: Relation -> Relation -> IO Bool
subset (ins1, outs1, rf1) (ins2, outs2, rf2) =
    do (ptr_r1, f1) <- build_relation (ins1, outs1, rf1)
       eval_relation ptr_r1 f1
       (ptr_r2, f2) <- build_relation (ins2, outs2, rf2)
       eval_relation ptr_r2 f2
       Omega_stub.must_be_subset ptr_r1 ptr_r2

difference :: Relation -> Relation -> IO RFormula
difference (ins1, outs1, rf1) (ins2, outs2, rf2) =
    do (ptr_r1, f1) <- build_relation (ins1, outs1, rf1)
       eval_relation ptr_r1 f1
       (ptr_r2, f2) <- build_relation (ins2, outs2, rf2)
       eval_relation ptr_r2 f2
       ptr_diff_r <- Omega_stub.difference ptr_r1 ptr_r2
       relation_extract_rformula ptr_diff_r

rformula_print :: Relation -> IO ()
rformula_print (ins, outs, rf) =
    do -- putStr ((show rf) ++ "\n")
       (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
--       Omega_stub.relation_print ptr_r
       Omega_stub.relation_print_with_subs ptr_r True

rformula_print_formula_to_string :: RFormula -> [String] -> String
rformula_print_formula_to_string (Formula f) _ = let (str, _) = omega_show f 1 in str
rformula_print_formula_to_string (RFormula rf) (var:vars) = rformula_print_formula_to_string (rf (var, nullPtr)) vars
rformula_print_formula_to_string (Union rfs) vars = 
  case rfs of
    [] -> ""
    [rf] -> rformula_print_formula_to_string rf vars
    (rf:rfs) -> (rformula_print_formula_to_string rf vars) ++ " union " ++ 
      (rformula_print_formula_to_string (Union rfs) vars)

convex_hull :: Relation -> IO RFormula
convex_hull (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
--       Omega_stub.relation_print ptr_r
       ptr_r' <- Omega_stub.convex_hull ptr_r
--       Omega_stub.relation_print ptr_r'
       relation_extract_rformula ptr_r'


gist:: Relation -> Relation -> IO RFormula
gist (ins1,outs1,rf1) (ins2,outs2,rf2) =
  build_relation(ins1,outs1,tr "#RF1#" rf1) >>= \(ptr_r1,f1) ->
  eval_relation ptr_r1 f1 >>
  build_relation(ins2,outs2,tr "#RF2#" rf2) >>= \(ptr_r2,f2) ->
  eval_relation ptr_r2 f2 >>
  Omega_stub.gist ptr_r1 ptr_r2 >>= \ptr_gist_r ->
  relation_extract_rformula ptr_gist_r >>= \rf -> return (tr "#GIST#" rf)

simplify:: Relation -> IO RFormula
simplify (ins1,outs1,rf1) =
  build_relation(ins1,outs1,rf1) >>= \(ptr_r1,f1) ->
  eval_relation ptr_r1 f1 >>
  Omega_stub.relation_simplify1 ptr_r1 >>
  relation_extract_rformula ptr_r1
