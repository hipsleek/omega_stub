module PFOmega where

import Omega
import Omega_types
import Omega_parser
import Foreign
import Foreign.C
import qualified Omega_stub
import Prelude hiding ((&&),(||))

simplify:: Relation -> IO RFormula
simplify (ins1,outs1,rf1) =
  build_relation(ins1,outs1,rf1) >>= \(ptr_r1,f1) ->
  eval_relation ptr_r1 f1 >>
  Omega_stub.relation_simplify3 ptr_r1 2 4 >> --what is the meaning of 2 and 4???? (2 and 4 are used in Omega parser.y)
  relation_extract_rformula ptr_r1 >>= \rf ->
  return rf

subset :: Relation -> Relation -> IO Bool
subset (ins1, outs1, rf1) (ins2, outs2, rf2) =
    do (ptr_r1, f1) <- build_relation (ins1, outs1, rf1)
       eval_relation ptr_r1 f1
       (ptr_r2, f2) <- build_relation (ins2, outs2, rf2)
       eval_relation ptr_r2 f2
       Omega_stub.must_be_subset ptr_r1 ptr_r2

gist:: Relation -> Relation -> IO RFormula
gist (ins1,outs1,rf1) (ins2,outs2,rf2) =
--  putStrLn ("#RF1#" ++ show rf1) >>
  build_relation(ins1,outs1,rf1) >>= \(ptr_r1,f1) ->
  eval_relation ptr_r1 f1 >>
--  putStrLn ("#RF2#" ++ show rf2) >>
  build_relation(ins2,outs2,rf2) >>= \(ptr_r2,f2) ->
  eval_relation ptr_r2 f2 >>
  Omega_stub.gist ptr_r1 ptr_r2 >>= \ptr_gist_r ->
  relation_extract_rformula ptr_gist_r >>= \rf -> 
--  putStrLn ("#GIST#" ++ show rf) >>
  return rf

hull0 :: Relation -> IO RFormula
hull0 (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
       ptr_r' <- Omega_stub.hull0 ptr_r
       relation_extract_rformula ptr_r'

convex_hull :: Relation -> IO RFormula
convex_hull (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
--       Omega_stub.relation_print ptr_r
       ptr_r' <- Omega_stub.convex_hull ptr_r
--       Omega_stub.relation_print ptr_r'
       relation_extract_rformula ptr_r'

union_relation:: Relation -> Relation -> IO RFormula
union_relation (ins1,outs1,rf1) (ins2,outs2,rf2) =
  build_relation (ins1,outs1,rf1) >>= \(ptr_r1,f1) ->
  build_relation (ins2,outs2,rf2) >>= \(ptr_r2,f2) ->
  eval_relation ptr_r1 f1 >>
  eval_relation ptr_r2 f2 >>
  Omega_stub.union_relation ptr_r1 ptr_r2 >>= \ptr_union_r ->
  relation_extract_rformula ptr_union_r >>= \rf ->
  return rf

composition:: Relation -> Relation -> IO RFormula
composition (ins1,outs1,rf1) (ins2,outs2,rf2) =
  build_relation (ins1,outs1,rf1) >>= \(ptr_r1,f1) ->
  build_relation (ins2,outs2,rf2) >>= \(ptr_r2,f2) ->
  eval_relation ptr_r1 f1 >>
  eval_relation ptr_r2 f2 >>
  Omega_stub.composition ptr_r1 ptr_r2 >>= \ptr_compose_r ->
  relation_extract_rformula ptr_compose_r >>= \rf ->
  return rf


transitive_closure :: Relation -> IO RFormula
transitive_closure (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
       ptr_tc_r <- Omega_stub.transitive_closure1 ptr_r
       relation_extract_rformula ptr_tc_r

difference :: Relation -> Relation -> IO RFormula
difference (ins1, outs1, rf1) (ins2, outs2, rf2) =
    do (ptr_r1, f1) <- build_relation (ins1, outs1, rf1)
       eval_relation ptr_r1 f1
       (ptr_r2, f2) <- build_relation (ins2, outs2, rf2)
       eval_relation ptr_r2 f2
       ptr_diff_r <- Omega_stub.difference ptr_r1 ptr_r2
       relation_extract_rformula ptr_diff_r
------------------------

------------------------
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

