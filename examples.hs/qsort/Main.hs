---
-- Fix point for "qsort"
--
-- $Id: Main.hs,v 1.1 2003-08-01 10:36:53 raz Exp $
--

module Main(main) where

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
--    apply rf [] = rf
    apply rf [] = rf
    apply (RFormula rf) (var:vars) = (apply (rf var) vars)

toFormula :: RFormula -> Formula
toFormula (Formula f) = f	  

transitive_closure :: Relation -> IO RFormula
transitive_closure (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
--       Omega_stub.relation_setup_names ptr_r
--       Omega_stub.relation_finalize ptr_r
       ptr_tc_r <- Omega_stub.transitive_closure1 ptr_r
       relation_extract_rformula ptr_tc_r


subset :: Relation -> Relation -> IO Bool
subset (ins1, outs1, rf1) (ins2, outs2, rf2) =
    do (ptr_r1, f1) <- build_relation (ins1, outs1, rf1)
       eval_relation ptr_r1 f1
       (ptr_r2, f2) <- build_relation (ins2, outs2, rf2)
       eval_relation ptr_r2 f2
       Omega_stub.must_be_subset ptr_r1 ptr_r2

convex_hull :: Relation -> IO RFormula
convex_hull (ins, outs, rf) =
    do (ptr_r, f) <- build_relation (ins, outs, rf)
       eval_relation ptr_r f
       ptr_r' <- Omega_stub.convex_hull ptr_r
       relation_extract_rformula ptr_r'

f_apply r vars = toFormula (apply r vars)

bup_one_step :: RFormula -> RFormula -> Int -> IO RFormula
bup_one_step r1 rec i =
    do let r2 = RFormula (\a1 -> RFormula (\b1 -> Formula (
		        (f_apply r1 [a1, b1]) Omega.||
                         Exists (\a2 -> Exists (\b2 -> Exists (\a3 -> Exists (\b3 ->
                             (f_apply rec [a1, b1, a2, b2, a3, b3]) Omega.&&
			     (f_apply r1 [a2, b2]) Omega.&&
			     (f_apply r1 [a3, b3]) ))) ))))
       putStr ("r"++ (show i) ++": " ++ (eval_RFormula ["a", "b"] r2 ) ++ "\n") 
       r2_subset_r1 <- subset (["a", "b"], [], r2) (["a", "b"], [], r1)
       putStr ("r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show r2_subset_r1) ++ "\n")
       return r2
--       ch_r2 <- convex_hull (["a", "b"], [], r2)
--       putStr ("ch_r"++ (show i) ++"': " ++ (eval_RFormula ["a", "b"] ch_r2 ) ++ "\n")
--       ch_r2_subset_r1 <- subset (["a", "b"], [], ch_r2) (["a", "b"], [], r1)
--       putStr ("ch_r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show ch_r2_subset_r1) ++ "\n")
--       return ch_r2


main = do
    putStr "[START]\n"

    let part = extract_rformula "{ [a]->[b,c] a = b + c && b, 2b-1 <= a <= 2b }"
	append = extract_rformula "{ [a,b]->[c] c = a + b && a >= 0 && b >= 0 }"
	qsort_0 = extract_rformula "{ [m,n] m = 0 && n = 0 }"
	qsort_r = RFormula (\m1 -> RFormula (\n1 -> RFormula (\m2 -> RFormula (\n2 -> RFormula (\m3 -> RFormula (\n3 -> Formula (
			Exists (\pa -> Exists (\pb -> Exists (\pc -> pa `eq` (m1 `minus` (1::Int)) Omega.&&
							      (f_apply part [pa, pb, pc]) Omega.&&
			Exists (\aa -> Exists (\ab -> Exists (\ac -> (aa `eq` n2) Omega.&&
							      (ab `eq` (n3 `plus` (1::Int))) Omega.&&
							      (f_apply append [aa, ab, ac])))) ))) )))))))

    putStr "Bottom-up:\n"
    bup_qsort_1 <- bup_one_step qsort_0 qsort_r 1
    bup_qsort_2 <- bup_one_step bup_qsort_1 qsort_r 2
--    bup_qsort_3 <- bup_one_step bup_qsort_2 qsort_r 3
--    let bup_append_4 = extract_rformula "{[a,b,c] b+a = c && b <= c }"
--    bup_append_5 <- bup_one_step bup_append_4 append_r 5


    putStr "[DONE]\n"


