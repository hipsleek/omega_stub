---
-- Fix point for "qsort"
--
-- $Id: Main.hs,v 1.2 2003-08-03 15:05:00 raz Exp $
--

module Main(main) where

import Omega
import PFOmega
import Omega_types
import Omega_parser
import Foreign
import Foreign.C
import qualified Omega_stub
import Prelude hiding ((&&),(||))


bup_one_step :: RFormula -> RFormula -> Int -> IO RFormula
bup_one_step r1 rec i =
    do let r2 = RFormula (\a1 -> RFormula (\b1 -> Formula (
		        (f_apply r1 [a1, b1]) Omega.||
                         Exists (\a2 -> Exists (\b2 -> Exists (\a3 -> Exists (\b3 ->
                             (f_apply rec [a1, b1, a2, b2, a3, b3]) Omega.&&
			     (f_apply r1 [a2, b2]) Omega.&&
			     (f_apply r1 [a3, b3]) ))) ))))
--       putStr ("r"++ (show i) ++": " ++ (rformula_print_formula_to_string r2 ["a", "b"]) ++ "\n") 
--       rformula_print (["a", "b"], [], r2)
--       r2_subset_r1 <- subset (["a", "b"], [], r2) (["a", "b"], [], r1)
--       putStr ("r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show r2_subset_r1) ++ "\n")
--       return r2
       ch_r2 <- convex_hull (["a", "b"], [], r2)
       putStr ("ch_r"++ (show i) ++": " ++ (rformula_print_formula_to_string ch_r2 ["a", "b"]) ++ "\n")
       ch_r2_subset_r1 <- subset (["a", "b"], [], ch_r2) (["a", "b"], [], r1)
       putStr ("ch_r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show ch_r2_subset_r1) ++ "\n")
       return ch_r2


main = do
    putStr "[START]\n"

    let part = extract_rformula "{ [a]->[b,c] a = b + c && a >= 0 }"
	append = extract_rformula "{ [a,b]->[c] c = a + b && a >= 0 && b >= 0 }"
	qsort_0 = extract_rformula "{ [a,b] a = 0 && b = 0 }"
	qsort_r = RFormula (\a1 -> RFormula (\b1 -> RFormula (\a2 -> RFormula (\b2 -> RFormula (\a3 -> RFormula (\b3 -> Formula ( And [
			Exists (\pa -> Exists (\pb -> Exists (\pc -> pa `eq` (a1 `minus` (1::Int)) Omega.&&
							      (f_apply part [pa, pb, pc]) Omega.&&
							      (a2 `eq` pb) Omega.&&
							      (a3 `eq` pc) Omega.&&
			Exists (\aa -> Exists (\ab -> Exists (\ac -> (aa `eq` b2) Omega.&&
							      (ab `eq` (b3 `plus` (1::Int))) Omega.&&
							      (f_apply append [aa, ab, ac]) Omega.&&
							      (b1 `eq` (ac `plus` (1::Int))) )))))) ] )))))))

    putStr "Bottom-up:\n"
--    putStr ("part: " ++ (rformula_print_formula_to_string part ["a", "b", "c"]) ++ "\n")
--    putStr ("append: " ++ (rformula_print_formula_to_string append ["a", "b", "c"]) ++ "\n")
--    putStr ("qsort_0: " ++ (rformula_print_formula_to_string qsort_0 ["a", "b"]) ++ "\n")
--    putStr ("qsort_r: " ++ (rformula_print_formula_to_string qsort_r ["a1", "b1", "a2", "b2", "a3", "b3"]) ++ "\n")
--    rformula_print (["a", "b"], [], qsort_0)
--    rformula_print (["a1", "b1"], ["a2", "b2", "a3", "b3"], qsort_r)
    bup_qsort_1 <- bup_one_step qsort_0 qsort_r 1
    bup_qsort_2 <- bup_one_step bup_qsort_1 qsort_r 2
    bup_qsort_3 <- bup_one_step bup_qsort_2 qsort_r 3
    bup_qsort_4 <- bup_one_step bup_qsort_3 qsort_r 4
    bup_qsort_5 <- bup_one_step bup_qsort_4 qsort_r 5

    let bup_append_6 = extract_rformula "{ [a,b] b = 2a && a >= 0 }"
    bup_append_7 <- bup_one_step bup_append_6 qsort_r 7

    putStr "[DONE]\n"


