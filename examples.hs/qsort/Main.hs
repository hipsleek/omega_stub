---
-- Fix point for "qsort"
--
-- $Id: Main.hs,v 1.4 2003-08-19 12:12:17 raz Exp $
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
       ch_r2 <- convex_hull (["a", "b"], [], r2)
       putStr ("ch_r"++ (show i) ++": " ++ (rformula_print_formula_to_string ch_r2 ["a", "b"]) ++ "\n")
       ch_r2_subset_r1 <- subset (["a", "b"], [], ch_r2) (["a", "b"], [], r1)
       putStr ("ch_r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show ch_r2_subset_r1) ++ "\n")
       return ch_r2

tdown_one_step :: RFormula -> RFormula -> Int -> IO RFormula
tdown_one_step r1 rec i =
    do let r2 = RFormula (\a1 -> RFormula (\b1 -> RFormula (\a2 -> RFormula (\b2 -> Formula (
		        (f_apply r1 [a1, b1, a2, b2]) Omega.||
                         Exists (\a3 -> Exists (\b3 -> Exists (\a4 -> Exists (\b4 ->
                             (f_apply rec [a1, b1, a3, b3, a4, b4]) Omega.&&
			     ((f_apply r1 [a3, b3, a2, b2]) Omega.||
			      (f_apply r1 [a4, b4, a2, b2])) )))) )))))
       ch_r2 <- convex_hull (["a", "b"], ["a'", "b'"], r2)
       putStr ("ch_r"++ (show i) ++": " ++ (rformula_print_formula_to_string ch_r2 ["a", "b", "a'", "b'"]) ++ "\n")
       ch_r2_subset_r1 <- subset (["a", "b"], ["a'", "b'"], ch_r2) (["a", "b"], ["a'", "b'"], r1)
       putStr ("ch_r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show ch_r2_subset_r1) ++ "\n")
       return ch_r2

main = do
    putStr "[START]\n"

    let part = extract_rformula "{ [a]->[b,c] a = b + c && a >= 0 && b >= 0 && c >= 0}"
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
							      (b1 `eq` ac )) ))))) ] )))))))

    putStr "Bottom-up:\n"
    bup_qsort_1 <- bup_one_step qsort_0 qsort_r 1
    bup_qsort_2 <- bup_one_step bup_qsort_1 qsort_r 2
    bup_qsort_3 <- bup_one_step bup_qsort_2 qsort_r 3
    bup_qsort_4 <- bup_one_step bup_qsort_3 qsort_r 4
    bup_qsort_5 <- bup_one_step bup_qsort_4 qsort_r 5

    let bup_append_6 = extract_rformula "{ [a,b] b = a && b >= 0 }"
    bup_append_7 <- bup_one_step bup_append_6 qsort_r 7

    putStr "Top-down:\n"
    let tdown_qsort_0 = RFormula (\a1 -> RFormula (\b1 -> RFormula (\a2 -> RFormula (\b2 -> Formula ( And [
                         Exists (\a3 -> Exists (\b3 -> 
                             (f_apply qsort_r [a1, b1, a2, b2, a3, b3]) Omega.||
                             (f_apply qsort_r [a1, b1, a3, b3, a2, b2]) )) ] )))))

    tdown_qsort_1 <- tdown_one_step tdown_qsort_0 qsort_r 1
    tdown_qsort_2 <- tdown_one_step tdown_qsort_1 qsort_r 2
    tdown_qsort_3 <- tdown_one_step tdown_qsort_2 qsort_r 3

    let tdown_qsort = RFormula (\a1 -> RFormula (\b1 -> Formula ( And [
                         Exists (\a2 -> Exists (\b2 -> (f_apply tdown_qsort_3 [a1, b1, a2, b2]) Omega.&& (f_apply qsort_0 [a2, b2]) )) ] )))
    rformula_print (["a", "b"], [], tdown_qsort)

    putStr "[DONE]\n"


