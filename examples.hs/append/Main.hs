---
-- Fix point for "append"
--
-- $Id: Main.hs,v 1.5 2003-07-31 11:12:35 raz Exp $
--


-- ----> this is old! <----
-- A0 := { [a,b,c] : a = 0 && b = c };
-- AR := { [a,b,c] -> [a',b',c'] : a >= 0 && b >= 0 && c >= 0 &&
--                                 a' >= 0 && b' >= 0 && c' >= 0 &&
--                                 a' = a - 1 && b' = b && c = c' + 1 };

-- A0;
-- AR;
-- AR+;

-- A := { [a,b,c] : (a = 0 && b = c) ||
--                 exists ( a', b', c' : 0 <= a' < a && a <= c + a' && 0 <= b && b' = b && c' = c - a + a' &&
--                         a' = 0 && b' = c' ) };
-- A;
-- hull A;
-- ----> this is old! <----

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
       Omega_stub.relation_print ptr_r'
       relation_extract_rformula ptr_r'

bup_one_step :: RFormula -> RFormula -> Int -> IO RFormula
bup_one_step r1 rec i =
    do let r2 = RFormula (\a -> RFormula (\b -> RFormula (\c -> Formula (
		        (toFormula (apply r1 [a, b, c])) Omega.||
                         Exists (\a' -> Exists (\b' -> Exists (\c' ->
                             (toFormula (apply rec [a, b, c, a', b', c'])) Omega.&& (toFormula (apply r1 [a', b', c']))))) ))))
--       putStr ("r2: " ++ (eval_RFormula ["a", "b", "c"] r2 ) ++ "\n")
       r2_subset_r1 <- subset (["a", "b", "c"], [], r2) (["a", "b", "c"], [], r1)
       putStr ("r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show r2_subset_r1) ++ "\n")
       ch_r2 <- convex_hull (["a", "b", "c"], [], r2)
       putStr ("ch_r"++ (show i) ++"': " ++ (eval_RFormula ["a", "b", "c"] ch_r2 ) ++ "\n")
       ch_r2_subset_r1 <- subset (["a", "b", "c"], [], ch_r2) (["a", "b", "c"], [], r1)
       putStr ("ch_r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show ch_r2_subset_r1) ++ "\n")
       return ch_r2

tdown_one_step r1 rec i =
    do let r2 = RFormula (\a1 -> RFormula (\b1 -> RFormula (\c1 -> 
		RFormula (\a2 -> RFormula (\b2 -> RFormula (\c2 -> Formula (
		        (toFormula (apply r1 [a1, b1, c1, a2, b2, c2])) Omega.||
                         Exists (\a3 -> Exists (\b3 -> Exists (\c3 ->
                             (toFormula (apply rec [a1, b1, c1, a3, b3, c3])) Omega.&& (toFormula (apply r1 [a3, b3, c3, a2, b2, c2]))))) )))))))
       r2_subset_r1 <- subset (["a", "b", "c"], ["a'", "b'", "c'"], r2) (["a", "b", "c"], ["a'", "b'", "c'"], r1)
       putStr ("r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show r2_subset_r1) ++ "\n")
       ch_r2 <- convex_hull (["a", "b", "c"], ["a'", "b'", "c'"], r2)
       putStr ("ch_r"++ (show i) ++"': " ++ (eval_RFormula ["a", "b", "c", "a'", "b'", "c'"] ch_r2 ) ++ "\n")
       ch_r2_subset_r1 <- subset (["a", "b", "c"], ["a'", "b'", "c'"], ch_r2) (["a", "b", "c"], ["a'", "b'", "c'"], r1)
       putStr ("ch_r"++ (show i) ++" subset r"++ (show (i - 1)) ++": "++ (show ch_r2_subset_r1) ++ "\n")
       return ch_r2

main = do
    putStr "[START]\n"

    let	append_0 = extract_rformula "{ [a,b,c] a = 0 && b = c }"
    let	append_r = extract_rformula "{ [a,b,c] -> [a',b',c'] a >= 0 && b >= 0 && c >= 0 && a' >= 0 && b' >= 0 && c' >= 0 && a' = a - 1 && b' = b && c = c' + 1 }"

--    append_tc_r <- transitive_closure (["a", "b", "c"], ["a'", "b'", "c'"], append_r)
--    putStr ("tc_append_r: " ++ (eval_RFormula ["a", "b", "c", "a'", "b'", "c;"] append_tc_r) ++ "\n")

    putStr "Bottom-up:\n"
    bup_append_1 <- bup_one_step append_0 append_r 1
    bup_append_2 <- bup_one_step bup_append_1 append_r 2
    bup_append_3 <- bup_one_step bup_append_2 append_r 3
    let bup_append_4 = extract_rformula "{[a,b,c] b+a = c && b <= c }"
    bup_append_5 <- bup_one_step bup_append_4 append_r 5

    putStr "Top-down:\n"
    tdown_append_1 <- tdown_one_step append_r append_r 1
    tdown_append_2 <- tdown_one_step tdown_append_1 append_r 2
    tdown_append_3 <- tdown_one_step tdown_append_2 append_r 3
    let tdown_append_4 = extract_rformula "{[a,b,c] -> [a',b',c'] c'+a = a'+c && b = b' && 0 <= c' < c && 0 <= b' && 0 <= a' }"
    tdown_append_5 <- tdown_one_step tdown_append_4 append_r 5

    putStr "[DONE]\n"



