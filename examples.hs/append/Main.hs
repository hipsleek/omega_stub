---
-- Fix point for "append"
--
-- $Id: Main.hs,v 1.4 2003-07-29 05:13:05 raz Exp $
--


-- A0 := { [a,b,c] : a = 0 && b = c };
-- AR := { [a,b,c] -> [a',b',c'] : a >= 0 && b >= 0 && c >= 0 &&
--                                 a' >= 0 && b >= 0 && c' >= 0 &&
--                                 a' = a - 1 && b' = b && c = c' + 1 };

-- A0;
-- AR;
-- AR+;

-- A := { [a,b,c] : (a = 0 && b = c) ||
--                 exists ( a', b', c' : 0 <= a' < a && a <= c + a' && 0 <= b && b' = b && c' = c - a + a' &&
--                         a' = 0 && b' = c' ) };
-- A;
-- hull A;

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

main = do
    putStr "[START]\n"

    let	append_0 = extract_rformula "{ [a,b,c] a = 0 && b = c }"
    let	append_r = extract_rformula "{ [a,b,c] -> [a',b',c'] a >= 0 && b >= 0 && c >= 0 && a' >= 0 && b >= 0 && c' >= 0 && a' = a - 1 && b' = b && c = c' + 1 }"

--    print ("append_0: " ++ (show append_0) ++ "\n")
--    print ("append_r: " ++ (show append_r) ++ "\n")

    (append_ptr_0, append_f_0) <- build_relation (["a", "b", "c"], [], append_0)
    (append_ptr_r, append_f_r) <- build_relation (["a", "b", "c"], ["a'", "b'", "c'"], append_r)

--    print ("append_f_0: " ++ (show append_f_0) ++ "\n")
--    print ("append_f_r: " ++ (show append_f_r) ++ "\n")

    eval_relation append_ptr_0 append_f_0
    eval_relation append_ptr_r append_f_r

    Omega_stub.relation_print append_ptr_0
    Omega_stub.relation_print append_ptr_r
    tc_append_ptr_r <- Omega_stub.transitive_closure1 append_ptr_r
--    putStr "tc_append_ptr_r: \n"
    Omega_stub.relation_print tc_append_ptr_r

    append_tc_r <- relation_extract_rformula tc_append_ptr_r
    putStr ("tc_append_r: " ++ (show (toFormula (apply append_tc_r [("a", nullPtr::Ptr Omega_stub.Variable),("b", nullPtr),("c", nullPtr),("a'", nullPtr::Ptr Omega_stub.Variable),("b'", nullPtr),("c'", nullPtr)]))) ++ "\n")

--    let append_r' = extract_rformula "{[a,b,c] (a = 0 && b = c) || exists (a', b', c' : b' = b && a+c' = a'+c && 0 <= a' && a' + 1 <= a && a <= a'+c && 0 <= b && a' = 0 && b' = c') }"
    let append_r' = RFormula (\a -> RFormula (\b -> RFormula (\c -> Formula (
		        ((a `eq` (0::Int)) Omega.&& (b `eq` c)) Omega.||
                         Exists (\a' -> Exists (\b' -> Exists (\c' ->
                             (toFormula (apply append_tc_r [a, b, c, a', b', c'])) Omega.&& (toFormula (apply append_0 [a', b', c']))))) ))))
--    putStr ("append_r': " ++ (show append_r') ++ "\n")
    (append_ptr_r', append_f_r') <- build_relation (["a", "b", "c"], [], append_r')
    eval_relation append_ptr_r' append_f_r'
    Omega_stub.relation_print append_ptr_r'

    hull_append_ptr_r' <- Omega_stub.hull0 append_ptr_r'
    Omega_stub.relation_print hull_append_ptr_r'
    hull_append_r' <- relation_extract_rformula hull_append_ptr_r'
--    putStr ("hull_append_r': " ++ (show (toFormula (apply hull_append_r' [("a", nullPtr::Ptr Omega_stub.Variable),("b", nullPtr),("c", nullPtr)]))) ++ "\n")
    putStr ("hull_append_r': " ++ (eval_RFormula ["a", "b", "c"] hull_append_r' ) ++ "\n")
    putStr ("hull_append_r': " ++  (show hull_append_r') ++ "\n")


--    Omega_stub.relation_print (apply hull_append_r' [("a", nullPtr),("b", nullPtr),("c", nullPtr)])

    putStr "[DONE]\n"


