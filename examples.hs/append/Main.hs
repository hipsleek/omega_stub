---
-- Fix point for "append"
--
-- $Id: Main.hs,v 1.3 2003-07-28 15:57:19 raz Exp $
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


main = do
    print "[START]"

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

--    append_r' <- relation_extract_rformula tc_append_ptr_r
    let append_r' = extract_rformula "{[a,b,c] (a = 0 && b = c) || exists (a', b', c' : b' = b && a+c' = a'+c && 0 <= a' && a' + 1 <= a && a <= a'+c && 0 <= b && a' = 0 && b' = c') }"
--    putStr ("append_r': " ++ (show append_r') ++ "\n")
    (append_ptr_r', append_f_r') <- build_relation (["a", "b", "c"], [], append_r')
    eval_relation append_ptr_r' append_f_r'
    Omega_stub.relation_print append_ptr_r'

    hull_append_r' <- Omega_stub.hull0 append_ptr_r'
    Omega_stub.relation_print hull_append_r'

    print "[DONE]"


