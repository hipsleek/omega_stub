---
-- Fix point for "append"
--
-- $Id: Main.hs,v 1.1 2003-07-10 06:51:02 raz Exp $
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
import Foreign
import Foreign.C
import qualified Omega_stub
import Prelude hiding ((&&),(||))


main = do
    print "[START]"

    let	append_0 = RFormula (\a -> RFormula (\b -> RFormula (\c -> Formula ( (a `eq` (0::Int)) && (b `eq` c) ))))
    let	append_r = RFormula (\a -> RFormula (\b -> RFormula (\c -> RFormula (\a' -> RFormula (\b' -> RFormula (\c' -> Formula (
                                                                       (a `geq` (0::Int)) && (b `geq` (0::Int)) && (c `geq` (0::Int)) &&
								       (a' `geq` (0::Int)) && (b' `geq` (0::Int)) && (c' `geq` (0::Int)) &&
								       (a' `eq` (a `plus` (- (1::Int)))) && (b' `eq` b) && (c `eq` (c' `plus` (1::Int))) )))))))

    (append_ptr_0, append_f_0) <- build_relation (["a", "b", "c"], [], append_0)
    (append_ptr_r, append_f_r) <- build_relation (["a", "b", "c"], ["a'", "b'", "c'"], append_r)

--    print (show append_f_0)
--    print (show append_f_r)

    eval_relation append_ptr_0 append_f_0
    eval_relation append_ptr_r append_f_r

    Omega_stub.relation_print append_ptr_0
    Omega_stub.relation_print append_ptr_r
    tc_append_ptr_r <- Omega_stub.transitive_closure1 append_ptr_r
    Omega_stub.relation_print tc_append_ptr_r


    let	append_r' = RFormula (\a -> RFormula (\b -> RFormula (\c -> Formula (
                                                                       ( (a `eq` (0::Int)) && (b `eq` c) ) ||
								       Exists (\a' -> Exists (\b' -> Exists (\c' ->
								           (a `geq` (0::Int)) && ((a `plus` (1::Int)) `geq` a') && ((c `plus` a') `geq` a) &&
									   (b `geq` (0::Int)) && (b' `eq` b) && (c' `eq` (c `minus` a `plus` a')) &&
									   (a' `eq` (0::Int)) && (b' `eq` c')
			     )))))))
    (append_ptr_r', append_f_r') <- build_relation (["a", "b", "c"], [], append_r')
    eval_relation append_ptr_r' append_f_r'
    Omega_stub.relation_print append_ptr_r'
    hull_append_r' <- Omega_stub.hull0 append_ptr_r'
    Omega_stub.relation_print hull_append_r'

    print "[DONE]"


