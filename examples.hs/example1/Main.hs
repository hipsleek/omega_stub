---
-- This is an example using the new Omega module.
--
-- $Id: Main.hs,v 1.2 2003-07-22 11:19:57 raz Exp $
--

-- R1 := { [i] -> [j] : j = i + 1 };
-- R2 := { [i] -> [j] : j = i + 2 };
-- R1;
-- R2;
-- R1(R2);
-- R2(R1);
-- R1(R1);

module Main(main) where

import Omega
import Foreign
import Foreign.C
import qualified Omega_stub
import Omega_util

main = do
    putStr "[START]\n"

--    let	rf1 = RFormula (\i -> RFormula (\j -> Formula (And [Eq [Coef j (- 1), Coef i 1, Const 1]])))
--    let	rf2 = RFormula (\i -> RFormula (\j -> Formula (And [Eq [Coef j (- 1), Coef i 1, Const 2]])))

    let	rf1 = RFormula (\i -> RFormula (\j -> Formula (And [ j `eq` (i `plus` (1::Int)) ])))
    let	rf2 = RFormula (\i -> RFormula (\j -> Formula (And [ j `eq` (i `plus` (2::Int)) ])))


    (ptr_r1, rf1) <- build_relation (["x"], ["y"], rf1)
    (ptr_r2, rf2) <- build_relation (["x"], ["y"], rf2)

    print (show rf1)
    print (show rf2)

    eval_relation ptr_r1 rf1
    eval_relation ptr_r2 rf2

    Omega_stub.relation_print ptr_r1
    Omega_stub.relation_print ptr_r2
    ptr_r <- Omega_stub.composition ptr_r1 ptr_r2
    Omega_stub.relation_print ptr_r

--    r_ptr_string <- Omega_stub.relation_print_with_subs_to_string r False
--    r_string <- peekCString r_ptr_string
--    print r_string
    Omega_stub.relation_print_with_subs ptr_r False
    Omega_stub.relation_print ptr_r


    putStr "extract testing...\n"
    let	rf3 = RFormula (\i -> RFormula (\j -> RFormula (\k -> Formula (And [ j `eq` (i `plus` k) ] ))))
--    let	rf3 = RFormula (\i -> RFormula (\j ->  Formula (And [ j `eq` (i `plus` (2::Int))) ] )))
--    let	rf3 = RFormula (\i -> RFormula (\j -> Formula (And [ Exists (\k -> And [ j `eq` (i `plus` k `plus` (2::Int)) ] ) ] )))
    (ptr_r3, rf3) <- build_relation (["a", "b", "c"], [], rf3)
    putStr ((show rf3) ++ "\n")
    eval_relation ptr_r3 rf3
    Omega_stub.relation_finalize ptr_r3
    Omega_stub.relation_print ptr_r3
    Omega_stub.query_experiment ptr_r3

    rf3' <- extract_formula_from_set ptr_r3
    putStr ((show rf3') ++ "\n")

    putStr "[DONE]\n"


