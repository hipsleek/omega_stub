---
-- This is an example using the new Omega module.
--
-- $Id $
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
    print "[START]"

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


    print "extract testing..."
    let	rf3 = RFormula (\i -> RFormula (\j -> RFormula (\k -> Formula (And [ j `eq` (i `plus` k) ] ))))
--    let	rf3 = RFormula (\i -> RFormula (\j ->  Formula (And [ j `eq` (i `plus` (2::Int))) ] )))
--    let	rf3 = RFormula (\i -> RFormula (\j -> Formula (And [ Exists (\k -> And [ j `eq` (i `plus` k `plus` (2::Int)) ] ) ] )))
    (ptr_r3, rf3) <- build_relation (["a", "b", "c"], [], rf3)
    putStr ((show rf3) ++ "\n")
    eval_relation ptr_r3 rf3
    Omega_stub.relation_finalize ptr_r3
    Omega_stub.relation_print ptr_r3
--    Omega_stub.relation_simplify1 ptr_r3
    Omega_stub.query_experiment ptr_r3

    putStr "Experiment from haskell:\n"
    Omega_stub.relation_setup_names ptr_r3
    let while_1 :: (Ptr Omega_stub.DNF_Iterator) -> IO ()
        while_1 ptr_dnf_iterator =
	    do let while_2 :: (Ptr Omega_stub.EQ_Iterator) -> IO ()
		   while_2 ptr_eq_iterator =
		       do let while_3 :: (Ptr Omega_stub.Constr_Vars_Iter) -> IO ()
			      while_3 ptr_constr_iter =
				  do debug_msg "HS_DEBUG: while_3\n"
				     more <- Omega_stub.constr_iter_more ptr_constr_iter
				     if (not more)
					then do debug_msg "HS_DEBUG: while_3 (done)\n"
					        return ()
					else do coef <- Omega_stub.constr_iter_get_coef ptr_constr_iter
						ptr_var <- Omega_stub.constr_iter_get_variable ptr_constr_iter
						kind <- Omega_stub.variable_kind ptr_var
						pos <- Omega_stub.variable_get_position ptr_var
						ptr_name <- Omega_stub.variable_name ptr_var
						name <- peekCString ptr_name
						putStr ("Coef: " ++ name ++ "(" ++ (show pos) ++ ")/" ++ (show kind) ++ " * " ++ (show coef) ++ "\n")
						Omega_stub.constr_iter_next ptr_constr_iter
						while_3 ptr_constr_iter
                          debug_msg "HS_DEBUG: while_2\n"
			  more <- Omega_stub.eq_iterator_more ptr_eq_iterator
			  if (not more)
			     then do debug_msg "HS_DEBUG: while_2 (done)\n"
				     return ()
			     else do ptr_constr_iter <- Omega_stub.eq_constr_iter_new ptr_eq_iterator
				     while_3 ptr_constr_iter
				     const <- Omega_stub.eq_get_const ptr_eq_iterator
				     putStr ("Const: " ++ (show const) ++ "\n")
				     Omega_stub.eq_iterator_next ptr_eq_iterator
				     while_2 ptr_eq_iterator
               debug_msg "HS_DEBUG: while_1\n"
	       more <- Omega_stub.dnf_iterator_more ptr_dnf_iterator
	       if (not more)
		  then do debug_msg "HS_DEBUG: while_1 (done)\n"
			  return ()
		  else do ptr_eq_iterator <- Omega_stub.eq_iterator_new ptr_dnf_iterator
			  while_2 ptr_eq_iterator
			  Omega_stub.dnf_iterator_next ptr_dnf_iterator
			  while_1 ptr_dnf_iterator
    ptr_dnf_iterator_r3 <- Omega_stub.dnf_iterator_new1 ptr_r3
    while_1 ptr_dnf_iterator_r3
--    ptr_eq_iterator <- eq_iterator_new ptr_dnf_r3
--    ptr_constr_iter <- eq_constr_iter_new ptr_eq
--    ptr_var <- constr_iter_get_variable ptr_constr_iter
    
    rf3' <- extract_formula_from_set ptr_r3
    print rf3'

    print "[DONE]"


