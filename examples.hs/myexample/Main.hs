---
-- This is an example using the new Omega module.
--
-- $Id $
--

module Main(main) where

import Omega
import Foreign
import Foreign.C
import qualified Omega_stub

main = do
    print "[START relation_new2]"
    s <- Omega_stub.relation_new1 2
    print "[START newCString]"
    n_str <- newCString "n"
    print "[START free_var_decl0]"
    n_ptr <- Omega_stub.free_var_decl0 n_str
    print "[START relation_get_local_global1]"
    s_local_n_ptr <- Omega_stub.relation_get_local_global1 s n_ptr
    print "[START let...]"
    let n = ("n", s_local_n_ptr)
	sf = RFormula (\x -> RFormula (\y -> (Formula (And [
							    Geq [Coef x 1, Const (-1)],
							    Geq [Coef x (-1), Coef n 1],
							    Geq [Coef x 1, Coef y (-1), Const 5],
							    Stride 17 [Coef x 1],
							    Exists (\z -> And [
									       Geq [Coef z 1, Coef y (-1)],
									       Geq [Coef x 1, Coef z (-1)],
									       Or [Stride 8 [Coef z 1], Stride 12 [Coef x 5]]
									      ]
								   )
							   ]
						      )
					     )
				      )
		      )
        app_rformula x (RFormula f) = f x
    print "[START relation_set_var]"
    x_ptr <- Omega_stub.relation_set_var s 1
    y_ptr <- Omega_stub.relation_set_var s 2
    print "[START app_rformula]"
    let sf1 = app_rformula ("x", x_ptr) sf
	sf2 = app_rformula ("y", y_ptr) sf1
    print "[OK putStr]"
    putStr (show sf2)
--    build_relation (["x", "y"], 2, s)
    print "[DONE]"


