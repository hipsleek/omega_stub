---
-- This is an example using the new Omega module.
--
-- $Id: Main.hs,v 1.3 2003-07-04 03:42:22 raz Exp $
--

module Main(main) where

import Omega
import Foreign
import Foreign.C
import qualified Omega_stub

main = do
    print "[START]"
    let	sf = RFormula (\x -> RFormula (\y -> RFormula (\n -> Formula (And [
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
    (s, (RFormula sf)) <- build_relation (["x", "y"], [], sf)

    n_str <- newCString "n"
    n_ptr <- Omega_stub.free_var_decl0 n_str
    s_local_n_ptr <- Omega_stub.relation_get_local_global1 s n_ptr
    let n = ("n", s_local_n_ptr)
    print (show (sf n))
    eval_relation s (sf n)
    Omega_stub.relation_finalize s
    Omega_stub.relation_print s
    print "[DONE]"


