---
-- This is the module that should export everything you need from
-- Omega Library. A more low lovel acces can be obtain using
-- Omega_stub module.
--
-- @see Omega_stub
--
-- 

module Omega where

import Foreign
import Foreign.C
import qualified Omega_stub

type Relation = ([Variable_name], Int, RFormula)

data RFormula = RFormula (Variable -> RFormula)
	      | Formula Formula

data Formula = And [Formula]
	     | Or [Formula]
	     | Not Formula
	     | Exists (Variable -> Formula)
	     | Forall (Variable -> Formula)
	     | Geq [Update]
	     | Eq [Update]
	     | Stride Int [Update]

data Update = Coef Variable Int
	    | Const Int

type Variable = (Variable_name, Ptr Omega_stub.Variable)

type Variable_name = String

instance Show Update where
    show (Const i) = show i
    show (Coef v i) = show i ++ "*" ++ show v

instance Show Formula where
    show (And c) = let show_vec :: [Formula] -> String
		       show_vec [c] = show c
		       show_vec (c:cs) = show c ++ " && " ++ show_vec cs
		   in "(" ++ show_vec c ++ ")"
    show (Or c) = let show_vec :: [Formula] -> String
		      show_vec [c] = show c
		      show_vec (c:cs) = show c ++ " || " ++ show_vec cs
		  in "(" ++ show_vec c ++ ")"
    show (Not c) = "(! " ++ show c ++ ")"
    show (Exists f) = "\\exists [_] . " ++ show (f ("[_]", nullPtr))
    show (Forall f) = "\\forall [_] . " ++ show (f ("[_]", nullPtr))

    show (Geq u) = let show_vec :: [Update] -> String
		       show_vec [u] = show u
		       show_vec (u:us) = show u ++ " + " ++ show_vec us
		   in show_vec u ++ " >= 0"
    show (Eq u) = let show_vec :: [Update] -> String
		      show_vec [u] = show u
		      show_vec (u:us) = show u ++ " + " ++ show_vec us
		  in show_vec u ++ " = 0"
    show (Stride i u) = let show_vec :: [Update] -> String
		            show_vec [u] = show u
		            show_vec (u:us) = show u ++ " + " ++ show_vec us
			in show i ++ "|" ++ show_vec u

instance Show RFormula where
    show (Formula f) = show f
    show (RFormula r) = "[*] . " ++ show (r ("[*]", nullPtr))

--instance Show Variable where
--    show (var_name, var_ptr) = show var_name

eval_RFormula :: [String] -> RFormula -> IO String
eval_RFormula _ (Formula f) = do let r = show f
				 putStr r
				 return r
eval_RFormula (var:vars) (RFormula rf) = eval_RFormula vars (rf (var, nullPtr))

-- experimental stuff
build_relation :: Relation -> IO ()
build_relation (vars_in_name, vars_out_no, rf) =
    do let vars_in_no =  (fromInteger (toInteger (length vars_in_name)))
       let vars_out_no = (fromInteger (toInteger vars_out_no))
       let build_formula :: RFormula -> [Variable_name] -> CInt -> (Ptr Omega_stub.Relation) -> IO RFormula
	   build_formula rf [] _ _ = return rf
	   build_formula (RFormula rf) (var_name:vars_name) offset_var_no r =
	       do var_str <- newCString var_name
		  Omega_stub.relation_name_set_var r offset_var_no var_str
	          var_ptr <- Omega_stub.relation_set_var r offset_var_no
		  build_formula (rf (var_name, var_ptr)) vars_name (offset_var_no + 1) r
       r <- Omega_stub.relation_new2 vars_in_no vars_out_no
       rf <- build_formula rf vars_in_name 1 r
       putStr "... show ..."
--       putStr (show f)

-- building ...

-- some printing (test)
--       Omega_stub.relation_finalize r
--       Omega_stub.relation_print r

{-
run_RFormula :: [IO (Ptr Variable)] -> RFormula -> IO ()
run_RFormula _ (Formula And fs) = do and <- relation_add_and
 
run_RFormula (var:vars) (RFormula rf) = 
-}

-- Examples --

{-
r1 = RFormula (\x -> Formula (And [
				   Eq [Coef x 2, Const 5],
				   Stride 2 [Coef x 1]
				  ]))

n = ("n", nullPtr)
s = RFormula (\x -> RFormula (\y -> (Formula (And [
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
-}

{-
*Main> And [Eq [Coef "x" 2, Const 5]]
(2*x + 5 = 0)
*Main> (\x -> And [Eq [Coef x 2, Const 5]]) "x"
(2*x + 5 = 0)
*Main> (\x -> And [Eq [Coef (head x) 2, Const 5]]) ["x"]
(2*x + 5 = 0)

Main> (\x -> And [Eq [Coef x 2, Const 5]]) "x"
(2*x + 5 = 0)

Main> Exists (\x -> And [Eq [Coef x 2, Const 5]])
(2*_ + 5 = 0)

Main> RFormula (\x -> Formula (And [Eq [Coef x 2, Const 5]]))
[*] . (2*[*] + 5 = 0)

Main> RFormula (\x -> RFormula (\y -> Formula (And [Eq [Coef x 2, Const 5]])))
[*] . [*] . (2*[*] + 5 = 0)

Main> RFormula (\x -> Formula (And [ Eq [Coef x 2, Const 5],  Stride 2 [Coef x 1]]))
[*] . (2*[*] + 5 = 0 && 2|1*[*])
-}

