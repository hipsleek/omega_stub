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
    show (Formula f) = "(F) . " ++ show f
    show (RFormula r) = "[*] . " ++ show (r ("[*]", nullPtr))

--instance Show Variable where
--    show (var_name, var_ptr) = show var_name

eval_RFormula :: [String] -> RFormula -> IO String
eval_RFormula _ (Formula f) = do let r = show f
				 putStr r
				 return r
eval_RFormula (var:vars) (RFormula rf) = eval_RFormula vars (rf (var, nullPtr))

-- experimental stuff

class Evaluable r where
    add_and :: (Ptr r) -> IO (Ptr Omega_stub.F_And)
    add_or :: (Ptr r) -> IO (Ptr Omega_stub.F_Or)
    add_not :: (Ptr r) -> IO (Ptr Omega_stub.F_Not)
    add_forall :: (Ptr r) -> IO (Ptr Omega_stub.F_Forall)
    add_exists :: (Ptr r) -> IO (Ptr Omega_stub.F_Exists)
    add_geq :: (Ptr r) -> IO (Ptr Omega_stub.GEQ_Handle)
    add_eq :: (Ptr r) -> IO (Ptr Omega_stub.EQ_Handle)
    add_stride :: (Ptr r) -> CInt -> IO (Ptr Omega_stub.Stride_Handle)

instance Evaluable Omega_stub.Relation where
    add_and r = do putStr ("relation_add_and " ++ (show r) ++ "\n")
		   Omega_stub.relation_add_and r
    add_or  r = do putStr ("relation_add_or " ++ (show r) ++ "\n")
		   Omega_stub.relation_add_or r
    add_not r = do putStr ("relation_add_or " ++ (show r) ++ "\n")
		   Omega_stub.relation_add_not r
    add_forall r = do putStr ("relation_add_forall " ++ (show r) ++ "\n")
		      Omega_stub.relation_add_forall r
    add_exists r = do putStr ("relation_add_exists " ++ (show r) ++ "\n")
		      Omega_stub.relation_add_exists r
    add_geq r = do putStr ("relation_add_geq " ++ (show r) ++ "\n")
		   f <- Omega_stub.relation_add_and r
		   Omega_stub.f_and_add_GEQ f
    add_eq r = do putStr ("relation_add_eq " ++ (show r) ++ "\n")
		  f <- Omega_stub.relation_add_and r
		  Omega_stub.f_and_add_EQ f
    add_stride r n = do putStr ("relation_add_stride " ++ (show r) ++ "\n")
			f <- Omega_stub.relation_add_and r
			Omega_stub.f_and_add_stride f n

instance Evaluable Omega_stub.F_And where
    add_and f = do putStr ("f_and_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_and f
    add_or  f = do putStr ("f_and_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_or f
    add_not f = do putStr ("f_and_add_not " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_not f
    add_forall f = do putStr ("f_and_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_and_add_forall f
    add_exists f = do putStr ("f_and_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_and_add_exists f
    add_geq f = do putStr ("f_and_add_geq " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_GEQ f
    add_eq f = do putStr ("f_and_add_eq " ++ (show f) ++ "\n")
		  Omega_stub.f_and_add_EQ f
    add_stride f n = do putStr ("f_and_add_stride " ++ (show f) ++ "\n")
			Omega_stub.f_and_add_stride f n

instance Evaluable Omega_stub.F_Or where
    add_and f = do putStr ("f_or_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_or_add_and f
    add_or  f = do putStr ("f_or_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_or_add_or f
    add_not f = do putStr ("f_or_add_not " ++ (show f) ++ "\n")
		   Omega_stub.f_or_add_not f
    add_forall f = do putStr ("f_or_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_or_add_forall f
    add_exists f = do putStr ("f_or_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_or_add_exists f
    add_geq f = do putStr ("f_or_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_or_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do putStr ("f_or_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_or_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do putStr ("f_or_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_or_add_and f
			Omega_stub.f_and_add_stride f' n

instance Evaluable Omega_stub.F_Not where
    add_and f = do putStr ("f_not_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_not_add_and f
    add_or  f = do putStr ("f_not_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_not_add_or f
    add_not f = do putStr ("f_not_add_not " ++ (show f) ++ "\n")
		   Omega_stub.f_not_add_not f
    add_forall f = do putStr ("f_not_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_not_add_forall f
    add_exists f = do putStr ("f_not_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_not_add_exists f
    add_geq f = do putStr ("f_not_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_not_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do putStr ("f_not_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_not_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do putStr ("f_not_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_not_add_and f
			Omega_stub.f_and_add_stride f' n

instance Evaluable Omega_stub.F_Forall where
    add_and f = do putStr ("f_forall_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_forall_add_and f
    add_or  f = do putStr ("f_forall_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_forall_add_or f
    add_not f = do putStr ("f_forall_add_forall " ++ (show f) ++ "\n")
		   Omega_stub.f_forall_add_not f
    add_forall f = do putStr ("f_forall_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_forall_add_forall f
    add_exists f = do putStr ("f_forall_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_forall_add_exists f
    add_geq f = do putStr ("f_forall_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_forall_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do putStr ("f_forall_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_forall_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do putStr ("f_forall_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_forall_add_and f
			Omega_stub.f_and_add_stride f' n

instance Evaluable Omega_stub.F_Exists where
    add_and f = do putStr ("f_exists_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_exists_add_and f
    add_or  f = do putStr ("f_exists_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_exists_add_or f
    add_not f = do putStr ("f_exists_add_forall " ++ (show f) ++ "\n")
		   Omega_stub.f_exists_add_not f
    add_forall f = do putStr ("f_exists_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_exists_add_forall f
    add_exists f = do putStr ("f_exists_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_exists_add_exists f
    add_geq f = do putStr ("f_exists_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_exists_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do putStr ("f_exists_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_exists_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do putStr ("f_exists_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_exists_add_and f
			Omega_stub.f_and_add_stride f' n


build_relation :: Relation -> IO ((Ptr Omega_stub.Relation), RFormula)
build_relation (vars_in_name, vars_out_no_, rf) =
    do let vars_in_no =  (fromInteger (toInteger (length vars_in_name)))
       let vars_out_no = (fromInteger (toInteger vars_out_no_))
       let build_formula :: RFormula -> [Variable_name] -> CInt -> (Ptr Omega_stub.Relation) -> IO RFormula
	   build_formula rf [] _ _ = return rf
	   build_formula (RFormula rf) (var_name:vars_name) offset_var_no r =
	       do var_str <- newCString var_name
		  Omega_stub.relation_name_set_var r offset_var_no var_str
	          var_ptr <- Omega_stub.relation_set_var r offset_var_no
		  build_formula (rf (var_name, var_ptr)) vars_name (offset_var_no + 1) r

       r <- if vars_out_no <= 0
           then Omega_stub.relation_new1 vars_in_no
	   else Omega_stub.relation_new2 vars_in_no vars_out_no
       rf <- build_formula rf vars_in_name 1 r
       return (r, rf)

eval_relation :: Evaluable r => (Ptr r) -> RFormula -> IO ()
eval_relation r (Formula (And fs)) =
    do r' <- add_and r
       sequence_ [eval_function r' f | f <- fs]
eval_relation r (Formula (Or fs)) =
    do r' <- add_or r
       sequence_ [eval_function r' f | f <- fs]
eval_relation r (Formula f) =
    do putStr "ERROR: You are trying to eval_relation-ing a Formula not implemented!"
       putStr (show f)
eval_relation r rf =
    do putStr "ERROR: You are trying to eval_relation-ing a Rformula not implemented!"
       putStr (show rf)

eval_function :: Evaluable r => (Ptr r) -> Formula -> IO ()
eval_function r (And fs) =
    do r' <- add_and r
       sequence_ [eval_function r' f | f <- fs]
eval_function r (Or fs) =
    do r' <- add_or r
       sequence_ [eval_function r' f | f <- fs]
eval_function r (Not f) =
    do r' <- add_not r
       eval_function r' f
eval_function r (Exists rf) =
    do r' <- add_exists r
       var_str <- newCString "_"
       var_ptr <- Omega_stub.f_exists_declare r' var_str
       r'' <- add_and r'
       eval_function r'' (rf ("_", var_ptr))
eval_function r (Forall rf) =
    do r' <- add_forall r
       var_str <- newCString "_"
       var_ptr <- Omega_stub.f_forall_declare r' var_str
       r'' <- add_and r'
       eval_function r'' (rf ("_", var_ptr))
eval_function r (Geq us) =
    do r' <- add_geq r
       sequence_ [eval_update r' u | u <- us]
eval_function r (Eq us) =
    do r' <- add_eq r
       sequence_ [eval_update r' u | u <- us]
eval_function r (Stride n us) =
    do --putStr "eval_function Stride"
       r' <- add_stride r (fromInteger (toInteger n))
       sequence_ [eval_update r' u | u <- us]
eval_function r f =
    do putStr "ERROR: You are trying to eval_function-ing a Formula not implemented!"
       putStr (show f)

eval_update :: (Ptr Omega_stub.Constraint_Handle) -> Update -> IO ()
eval_update ch (Coef (_, var_ptr) n) =
    do Omega_stub.constraint_handler_update_coef ch var_ptr (fromInteger (toInteger n))
eval_update ch (Const n) =
    do Omega_stub.constraint_handler_update_const ch (fromInteger (toInteger n))

