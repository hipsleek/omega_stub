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
import Omega_util
import Prelude hiding ((>),(&&),(||))

type Relation = ([Variable_name], [Variable_name], RFormula)

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
		       show_vec [] = show "--void--"
		       show_vec [c] = show c
		       show_vec (c:cs) = show c ++ " && " ++ show_vec cs
		   in "(" ++ show_vec c ++ ")"
    show (Or c) = let show_vec :: [Formula] -> String
		      show_vec [c] = show c
		      show_vec (c:cs) = show c ++ " || " ++ show_vec cs
		  in "(" ++ show_vec c ++ ")"
    show (Not c) = "(! " ++ show c ++ ")"
    show (Exists f) = "\\exists _ . " ++ show (f ("_", nullPtr))
    show (Forall f) = "\\forall _ . " ++ show (f ("_", nullPtr))

    show (Geq u) = let show_vec :: [Update] -> String
		       show_vec [u] = show u
		       show_vec (u:us) = show u ++ " + " ++ show_vec us
		   in show_vec u ++ " >= 0"
    show (Eq u) = let show_vec :: [Update] -> String
		      show_vec [] = "--void--"
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
    add_and r = do debug_msg ("relation_add_and " ++ (show r) ++ "\n")
		   Omega_stub.relation_add_and r
    add_or  r = do debug_msg ("relation_add_or " ++ (show r) ++ "\n")
		   Omega_stub.relation_add_or r
    add_not r = do debug_msg ("relation_add_or " ++ (show r) ++ "\n")
		   Omega_stub.relation_add_not r
    add_forall r = do debug_msg ("relation_add_forall " ++ (show r) ++ "\n")
		      Omega_stub.relation_add_forall r
    add_exists r = do debug_msg ("relation_add_exists " ++ (show r) ++ "\n")
		      Omega_stub.relation_add_exists r
    add_geq r = do debug_msg ("relation_add_geq " ++ (show r) ++ "\n")
		   f <- Omega_stub.relation_add_and r
		   Omega_stub.f_and_add_GEQ f
    add_eq r = do debug_msg ("relation_add_eq " ++ (show r) ++ "\n")
		  f <- Omega_stub.relation_add_and r
		  Omega_stub.f_and_add_EQ f
    add_stride r n = do debug_msg ("relation_add_stride " ++ (show r) ++ "\n")
			f <- Omega_stub.relation_add_and r
			Omega_stub.f_and_add_stride f n

instance Evaluable Omega_stub.F_And where
    add_and f = do debug_msg ("f_and_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_and f
    add_or  f = do debug_msg ("f_and_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_or f
    add_not f = do debug_msg ("f_and_add_not " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_not f
    add_forall f = do debug_msg ("f_and_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_and_add_forall f
    add_exists f = do debug_msg ("f_and_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_and_add_exists f
    add_geq f = do debug_msg ("f_and_add_geq " ++ (show f) ++ "\n")
		   Omega_stub.f_and_add_GEQ f
    add_eq f = do debug_msg ("f_and_add_eq " ++ (show f) ++ "\n")
		  Omega_stub.f_and_add_EQ f
    add_stride f n = do debug_msg ("f_and_add_stride " ++ (show f) ++ "\n")
			Omega_stub.f_and_add_stride f n

instance Evaluable Omega_stub.F_Or where
    add_and f = do debug_msg ("f_or_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_or_add_and f
    add_or  f = do debug_msg ("f_or_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_or_add_or f
    add_not f = do debug_msg ("f_or_add_not " ++ (show f) ++ "\n")
		   Omega_stub.f_or_add_not f
    add_forall f = do debug_msg ("f_or_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_or_add_forall f
    add_exists f = do debug_msg ("f_or_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_or_add_exists f
    add_geq f = do debug_msg ("f_or_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_or_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do debug_msg ("f_or_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_or_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do debug_msg ("f_or_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_or_add_and f
			Omega_stub.f_and_add_stride f' n

instance Evaluable Omega_stub.F_Not where
    add_and f = do debug_msg ("f_not_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_not_add_and f
    add_or  f = do debug_msg ("f_not_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_not_add_or f
    add_not f = do debug_msg ("f_not_add_not " ++ (show f) ++ "\n")
		   Omega_stub.f_not_add_not f
    add_forall f = do debug_msg ("f_not_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_not_add_forall f
    add_exists f = do debug_msg ("f_not_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_not_add_exists f
    add_geq f = do debug_msg ("f_not_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_not_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do debug_msg ("f_not_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_not_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do debug_msg ("f_not_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_not_add_and f
			Omega_stub.f_and_add_stride f' n

instance Evaluable Omega_stub.F_Forall where
    add_and f = do debug_msg ("f_forall_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_forall_add_and f
    add_or  f = do debug_msg ("f_forall_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_forall_add_or f
    add_not f = do debug_msg ("f_forall_add_forall " ++ (show f) ++ "\n")
		   Omega_stub.f_forall_add_not f
    add_forall f = do debug_msg ("f_forall_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_forall_add_forall f
    add_exists f = do debug_msg ("f_forall_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_forall_add_exists f
    add_geq f = do debug_msg ("f_forall_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_forall_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do debug_msg ("f_forall_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_forall_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do debug_msg ("f_forall_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_forall_add_and f
			Omega_stub.f_and_add_stride f' n

instance Evaluable Omega_stub.F_Exists where
    add_and f = do debug_msg ("f_exists_add_and " ++ (show f) ++ "\n")
		   Omega_stub.f_exists_add_and f
    add_or  f = do debug_msg ("f_exists_add_or " ++ (show f) ++ "\n")
		   Omega_stub.f_exists_add_or f
    add_not f = do debug_msg ("f_exists_add_forall " ++ (show f) ++ "\n")
		   Omega_stub.f_exists_add_not f
    add_forall f = do debug_msg ("f_exists_add_forall " ++ (show f) ++ "\n")
		      Omega_stub.f_exists_add_forall f
    add_exists f = do debug_msg ("f_exists_add_exists " ++ (show f) ++ "\n")
		      Omega_stub.f_exists_add_exists f
    add_geq f = do debug_msg ("f_exists_add_geq " ++ (show f) ++ "\n")
		   f' <- Omega_stub.f_exists_add_and f
		   Omega_stub.f_and_add_GEQ f'
    add_eq f = do debug_msg ("f_exists_add_eq " ++ (show f) ++ "\n")
		  f' <- Omega_stub.f_exists_add_and f
		  Omega_stub.f_and_add_EQ f'
    add_stride f n = do debug_msg ("f_exists_add_stride " ++ (show f) ++ "\n")
			f' <- Omega_stub.f_exists_add_and f
			Omega_stub.f_and_add_stride f' n


build_relation :: Relation -> IO ((Ptr Omega_stub.Relation), RFormula)
build_relation (vars_in_name, vars_out_name, rf) =
    do let vars_in_no =  (fromInteger (toInteger (length vars_in_name)))
       let vars_out_no = (fromInteger (toInteger (length vars_out_name)))
       let build_formula :: RFormula -> [Variable_name] -> CInt -> (Ptr Omega_stub.Relation) ->
			    ((Ptr Omega_stub.Relation) -> CInt -> CString -> IO ()) ->
			    ((Ptr Omega_stub.Relation) -> CInt -> IO (Ptr Omega_stub.Variable)) ->
			    IO RFormula
	   build_formula rf [] _ _ _ _ = return rf
	   build_formula (RFormula rf) (var_name:vars_name) offset_var_no r _name_var _var =
	       do var_str <- newCString var_name
		  _name_var r offset_var_no var_str
	          var_ptr <- _var r offset_var_no
		  build_formula (rf (var_name, var_ptr)) vars_name (offset_var_no + 1) r _name_var _var
           name_output :: RFormula -> [Variable_name] -> CInt -> (Ptr Omega_stub.Relation) ->
			  IO RFormula
	   name_output rf [] _ _ = return rf
	   name_output (RFormula rf) (var_name:vars_name) offset_var_no r =
	       do var_str <- newCString var_name
		  Omega_stub.relation_name_output_var r offset_var_no var_str
		  var_ptr <- Omega_stub.relation_output_var r offset_var_no
		  name_output (rf (var_name, var_ptr)) vars_name (offset_var_no + 1) r
       r <- if vars_out_no <= 0
           then Omega_stub.relation_new1 vars_in_no 
	   else Omega_stub.relation_new2 vars_in_no vars_out_no
       rf <- if vars_out_no <= 0
	  then build_formula rf vars_in_name 1 r Omega_stub.relation_name_set_var Omega_stub.relation_set_var
	  else do rf <- build_formula rf vars_in_name 1 r Omega_stub.relation_name_input_var Omega_stub.relation_input_var
		  name_output rf vars_out_name 1 r
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
    do r' <- add_stride r (fromInteger (toInteger n))
       sequence_ [eval_update r' u | u <- us]
--eval_function r f =
--    do putStr "ERROR: You are trying to eval_function-ing a Formula not implemented!"
--       putStr (show f)

eval_update :: (Ptr Omega_stub.Constraint_Handle) -> Update -> IO ()
eval_update ch (Coef (_, var_ptr) n) =
    do Omega_stub.constraint_handler_update_coef ch var_ptr (fromInteger (toInteger n))
eval_update ch (Const n) =
    do Omega_stub.constraint_handler_update_const ch (fromInteger (toInteger n))

(&&) :: Formula -> Formula -> Formula
f && (And fs) = And (f:fs)
f1 && f2 = And (f1:[f2])

(||) :: Formula -> Formula -> Formula
f || (Or fs) = Or (f:fs)
f1 || f2 = Or (f1:[f2])

class Arith_plus a b where
    plus :: a -> b -> [Update]
    minus :: a -> b -> [Update]
class Arith_mul a b where
    mul :: a -> b -> Update
class Arith_q a b where
    eq :: a -> b -> Formula
    geq :: a -> b -> Formula


instance Arith_plus Update Update where
    u1 `plus` u2 = (u1:[u2])
    u1 `minus` u2 = (u1:(minus_update [u2]))
instance Arith_plus Update [Update] where
    u `plus` us = u:us
    u `minus` us = u:(minus_update us)

-- instance Num a => Arith_plus [Update] a where
--     us `plus` i = (Const i):us
-- instance Num a => Arith_plus a [Update] where
--     i `plus` us = (Const i):us
-- instance Num a => Arith_plus Update a where
--     u `plus` i = (Const i):[u]
-- instance Num a => Arith_plus a Update where
--     i `plus` u = (Const i):[u]

instance Arith_plus [Update] Int where
    us `plus` i = (Const i):us
    us `minus` i = (Const (- i)):us
instance Arith_plus Int [Update] where
    i `plus` us = (Const i):us
    i `minus` us = (Const i):(minus_update us)
instance Arith_plus Update Int where
    u `plus` i = (Const i):[u]
    u `minus` i = (Const (- i)):[u]
instance Arith_plus Int Update where
    i `plus` u = (Const i):[u]
    i `minus` u = (Const i):(minus_update [u])

instance Arith_plus [Update] Variable where
    us `plus` v = (Coef v 1):us
    us `minus` v = (Coef v (- 1)):us
instance Arith_plus Variable [Update] where
    v `plus` us = (Coef v 1):us
    v `minus` us = (Coef v 1):(minus_update us)
instance Arith_plus Variable Int where
    v `plus` i = [Coef v 1, Const i]
    v `minus` i = [Coef v 1, Const (- i)]
instance Arith_plus Int Variable where
    i `plus` v = [Const i, Coef v 1]
    i `minus` v = [Const i, Coef v (- 1)]

instance Arith_plus Variable Variable where
    v1 `plus` v2 = [Coef v1 1, Coef v2 1]
    v1 `minus` v2 = [Coef v1 1, Coef v2 (- 1)]


minus_update :: [Update] -> [Update]
minus_update [] = []
minus_update ((Const i):us) = (Const (- i)):(minus_update us)
minus_update ((Coef v i):us) = (Coef v (- i)):(minus_update us)


instance Arith_q [Update] Int where
    us `eq` 0 = Eq us
    us `eq` i = Eq ((Const (- i)):us)
    us `geq` 0 = Geq us
    us `geq` i = Geq ((Const (- i)):us)
instance Arith_q Int [Update] where
    0 `eq` us = Eq us
    i `eq` us = Eq ((Const (- i)):us)
    0 `geq` us = Geq us
    i `geq` us = Geq ((Const (- i)):us)

instance Arith_q [Update] Variable where
    us `eq` v = Eq ((Coef v (- 1)):us)
    us `geq` v = Geq ((Coef v (- 1)):us)
instance Arith_q Variable [Update] where
    v `eq` us = Eq ((Coef v (- 1)):us)
    v `geq` us = Geq ((Coef v (- 1)):us)
instance Arith_q [Update] Update where
    us `eq` (Coef v i) = Eq ((Coef v (- i)):us)
    us `eq` (Const i) = Eq ((Const (- i)):us)
    us `geq` (Coef v i) = Geq ((Coef v (- i)):us)
    us `geq` (Const i) = Geq ((Const (- i)):us)
instance Arith_q Update [Update] where
    (Coef v i) `eq` us = Eq ((Coef v (- i)):us)
    (Const i) `eq` us = Eq ((Const (- i)):us)
    (Coef v i) `geq` us = Geq ((Coef v (- i)):us)
    (Const i) `geq` us = Geq ((Const (- i)):us)
instance Arith_q Variable Int where
    v `eq` i = Eq [Const (- i), Coef v 1]
    v `geq` i = Geq [Const (- i), Coef v 1]
instance Arith_q Int Variable where
    i `eq` v = Eq [Const (- i), Coef v 1]
    i `geq` v = Geq [Const (- i), Coef v 1]
instance Arith_q Variable Variable where
    v1 `eq` v2 = Eq [Coef v1 1, Coef v2 (- 1)]
    v1 `geq` v2 = Geq [Coef v1 1, Coef v2 (- 1)]

instance Arith_mul Int Variable where
    i `mul` v = (Coef v i)
instance Arith_mul Variable Int where
    v `mul` i = (Coef v i)


n `div` us = Stride n us


extract_formula_from_set :: (Ptr Omega_stub.Relation) -> (IO RFormula)
extract_formula_from_set ptr_r =
    do Omega_stub.relation_finalize ptr_r
       vars_no <- Omega_stub.relation_n_set ptr_r
       debug_msg ("HS_DEBUG(extract_formula_from_set): vars_no: " ++ (show vars_no) ++ "\n")

       let vars_to_eq [] = []
           vars_to_eq (v:vars) = (Coef v 1: (vars_to_eq vars))
	   build_formula _ 0 vars = Formula (And [Eq (vars_to_eq vars)])
           build_formula vars_no i vars = RFormula (\x -> (build_formula vars_no (i - 1) (x:vars)))

       return (build_formula vars_no vars_no [])
--       return (Formula (And []))

--     Omega_stub.relation_setup_names ptr_r3
--     let while_1 :: (Ptr Omega_stub.DNF_Iterator) -> IO ()
--         while_1 ptr_dnf_iterator =
-- 	    do let while_2 :: (Ptr Omega_stub.EQ_Iterator) -> IO ()
-- 		   while_2 ptr_eq_iterator =
-- 		       do let while_3 :: (Ptr Omega_stub.Constr_Vars_Iter) -> IO ()
-- 			      while_3 ptr_constr_iter =
-- 				  do debug_msg "HS_DEBUG: while_3\n"
-- 				     more <- Omega_stub.constr_iter_more ptr_constr_iter
-- 				     if (not more)
-- 					then do debug_msg "HS_DEBUG: while_3 (done)\n"
-- 					        return ()
-- 					else do coef <- Omega_stub.constr_iter_get_coef ptr_constr_iter
-- 						ptr_var <- Omega_stub.constr_iter_get_variable ptr_constr_iter
-- 						kind <- Omega_stub.variable_kind ptr_var
-- 						pos <- Omega_stub.variable_get_position ptr_var
-- 						ptr_name <- Omega_stub.variable_name ptr_var
-- 						name <- peekCString ptr_name
-- 						putStr ("Coef: " ++ name ++ "(" ++ (show pos) ++ ")/" ++ (show kind) ++ " * " ++ (show coef) ++ "\n")
-- 						Omega_stub.constr_iter_next ptr_constr_iter
-- 						while_3 ptr_constr_iter
--                           debug_msg "HS_DEBUG: while_2\n"
-- 			  more <- Omega_stub.eq_iterator_more ptr_eq_iterator
-- 			  if (not more)
-- 			     then do debug_msg "HS_DEBUG: while_2 (done)\n"
-- 				     return ()
-- 			     else do ptr_constr_iter <- Omega_stub.eq_constr_iter_new ptr_eq_iterator
-- 				     while_3 ptr_constr_iter
-- 				     const <- Omega_stub.eq_get_const ptr_eq_iterator
-- 				     putStr ("Const: " ++ (show const) ++ "\n")
-- 				     Omega_stub.eq_iterator_next ptr_eq_iterator
-- 				     while_2 ptr_eq_iterator
--                debug_msg "HS_DEBUG: while_1\n"
-- 	       more <- Omega_stub.dnf_iterator_more ptr_dnf_iterator
-- 	       if (not more)
-- 		  then do debug_msg "HS_DEBUG: while_1 (done)\n"
-- 			  return ()
-- 		  else do ptr_eq_iterator <- Omega_stub.eq_iterator_new ptr_dnf_iterator
-- 			  while_2 ptr_eq_iterator
-- 			  Omega_stub.dnf_iterator_next ptr_dnf_iterator
-- 			  while_1 ptr_dnf_iterator
--     ptr_dnf_iterator_r3 <- Omega_stub.dnf_iterator_new1 ptr_r3
--     while_1 ptr_dnf_iterator_r3

