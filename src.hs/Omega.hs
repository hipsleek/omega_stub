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
import Omega_util
import Omega_types
import qualified Omega_stub
import Omega_parser
import Prelude hiding ((>),(&&),(||))
import Data.List
import Debug.Trace

build_relation :: Relation -> IO ((Ptr Omega_stub.Relation), RFormula)
build_relation (vars_in_name, vars_out_name, rf) =
  let vars_in_no =  (fromInteger (toInteger (length vars_in_name))) in
  let vars_out_no = (fromInteger (toInteger (length vars_out_name))) in
  if vars_out_no <= 0 then 
    Omega_stub.relation_new1 vars_in_no >>= \r ->
    build_formula rf vars_in_name 1 r Omega_stub.relation_name_set_var Omega_stub.relation_set_var >>= \rf ->
    return (r,rf)
  else 
    Omega_stub.relation_new2 vars_in_no vars_out_no >>= \r ->
    build_formula rf vars_in_name 1 r Omega_stub.relation_name_input_var Omega_stub.relation_input_var >>= \newRf ->
    name_output newRf vars_out_name 1 r >>= \rf ->
    return (r,rf)
  where
     build_formula :: RFormula -> [Variable_name] -> CInt -> (Ptr Omega_stub.Relation) ->
          ((Ptr Omega_stub.Relation) -> CInt -> CString -> IO ()) ->
          ((Ptr Omega_stub.Relation) -> CInt -> IO (Ptr Omega_stub.Variable)) ->
          IO RFormula
     build_formula rf [] _ _ _ _ = return rf
     build_formula (RFormula rf) (var_name:vars_name) offset_var_no r _name_var _var =
          newCString var_name >>= \var_str ->
          _name_var r offset_var_no var_str >>
          _var r offset_var_no >>= \var_ptr ->
          build_formula (rf (var_name, var_ptr)) vars_name (offset_var_no + 1) r _name_var _var
     name_output :: RFormula -> [Variable_name] -> CInt -> (Ptr Omega_stub.Relation) -> IO RFormula
     name_output rf [] _ _ = return rf
     name_output (RFormula rf) (var_name:vars_name) offset_var_no r =
          newCString var_name >>= \var_str ->
          Omega_stub.relation_name_output_var r offset_var_no var_str >>
          Omega_stub.relation_output_var r offset_var_no >>= \var_ptr ->
          name_output (rf (var_name, var_ptr)) vars_name (offset_var_no + 1) r
     name_output rf vars _ _ = error ("ERROR: name_output: rf = " ++ (show rf) ++ "\n##\n" ++ show vars)

eval_relation :: Evaluable r => (Ptr r) -> RFormula -> IO ()
eval_relation r (Formula (And fs)) =
    do r' <- add_and r
       sequence_ [eval_function r' f | f <- fs]
eval_relation r (Formula (Or fs)) =
    do r' <- add_or r
       sequence_ [eval_function r' f | f <- fs]
eval_relation r (Formula f) =
    let (str, _) = omega_show f 1
    in do error ("ERROR: You are trying to eval_relation-ing a Formula not implemented!\n" ++ str)
eval_relation r rf =
    let (str, _) = omega_show rf 1
    in do error ("ERROR: You are trying to eval_relation-ing a RFormula not implemented!\n" ++ str)


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
    i `geq` v = Geq [Const i, Coef v (- 1)]
instance Arith_q Variable Variable where
    v1 `eq` v2 = Eq [Coef v1 1, Coef v2 (- 1)]
    v1 `geq` v2 = Geq [Coef v1 1, Coef v2 (- 1)]

instance Arith_mul Int Variable where
    i `mul` v = (Coef v i)
instance Arith_mul Variable Int where
    v `mul` i = (Coef v i)


n `div` us = Stride n us


relation_extract_rformula :: (Ptr Omega_stub.Relation) -> IO RFormula
relation_extract_rformula ptr_r =
    do Omega_stub.relation_setup_names ptr_r
       ptr_str_print <- Omega_stub.relation_print_to_string ptr_r
       str_print <- peekCString ptr_str_print
       free ptr_str_print 
       return (extract_rformula str_print)

tr s f = trace (s++show f) f

-- extract_formula_from_set :: (Ptr Omega_stub.Relation) -> (IO RFormula)
-- extract_formula_from_set ptr_r =
--     do let vars_to_eq [] = []
--            vars_to_eq (v:vars) = (Coef v 1: (vars_to_eq vars))
--            formula_building vars = 
-- 	       do Omega_stub.relation_setup_names ptr_r
-- 		  let while_1 :: (Ptr Omega_stub.DNF_Iterator) -> IO ()
-- 		      while_1 ptr_dnf_iterator =
--  		          do let while_2 :: (Ptr Omega_stub.EQ_Iterator) -> IO ()
--  				 while_2 ptr_eq_iterator =
--  				     do let while_3 :: (Ptr Omega_stub.Constr_Vars_Iter) -> IO ()
--  					    while_3 ptr_constr_iter =
--  						do debug_msg "HS_DEBUG(extract_formula_from_set): while_3\n"
--  						   more <- Omega_stub.constr_iter_more ptr_constr_iter
--  						   if (not more)
--  						      then do debug_msg "HS_DEBUG(extract_formula_from_set): while_3 (done)\n"
--  							      return []
--  						      else do coef <- Omega_stub.constr_iter_get_coef ptr_constr_iter
--  							      ptr_var <- Omega_stub.constr_iter_get_variable ptr_constr_iter
--  							      kind <- Omega_stub.variable_kind ptr_var
--  							      pos <- Omega_stub.variable_get_position ptr_var
-- -- 						                ptr_name <- Omega_stub.variable_name ptr_var
-- -- 						                name <- peekCString ptr_name
-- -- 						                putStr ("Coef: " ++ name ++ "(" ++ (show pos) ++ ")/" ++ (show kind) ++ " * " ++ (show coef) ++ "\n")
--  							      Omega_stub.constr_iter_next ptr_constr_iter
--  							      return ((Coef (genericIndex vars pos) coef):(while_3 ptr_constr_iter))
--                                         debug_msg "HS_DEBUG(extract_formula_from_set): while_2\n"
--  					more <- Omega_stub.eq_iterator_more ptr_eq_iterator
--  					if (not more)
--  					   then do debug_msg "HS_DEBUG(extract_formula_from_set): while_2 (done)\n"
--  						   return []
--  					   else do ptr_constr_iter <- Omega_stub.eq_constr_iter_new ptr_eq_iterator
--  						   while_3 ptr_constr_iter
--  						   const <- Omega_stub.eq_get_const ptr_eq_iterator
--  						   putStr ("Const: " ++ (show const) ++ "\n")
--  						   Omega_stub.eq_iterator_next ptr_eq_iterator
--  						   return Eq ((while_2 ptr_eq_iterator):(Const const))
--                              debug_msg "HS_DEBUG(extract_formula_from_set): while_1\n"
--  			     more <- Omega_stub.dnf_iterator_more ptr_dnf_iterator
--  			     if (not more)
--  				then do debug_msg "HS_DEBUG(extract_formula_from_set): while_1 (done)\n"
--  					return []
--  				else do ptr_eq_iterator <- Omega_stub.eq_iterator_new ptr_dnf_iterator
--  					eq <- while_2 ptr_eq_iterator
--  					Omega_stub.dnf_iterator_next ptr_dnf_iterator
--  					return eq:(while_1 ptr_dnf_iterator)
--                   ptr_dnf_iterator_r <- Omega_stub.dnf_iterator_new1 ptr_r
-- 		  while_1 ptr_dnf_iterator_r
-- --	   build_formula _ 0 vars = Formula (And [Eq (vars_to_eq vars)])
-- 	   build_formula _ 0 vars = do vec_f <- formula_building vars
-- 				       return Formula (And vec_f)
--            build_formula vars_no i vars = do rf <- build_formula vars_no (i - 1) (x:vars)
-- 					     return RFormula (\x -> rf)
--        Omega_stub.relation_finalize ptr_r
--        vars_no <- Omega_stub.relation_n_set ptr_r
--        debug_msg ("HS_DEBUG(extract_formula_from_set): vars_no: " ++ (show vars_no) ++ "\n")
--        build_formula vars_no vars_no []
-- --       return (Formula (And []))

