{- $Id: Omega_stub.hs,v 1.4 2003-07-07 04:25:49 raz Exp $ -}

module Omega_stub where

import Foreign
import Foreign.C

data Relation = Relation
data Formula = Formula
type F_Declaration = Formula


data F_And 
data F_Or 
data F_Not 

data F_Exists
data F_Forall

data Constraint_Handle = Constraint_Handle

type GEQ_Handle = Constraint_Handle
type EQ_Handle = Constraint_Handle
type Stride_Handle = Constraint_Handle

data Variable = Variable
--type Variable_ID = Ptr Var_Decl

type Argument_Touple = CInt

foreign import ccall relation_new0 :: IO (Ptr Relation)
foreign import ccall relation_new1 :: CInt -> IO (Ptr Relation)
foreign import ccall relation_new2 :: CInt -> CInt -> IO (Ptr Relation)

foreign import ccall relation_delete :: (Ptr Relation) -> IO ()

foreign import ccall relation_finalize :: (Ptr Relation) -> IO ()

foreign import ccall relation_set_var :: (Ptr Relation) -> CInt -> IO (Ptr Variable)
foreign import ccall relation_input_var :: (Ptr Relation) -> CInt -> IO (Ptr Variable)
foreign import ccall relation_output_var :: (Ptr Relation) -> CInt -> IO (Ptr Variable)
foreign import ccall relation_get_local1 :: (Ptr Relation) -> Ptr Variable -> IO (Ptr Variable)
foreign import ccall relation_get_local_global1 :: (Ptr Relation) -> Ptr Variable -> IO (Ptr Variable)
foreign import ccall relation_get_local_global2 :: (Ptr Relation) -> Ptr Variable -> Argument_Touple -> IO (Ptr Variable)

foreign import ccall relation_name_set_var :: (Ptr Relation) -> CInt -> CString -> IO ()
foreign import ccall relation_name_input_var :: (Ptr Relation) -> CInt -> CString -> IO ()
foreign import ccall relation_name_output_var :: (Ptr Relation) -> CInt -> CString -> IO ()

foreign import ccall relation_add_and :: (Ptr Relation) -> IO (Ptr F_And)
foreign import ccall relation_add_or :: (Ptr Relation) -> IO (Ptr F_Or)
foreign import ccall relation_add_not :: (Ptr Relation) -> IO (Ptr F_Not)
foreign import ccall relation_add_forall :: (Ptr Relation) -> IO (Ptr F_Forall)
foreign import ccall relation_add_exists :: (Ptr Relation) -> IO (Ptr F_Exists)

foreign import ccall f_and_add_and :: (Ptr F_And) -> IO (Ptr F_And)
foreign import ccall f_and_add_or :: (Ptr F_And) -> IO (Ptr F_Or)
foreign import ccall f_and_add_not :: (Ptr F_And) -> IO (Ptr F_Not)
foreign import ccall f_and_add_forall :: (Ptr F_And) -> IO (Ptr F_Forall)
foreign import ccall f_and_add_exists :: (Ptr F_And) -> IO (Ptr F_Exists)

foreign import ccall f_and_add_GEQ :: (Ptr F_And) -> IO (Ptr GEQ_Handle)
foreign import ccall f_and_add_EQ :: (Ptr F_And) -> IO (Ptr EQ_Handle)
foreign import ccall f_and_add_stride :: (Ptr F_And) -> CInt -> IO (Ptr Stride_Handle)

foreign import ccall f_or_add_and :: (Ptr F_Or) -> IO (Ptr F_And)
foreign import ccall f_or_add_or :: (Ptr F_Or) -> IO (Ptr F_Or)
foreign import ccall f_or_add_not :: (Ptr F_Or) -> IO (Ptr F_Not)
foreign import ccall f_or_add_forall :: (Ptr F_Or) -> IO (Ptr F_Forall)
foreign import ccall f_or_add_exists :: (Ptr F_Or) -> IO (Ptr F_Exists)

foreign import ccall f_not_add_and :: (Ptr F_Not) -> IO (Ptr F_And)
foreign import ccall f_not_add_or :: (Ptr F_Not) -> IO (Ptr F_Or)
foreign import ccall f_not_add_not :: (Ptr F_Not) -> IO (Ptr F_Not)
foreign import ccall f_not_add_forall :: (Ptr F_Not) -> IO (Ptr F_Forall)
foreign import ccall f_not_add_exists :: (Ptr F_Not) -> IO (Ptr F_Exists)

foreign import ccall f_forall_add_and :: (Ptr F_Forall) -> IO (Ptr F_And)
foreign import ccall f_forall_add_or :: (Ptr F_Forall) -> IO (Ptr F_Or)
foreign import ccall f_forall_add_not :: (Ptr F_Forall) -> IO (Ptr F_Not)
foreign import ccall f_forall_add_forall :: (Ptr F_Forall) -> IO (Ptr F_Forall)
foreign import ccall f_forall_add_exists :: (Ptr F_Forall) -> IO (Ptr F_Exists)

foreign import ccall f_exists_add_and :: (Ptr F_Exists) -> IO (Ptr F_And)
foreign import ccall f_exists_add_or :: (Ptr F_Exists) -> IO (Ptr F_Or)
foreign import ccall f_exists_add_not :: (Ptr F_Exists) -> IO (Ptr F_Not)
foreign import ccall f_exists_add_forall :: (Ptr F_Exists) -> IO (Ptr F_Forall)
foreign import ccall f_exists_add_exists :: (Ptr F_Exists) -> IO (Ptr F_Exists)

foreign import ccall f_forall_declare :: (Ptr F_Forall) -> CString -> IO (Ptr Variable)
foreign import ccall f_exists_declare :: (Ptr F_Exists) -> CString -> IO (Ptr Variable)

foreign import ccall free_var_decl0 :: CString -> IO (Ptr Variable)
foreign import ccall free_var_decl1 :: CString -> CInt -> IO (Ptr Variable)

foreign import ccall constraint_handler_update_const :: (Ptr Constraint_Handle) -> CInt -> IO ()
foreign import ccall constraint_handler_update_coef :: (Ptr Constraint_Handle) -> (Ptr Variable) -> CInt -> IO ()

foreign import ccall relation_print :: (Ptr Relation) -> IO ()
foreign import ccall relation_print_to_file :: (Ptr Relation) -> CInt -> (Ptr CFile) -> IO ()
foreign import ccall relation_print_with_subs :: (Ptr Relation) -> Bool -> IO ()
foreign import ccall relation_print_with_subs_to_file :: (Ptr Relation) -> (Ptr CFile) -> Bool -> IO ()
foreign import ccall relation_print_with_subs_to_string :: (Ptr Relation) -> Bool -> IO (CString)
foreign import ccall relation_print_formula_to_string :: (Ptr Relation) -> IO (CString)


-- simplification and satisfiability
foreign import ccall is_upper_bound_satisfiable :: (Ptr Relation) -> IO (Ptr Bool)
foreign import ccall is_lower_bound_satisfiable :: (Ptr Relation) -> IO (Ptr Bool)
foreign import ccall is_satisfiable :: (Ptr Relation) -> IO (Ptr Bool)
foreign import ccall is_obvious_tautology :: (Ptr Relation) -> IO (Ptr Bool)
--foreign import ccall is_definite_tautology :: (Ptr Relation) -> IO (Ptr Bool)
foreign import ccall is_exact :: (Ptr Relation) -> IO (Ptr Bool)
foreign import ccall is_inexact :: (Ptr Relation) -> IO (Ptr Bool)
foreign import ccall is_unknown :: (Ptr Relation) -> IO (Ptr Bool)

-- binary relational operators
foreign import ccall union_relation :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall intersection :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall composition :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall join :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall restrict_domain :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall restrict_range :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall difference :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall cross_product :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall gist :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)

-- unary relation operations
foreign import ccall transitive_closure1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall transitive_closure2 :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall domain :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall range :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall inverse :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall complement :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall project :: (Ptr Relation) -> (Ptr Variable) -> IO (Ptr Relation)
foreign import ccall project_sym :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall project_on_sym :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall extend_domain1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall extend_domain2 :: (Ptr Relation) -> CInt -> IO (Ptr Relation)
foreign import ccall range_domain1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall range_domain2 :: (Ptr Relation) -> CInt ->IO (Ptr Relation)
foreign import ccall extend_set1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall extend_set2 :: (Ptr Relation) -> CInt -> IO (Ptr Relation)
foreign import ccall deltas1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall deltas2 :: (Ptr Relation) -> CInt -> IO (Ptr Relation)
foreign import ccall approximate1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall approximate2 :: (Ptr Relation) -> CInt -> IO (Ptr Relation)
foreign import ccall egs_to_geqs1 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall egs_to_geqs2 :: (Ptr Relation) -> Bool -> IO (Ptr Relation)
foreign import ccall sample_solution :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall symbolic_solution :: (Ptr Relation) -> IO (Ptr Relation)

-- advance operations
foreign import ccall convex_hull :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall decoupled_convex_hull :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall affine_hull :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall linear_hull :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall conic_hull :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall fast_tight_hull :: (Ptr Relation) -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall hull0 :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall hull1 :: (Ptr Relation) -> Bool -> IO (Ptr Relation)
foreign import ccall hull2 :: (Ptr Relation) -> Bool -> CInt -> IO (Ptr Relation)
foreign import ccall hull3 :: (Ptr Relation) ->  Bool -> CInt -> (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall check_for_convex_pairs :: (Ptr Relation) -> IO (Ptr Relation)
foreign import ccall check_for_convex_representation :: (Ptr Relation) -> IO (Ptr Relation)

-- relational function that return boolean values
foreign import ccall must_be_subset :: (Ptr Relation) -> (Ptr Relation) -> IO (Bool)
foreign import ccall might_be_subset :: (Ptr Relation) -> (Ptr Relation) -> IO (Bool)
foreign import ccall is_obvious_subset :: (Ptr Relation) -> (Ptr Relation) -> IO (Bool)

