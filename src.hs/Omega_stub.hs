{- $Id: Omega_stub.hs,v 1.7 2003-07-28 09:49:19 raz Exp $ -}

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

type Argument_Touple = CInt

data DNF_Iterator = DNF_Iterator
data EQ_Iterator = EQ_Iterator
data GEQ_Iterator = GEQ_Iterator
data Constr_Vars_Iter = Constr_Vars_Iter
data Variable_Iterator = Variable_Iterator

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

foreign import ccall relation_is_set :: (Ptr Relation) -> IO (Bool)
foreign import ccall relation_n_inp :: (Ptr Relation) -> IO (CInt)
foreign import ccall relation_n_out :: (Ptr Relation) -> IO (CInt)
foreign import ccall relation_n_set :: (Ptr Relation) -> IO (CInt)

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
foreign import ccall relation_print_to_string :: (Ptr Relation) -> IO (CString)
foreign import ccall relation_print_with_subs :: (Ptr Relation) -> Bool -> IO ()
foreign import ccall relation_print_with_subs_to_file :: (Ptr Relation) -> (Ptr CFile) -> Bool -> IO ()
foreign import ccall relation_print_with_subs_to_string :: (Ptr Relation) -> Bool -> IO (CString)
foreign import ccall relation_print_formula_to_string :: (Ptr Relation) -> IO (CString)
foreign import ccall relation_print_outputs_with_subs_to_string :: (Ptr Relation) -> IO (CString)



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

-- query
foreign import ccall dnf_iterator_new1 :: (Ptr Relation) -> IO (Ptr DNF_Iterator)
foreign import ccall dnf_iterator_new3 :: (Ptr Relation) -> CInt -> CInt -> IO (Ptr DNF_Iterator)
foreign import ccall dnf_iterator_next :: (Ptr DNF_Iterator) -> IO ()
foreign import ccall dnf_iterator_more :: (Ptr DNF_Iterator) -> IO (Bool)
--DNF_Iterator* dnf_iterator_new1(Relation* r);
--DNF_Iterator* dnf_iterator_new3(Relation* r, int rdt_conjs, int rdt_constrs);
--void dnf_iterator_next(DNF_Iterator* dnfi);

foreign import ccall eq_iterator_new :: (Ptr DNF_Iterator) -> IO (Ptr EQ_Iterator)
foreign import ccall eq_iterator_next :: (Ptr EQ_Iterator) -> IO ()
foreign import ccall eq_iterator_more :: (Ptr EQ_Iterator) -> IO (Bool)
foreign import ccall eq_constr_iter_new :: (Ptr EQ_Iterator) -> IO (Ptr Constr_Vars_Iter)
--EQ_Iterator* eq_iterator_new(DNF_Iterator* dnfi);
--void eq_iterator_next(EQ_Iterator* eq);
--Constr_Vars_Iter* eq_constr_iter_new(EQ_Iterator* eq);

foreign import ccall geq_iterator_new :: (Ptr DNF_Iterator) -> IO (Ptr GEQ_Iterator)
foreign import ccall geq_iterator_next :: (Ptr GEQ_Iterator) -> IO ()
foreign import ccall geq_iterator_more :: (Ptr GEQ_Iterator) -> IO (Bool)
foreign import ccall geq_constr_iter_new :: (Ptr GEQ_Iterator) -> IO (Ptr Constr_Vars_Iter)
--GEQ_Iterator* geq_iterator_new(DNF_Iterator* dnfi);
--void geq_iterator_next(GEQ_Iterator* geq);
--Constr_Vars_Iter* geq_constr_iter_new(GEQ_Iterator* geq);

foreign import ccall constr_iter_get_variable :: (Ptr Constr_Vars_Iter) -> IO (Ptr Variable)
foreign import ccall constr_iter_get_coef :: (Ptr Constr_Vars_Iter) -> IO (CInt)
foreign import ccall constr_iter_next :: (Ptr Constr_Vars_Iter) -> IO ()
foreign import ccall constr_iter_more :: (Ptr Constr_Vars_Iter) -> IO (Bool)
--Variable_ID constr_iter_get_variable(Constr_Vars_Iter* cvi);
--int constr_iter_get_coef(Constr_Vars_Iter* cvi);
--void constr_iter_next(Constr_Vars_Iter* cvi)
--bool constr_iter_more(Constr_Vars_Iter* cvi);


foreign import ccall var_iter_new :: (Ptr DNF_Iterator) -> IO (Ptr Variable_Iterator)
foreign import ccall var_iter_next :: (Ptr Variable_Iterator) -> IO ()
foreign import ccall var_iter_more :: (Ptr Variable_Iterator) -> IO (Bool)
foreign import ccall var_iter_get_variable :: (Ptr Variable_Iterator) -> IO (Ptr Variable)
--Variable_Iterator* var_iter_new(DNF_Iterator* dnfi);
--void var_iter_next(Variable_Iterator* vi);
--Variable_ID var_iter_get_variable(Variable_Iterator* vi);

foreign import ccall eq_get_const :: (Ptr EQ_Iterator) -> IO (CInt)
foreign import ccall geq_get_const :: (Ptr EQ_Iterator) -> IO (CInt)
--int eq_get_const(GEQ_Iterator* eq);
--int geq_get_const(GEQ_Iterator* qi);

foreign import ccall relation_setup_names :: (Ptr Relation) -> IO ()
foreign import ccall relation_number_of_conjuncts :: (Ptr Relation) -> IO (CInt)

-- experimental stuff
foreign import ccall query_experiment :: (Ptr Relation) -> IO ()

-- simplify
foreign import ccall relation_simplify1 :: (Ptr Relation) -> IO ()
foreign import ccall relation_simplify3 :: (Ptr Relation) -> CInt -> CInt -> IO ()

-- variable
foreign import ccall variable_name :: (Ptr Variable) -> IO (CString)
foreign import ccall variable_kind :: (Ptr Variable) -> IO (CInt)
foreign import ccall variable_get_position :: (Ptr Variable) -> IO (CInt)
--const char* variable_get_name(Variable_ID v);
--Var_Kind variable_kind(Variable_ID v);
--int variable_get_position(Variable_ID v);
