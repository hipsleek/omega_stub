{- $Id: Omega_stub.hs,v 1.3 2003-07-04 03:56:59 raz Exp $ -}

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

foreign import ccall constraint_handler_update_const :: (Ptr Constraint_Handle) -> CInt -> IO ()
foreign import ccall constraint_handler_update_coef :: (Ptr Constraint_Handle) -> (Ptr Variable) -> CInt -> IO ()

foreign import ccall relation_print :: (Ptr Relation) -> IO ()
foreign import ccall relation_print_to_file :: (Ptr Relation) -> CInt -> (Ptr CFile) -> IO ()
foreign import ccall relation_print_with_subs_to_file :: (Ptr Relation) -> CInt -> (Ptr CFile) -> CInt -> IO ()
foreign import ccall relation_print_with_subs_to_string :: (Ptr Relation) -> CInt -> IO (CString)
foreign import ccall relation_print_formula_to_string :: (Ptr Relation) -> IO (CString)

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

foreign import ccall free_var_decl0 :: CString -> IO (Ptr Variable)
foreign import ccall free_var_decl1 :: CString -> CInt -> IO (Ptr Variable)

