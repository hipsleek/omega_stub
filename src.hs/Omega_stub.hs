{- $Id: Omega_stub.hs,v 1.1 2003-05-26 05:53:53 raz Exp $ -}

module Omega_stub where

import Foreign
import Foreign.C

data Relation = Relation
data Formula = Formula
type F_Declaration = Formula


type F_And = Formula
type F_Or = Formula
type F_Not = Formula

type F_Exists = F_Declaration
type F_Forall = F_Declaration

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

--foreign import ccall f_declare :: (F_Declaration f) => (Ptr f) -> CString -> IO (Variable_ID)
foreign import ccall f_declare :: (Ptr F_Declaration) -> CString -> IO (Ptr Variable)

--foreign import ccall constraint_handler_update_const :: (Constraint_Handle ch) => (Ptr ch) -> CInt -> IO ()
--foreign import ccall constraint_handler_update_coef :: (Constraint_Handle ch) => (Ptr ch) -> CInt -> IO ()
foreign import ccall constraint_handler_update_const :: (Ptr Constraint_Handle) -> CInt -> IO ()
foreign import ccall constraint_handler_update_coef :: (Ptr Constraint_Handle) -> (Ptr Variable) -> CInt -> IO ()

foreign import ccall relation_print :: (Ptr Relation) -> IO ()
foreign import ccall relation_print_file :: (Ptr Relation) -> CInt -> (Ptr CFile) -> IO ()
foreign import ccall relation_print_with_subs :: (Ptr Relation) -> CInt -> (Ptr CFile) -> CInt -> IO ()
--foreign import ccall relation_print_with_subs_to_string :: (Ptr Relation) -> CInt -> IO (CString)
--foreign import ccall relation_print_formula_to_string :: (Ptr Relation) -> IO (CString)

foreign import ccall free_var_decl0 :: CString -> IO (Ptr Variable)
foreign import ccall free_var_decl1 :: CString -> CInt -> IO (Ptr Variable)




