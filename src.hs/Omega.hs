module Omega where

import Foreign
import Foreign.C

data Relation = Relation ()

data F_And = F_And ()
data F_Or = F_Or ()
data F_Not = F_Not ()
data F_Exists = F_Exists ()
data F_Forall = F_Forall ()

data GEQ_Handle = GEQ_Handle ()
data EQ_Handle = EQ_Handle ()
data Stride_Handle = Stride_Handle ()

class Formula f

instance Formula F_And
instance Formula F_Or 
instance Formula F_Not
instance Formula F_Exists
instance Formula F_Forall

class Formula f => F_Declaration f

instance F_Declaration F_Exists
instance F_Declaration F_Forall

class Constraint_Handle h

instance Constraint_Handle GEQ_Handle
instance Constraint_Handle EQ_Handle
instance Constraint_Handle Stride_Handle

data Free_Var_Decl = Free_Var_Decl ()

data Var_Decl = Var_Decl ()
class Global_Var_Decl v

instance Global_Var_Decl Free_Var_Decl

type Variable_ID = Ptr Var_Decl
--type Global_Var_ID = Global_Var_Decl v => (Ptr v)
type Argument_Touple = CInt

foreign import ccall relation_new0 :: IO (Ptr Relation)
foreign import ccall relation_new1 :: CInt -> IO (Ptr Relation)
foreign import ccall relation_new2 :: CInt -> CInt -> IO (Ptr Relation)

foreign import ccall relation_delete :: (Ptr Relation) -> IO ()

foreign import ccall relation_finalize :: (Ptr Relation) -> IO ()

foreign import ccall relation_set_var :: (Ptr Relation) -> CInt -> IO (Variable_ID)
foreign import ccall relation_input_var :: (Ptr Relation) -> CInt -> IO (Variable_ID)
foreign import ccall relation_output_var :: (Ptr Relation) -> CInt -> IO (Variable_ID)
foreign import ccall relation_get_local1 :: (Ptr Relation) -> Variable_ID -> IO (Variable_ID)
foreign import ccall relation_get_local_global1 :: (Ptr Relation) -> Variable_ID -> IO (Variable_ID)
foreign import ccall relation_get_local_global2 :: (Ptr Relation) -> Variable_ID -> Argument_Touple -> IO (Variable_ID)

foreign import ccall relation_name_set_var :: (Ptr Relation) -> CInt -> CString -> IO ()
foreign import ccall relation_name_input_var :: (Ptr Relation) -> CInt -> CString -> IO ()
foreign import ccall relation_name_output_var :: (Ptr Relation) -> CInt -> CString -> IO ()

foreign import ccall relation_add_and :: (Ptr Relation) -> (Ptr F_And)
foreign import ccall relation_add_or :: (Ptr Relation) -> (Ptr F_Or)
foreign import ccall relation_add_not :: (Ptr Relation) -> (Ptr F_Not)
foreign import ccall relation_add_forall :: (Ptr Relation) -> (Ptr F_Forall)
foreign import ccall relation_add_exists :: (Ptr Relation) -> (Ptr F_Exists)

foreign import ccall f_add_and :: (Ptr F_And) -> (Ptr F_And)
foreign import ccall f_add_or :: (Ptr F_And) -> (Ptr F_Or)
foreign import ccall f_add_not :: (Ptr F_And) -> (Ptr F_Not)
foreign import ccall f_add_forall :: (Ptr F_And) -> (Ptr F_Forall)
foreign import ccall f_add_exists :: (Ptr F_And) -> (Ptr F_Exists)

foreign import ccall f_and_add_GEQ :: (Ptr F_And) -> (Ptr GEQ_Handle)
foreign import ccall f_and_add_EQ :: (Ptr F_And) -> (Ptr EQ_Handle)
foreign import ccall f_and_add_stride :: (Ptr F_And) -> (Ptr Stride_Handle)

--foreign import ccall f_declare :: (F_Declaration f) => (Ptr f) -> CString -> IO (Variable_ID)
foreign import ccall f_declare :: (Ptr F_Exists) -> CString -> IO (Variable_ID)

--foreign import ccall constraint_handler_update_const :: (Constraint_Handle ch) => (Ptr ch) -> CInt -> IO ()
--foreign import ccall constraint_handler_update_coef :: (Constraint_Handle ch) => (Ptr ch) -> CInt -> IO ()
foreign import ccall constraint_handler_update_const :: (Ptr GEQ_Handle) -> CInt -> IO ()
foreign import ccall constraint_handler_update_coef :: (Ptr GEQ_Handle) -> CInt -> IO ()

foreign import ccall relation_print :: (Ptr Relation) -> IO ()
foreign import ccall relation_print_file :: (Ptr Relation) -> CInt -> (Ptr CFile) -> IO ()
foreign import ccall relation_print_with_subs :: (Ptr Relation) -> CInt -> (Ptr CFile) -> CInt -> IO ()
foreign import ccall relation_print_with_subs_to_string :: (Ptr Relation) -> CInt -> IO (CString)
foreign import ccall relation_print_formula_to_string :: (Ptr Relation) -> IO (CString)

foreign import ccall free_var_decl0 :: CString -> IO (Ptr Free_Var_Decl)
foreign import ccall free_var_decl1 :: CString -> CInt -> IO (Ptr Free_Var_Decl)




