{- $Id $ -}

module Main(main) where
import Omega_stub
import Foreign
import Foreign.C

main = do
       print "[OK]"

       r <- relation_new2 2 2
       s <- relation_new1 2

       x_str <- newCString "x"
       y_str <- newCString "y"
       relation_name_set_var s 1 x_str
       relation_name_set_var s 2 y_str

       n_str <- newCString "n"
       m_str <- newCString "l"
       l_str <- newCString "l"
       n <- free_var_decl0 n_str
       m <- free_var_decl0 m_str
       l <- free_var_decl1 l_str 1

       local_n <- relation_get_local_global1 r n
       local_m <- relation_get_local_global1 r m
       local_in <- relation_get_local_global2 r n 1
       local_out <- relation_get_local_global2 r n 2

       in1 <- relation_input_var r 1
       in2 <- relation_input_var r 2
       out1 <- relation_output_var r 1
       out2 <- relation_output_var r 2
       x <- relation_set_var s 1
       y <- relation_set_var s 2

       s_root <- relation_add_and s

       xmin <- f_and_add_GEQ s_root
       constraint_handler_update_coef xmin x 1
       constraint_handler_update_const xmin (-1)

       xmax <- f_and_add_GEQ s_root
       constraint_handler_update_coef xmax x (-1)
       s_local_n <- relation_get_local_global1 s n
       constraint_handler_update_coef xmax s_local_n 1

       ymax <- f_and_add_GEQ s_root
       constraint_handler_update_coef ymax x 1
       constraint_handler_update_coef ymax y (-1)
       constraint_handler_update_const ymax 5

       stride_17 <- f_and_add_stride s_root 17
       constraint_handler_update_coef stride_17 x 1

       e <- f_and_add_exists s_root

       z_str <- newCString "z"
       z <- f_exists_declare e z_str
       z_stuff <- f_exists_add_and e

       zmin <- f_and_add_GEQ z_stuff
       constraint_handler_update_coef zmin z 1
       constraint_handler_update_coef zmin y (-1)

       zmax <- f_and_add_GEQ z_stuff
       constraint_handler_update_coef zmax x 1
       constraint_handler_update_coef zmax z (-1)

       o <- f_and_add_or z_stuff
       o8_and <- f_or_add_and o
       z8 <- f_and_add_stride o8_and 8
       constraint_handler_update_coef z8 z 1

       o12_and <- f_or_add_and o
       z12 <- f_and_add_stride o12_and 12
       constraint_handler_update_coef z12 z 1
       constraint_handler_update_coef z12 x 5

       relation_finalize s

       relation_print s
       s_ptr_string <- relation_print_with_subs_to_string s True
       s_string <- peekCString s_ptr_string
       print s_string

       print "[DONE]"
