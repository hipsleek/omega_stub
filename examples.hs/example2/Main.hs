---
-- This is an example using the new Omega module.
--
-- $Id: Main.hs,v 1.2 2003-07-28 09:37:29 raz Exp $
--

module Main(main) where

import Omega
import Omega_types
import Foreign
import Foreign.C
import qualified Omega_stub
import Omega_util
import Omega_parser
import Omega_lexer

main = do
    putStr "[START]\n"

    let	rf = RFormula (\i -> RFormula (\j -> RFormula (\k -> Formula (And [ j `eq` (i `plus` k) ] ))))
--    let	rf3 = RFormula (\i -> RFormula (\j ->  Formula (And [ j `eq` (i `plus` (2::Int))) ] )))
--    let	rf3 = RFormula (\i -> RFormula (\j -> Formula (And [ Exists (\k -> And [ j `eq` (i `plus` k `plus` (2::Int)) ] ) ] )))
    (ptr_r, rf) <- build_relation (["a", "b"], ["c"], rf)
    putStr ((show rf) ++ "\n")
    eval_relation ptr_r rf

    Omega_stub.relation_finalize ptr_r
    putStr "relation_print: \n"
    Omega_stub.relation_print ptr_r
    putStr "relation_print_with_subs: \n"
    Omega_stub.relation_print_with_subs ptr_r True
--    Omega_stub.query_experiment ptr_r

    ptr_str_print_formula <- Omega_stub.relation_print_formula_to_string ptr_r
    str_print_formula <- peekCString ptr_str_print_formula
    putStr ("str_print_formula: " ++ str_print_formula ++ "\n")

    ptr_str_print_with_subs <- Omega_stub.relation_print_with_subs_to_string ptr_r True
    str_print_with_subs <- peekCString ptr_str_print_with_subs 
    putStr ("relation_print_with_subs: " ++ str_print_with_subs ++ "\n")

    ptr_str_print <- Omega_stub.relation_print_to_string ptr_r
    str_print <- peekCString ptr_str_print
    putStr ("relation_print: " ++ str_print ++ "\n")

--    ptr_str_print_outputs <- Omega_stub.relation_print_outputs_with_subs_to_string ptr_r
--    str_print_outputs <- peekCString ptr_str_print_outputs
--    putStr ("relation_print_outputs: " ++ str_print_outputs ++ "\n")

--    rf' <- extract_formula_from_set ptr_r
--    putStr ((show rf') ++ "\n")


    rf' <- relation_extract_rformula ptr_r
    putStr ("relation_extract_rformula: " ++ (show rf') ++ "\n")


    putStr ((show (extract_rformula "{[a,b] -> [c] a+c = b }")) ++ "\n")
    putStr ((show (extract_rformula "{[a,b] -> [c] a+c >= b }")) ++ "\n")
    putStr ((show (extract_rformula "{[a,b] -> [c] a+c <= b }")) ++ "\n")

    putStr ((show (omega_lexer "{[a,b] -> [c] a+c = b }")) ++ "\n")

    putStr "[DONE]\n"

-- foreign import ccall relation_print :: (Ptr Relation) -> IO ()

-- foreign import ccall relation_print_to_file :: (Ptr Relation) -> CInt -> (Ptr CFile) -> IO ()

-- foreign import ccall relation_print_with_subs :: (Ptr Relation) -> Bool -> IO ()

-- foreign import ccall relation_print_with_subs_to_file :: (Ptr Relation) -> (Ptr CFile) -> Bool -> IO ()

-- foreign import ccall relation_print_with_subs_to_string :: (Ptr Relation) -> Bool -> IO (CString)
-- foreign import ccall relation_print_formula_to_string :: (Ptr Relation) -> IO (CString)
