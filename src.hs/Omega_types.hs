module Omega_types where

import Foreign
import Foreign.C
import Omega_util
import qualified Omega_stub

type Relation = ([Variable_name], [Variable_name], RFormula)

data RFormula = RFormula (Variable -> RFormula)
	      | Formula Formula
--	      deriving Show

data Formula = And [Formula]
	     | Or [Formula]
	     | Not Formula
	     | Exists (Variable -> Formula)
	     | Forall (Variable -> Formula)
	     | Geq [Update]
	     | Eq [Update]
	     | Stride Int [Update]
--	     deriving Show

data Update = Coef Variable Int
	    | Const Int
--	    deriving Show

type Variable = (Variable_name, Ptr Omega_stub.Variable)

type Variable_name = String

instance Show Update where
    show (Const i) = show i
    show (Coef (v_name,_) 1) = v_name
    show (Coef (v_name,_) (- 1)) = "(-"++ v_name ++ ")"
    show (Coef (v_name,_) i) = (show i) ++ "*" ++ v_name


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

--instance Show (Variable -> RFormula) where
--    show f = show (f ("_", nullPtr))

--instance Show (Variable -> Formula) where
--    show f = show (f ("_", nullPtr))

--instance Show Variable where
--    show (var_name, var_ptr) = show var_name


eval_RFormula :: [String] -> RFormula -> String
eval_RFormula _ (Formula f) = show f
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


minus_update :: [Update] -> [Update]
minus_update [] = []
minus_update ((Const i):us) = (Const (- i)):(minus_update us)
minus_update ((Coef v i):us) = (Coef v (- i)):(minus_update us)
