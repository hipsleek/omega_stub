module Omega_types where

import Foreign
import Foreign.C
import Omega_util
import qualified Omega_stub

type Relation = ([Variable_name], [Variable_name], RFormula)

data RFormula = RFormula (Variable -> RFormula)
	      | Formula Formula
	      | Union [RFormula]
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
    show rf = let (str, _) = (omega_show rf 1)
	      in str

class Omega_Show a where
    omega_show :: a -> Int -> (String, Int)

instance Omega_Show RFormula where
    omega_show (Formula f) vari = let (str', vari') = omega_show f vari
				  in ("(F) . " ++ str', vari')
    omega_show (RFormula r) vari = let (str', vari') = omega_show (r (("_" ++ (show vari)), nullPtr)) (vari + 1)
				   in ("[_" ++ (show vari) ++ "] . " ++ str', vari')
    omega_show (Union rfs) vari = omega_show_union rfs vari

omega_show_union:: [RFormula] -> Int -> (String,Int)
omega_show_union [] vari = ("",vari)
omega_show_union [f] vari = omega_show f vari
omega_show_union (f:fs) vari = 
  let (str1,var1) = omega_show f vari in
  let (str2,var2) = omega_show_union fs var1 in
    (str1 ++ " union " ++ str2,var2)


instance Omega_Show Formula where
    omega_show (And c) vari = let show_vec :: [Formula] -> Int -> (String, Int)
				  show_vec [] vari = (show "--void--", vari)
				  show_vec [c] vari = omega_show c vari
				  show_vec (c:cs) vari = let (str', vari') = omega_show c vari
							     (str'', vari'') = show_vec cs vari'
							 in (str' ++ " && " ++ str'', vari'')
				  (str', vari')= show_vec c vari
			      in ("(" ++ str' ++ ")", vari')
    omega_show (Or c) vari = let show_vec :: [Formula] -> Int -> (String, Int)
				 show_vec [] vari = ((show "--void--"), vari)
				 show_vec [c] vari = (omega_show c vari)
				 show_vec (c:cs) vari = let (str', vari') = omega_show c vari
				       			    (str'', vari'') = show_vec cs vari'
							in (str' ++ " || " ++ str'', vari'')
				 (str', vari')= show_vec c vari
			      in ("(" ++ str' ++ ")", vari')
    omega_show (Not c) vari = let (str', vari') = omega_show c vari
			      in ("(! " ++ str' ++ ")", vari')
    omega_show (Exists f) vari = let (str', vari') = omega_show (f (("_" ++ (show vari)), nullPtr)) (vari + 1)
			         in ("\\exists _" ++ (show vari) ++ " . " ++ str', vari')
    omega_show (Forall f) vari = let (str', vari') = omega_show (f (("_" ++ (show vari)), nullPtr)) (vari + 1)
			         in ("\\forall _" ++ (show vari) ++ " . " ++ str', vari')
    omega_show (Geq u) vari = (show (Geq u), vari)
    omega_show (Eq u) vari = (show (Eq u), vari)
    omega_show (Stride i u) vari = (show (Stride i u), vari)
    

--instance Show (Variable -> RFormula) where
--    show f = show (f ("_", nullPtr))

--instance Show (Variable -> Formula) where
--    show f = show (f ("_", nullPtr))

--instance Show Variable where
--    show (var_name, var_ptr) = show var_name

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
