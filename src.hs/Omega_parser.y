{
module Omega_parser where
import Char
import Omega_types
import Omega_tokens
import Omega_lexer
import Foreign.Ptr
import Debug.Trace
}

%name omega_parser
%tokentype { Token }
%token 
	exists		{ TokenExists }
	forall		{ TokenForall }
	true      { TokenTrue }
	false     { TokenFalse }
	union		{ TokenUnion }
	int		{ TokenInt $$ }
	var		{ TokenVar $$ }
	arrow           { TokenArrow }
	'='		{ TokenEq }
	geq		{ TokenGeq }
	leq		{ TokenLeq }
	'>'		{ TokenGT }
	'<'		{ TokenLT }
	'+'		{ TokenPlus }
	'-'		{ TokenMinus }
	'*'		{ TokenTimes }
	'/'		{ TokenDiv }
	'('		{ TokenORB }
	')'		{ TokenCRB }
	'{'		{ TokenOCB }
	'}'		{ TokenCCB }
	'['		{ TokenOSB }
	']'		{ TokenCSB }
	and		{ TokenAnd }
	or		{ TokenOr }
	','		{ TokenComma }
	':'		{ TokenSColon }

%left and or
%left '=' geq leq
%left '+' '-'
%left '*' '/'
%left NEG
%left exists

%%
LUnion: LUnion1 { let fs = reverse $1 in if (length fs == 1) then head fs else Union fs }

LUnion1: OFormula   {[$1]}
  | LUnion1 union OFormula  {$3:$1}

OFormula : '{' '[' Vars ']' arrow '[' Vars ']' Formulas '}' { (replace_vars_in_rformula_With_Pre ($3 ++ $7) (Formula $9)) }
         | '{' '[' Vars ']' arrow '[' Vars ']' ':' Formulas '}' { (replace_vars_in_rformula_With_Pre ($3 ++ $7) (Formula $10)) }
         | '{' '[' Vars ']' Formulas '}' { (replace_vars_in_rformula_With_Pre $3 (Formula $5)) }
         | '{' '[' Vars ']' ':' Formulas '}' { (replace_vars_in_rformula_With_Pre $3 (Formula $6)) }
         | '{' Formulas '}'   { Formula $2}

Vars :: { [HeadVariable] }
Vars : Vars ',' var { $1 ++ [HeadString $3]}
     | Vars ',' int { $1 ++ [HeadInt $3]}
     | Vars ',' '-' int %prec NEG { $1 ++ [HeadInt (- $4)]}
     | var          { [HeadString $1] }
     | int          { [HeadInt $1] }
     | '-' int %prec NEG { [HeadInt (- $2)] }

Formulas :: { Formula }
Formulas : 
  Formula {$1}
  | '(' Formulas ')' {$2}
  | Formulas or Formulas {Or [$1,$3]}
  | Formulas and Formulas {And [$1,$3]}    
  | {- empty -}      { And [] }
  | true { Or [Eq[Const 0]] }
  | false { And [Eq[Const 0,Const 1]] }

Formula :: { Formula }
Formula : qs              { let (f,rest)=$1 in f }
        | exists '(' Vars ':' Formulas ')' { (exists_vars_in_formula $3 $5) }
 	      | forall '(' Vars ':' Formulas ')' { (forall_vars_in_formula $3 $5) }

-- from the final result of qs, only Formula is useful
-- [[Update]] is needed only in the intermediate productions
qs :: { Formula,[[Update]] }
qs : q {$1}
  | qs RelOp Exprs
  { let (f,rest) = $1 in
    let third = reverse $3 in
    let combi = [(e1,e2) | e1 <- rest, e2 <- third] in
      case $2 of
        TokenEq  -> 
          let newfs = map (\(e1,e2) -> Eq (e1 ++ (minus_update e2))) combi in 
            (And (f:newfs),third)
        TokenGeq -> 
          let newfs = map (\(e1,e2) -> Geq (e1 ++ (minus_update e2))) combi in 
            (And (f:newfs),third)
        TokenGT  ->
          let newfs = map (\(e1,e2) -> Geq ((Const (-1)):(e1 ++ minus_update e2))) combi in
            (And (f:newfs),third)
        TokenLeq ->
          let newfs = map (\(e1,e2) -> Geq (e2 ++ (minus_update e1))) combi in
            (And (f:newfs),third)
        TokenLT  ->
          let newfs = map (\(e1,e2) -> Geq ((Const (-1)):(e2 ++ minus_update e1))) combi in
            (And (f:newfs),third)
  }
  
q :: { (Formula,[[Update]]) }
q : Exprs RelOp Exprs
  { let (first,third) = (reverse $1,reverse $3) in
    let combi = [(e1,e2)| e1 <-first,e2<-third] in
    case $2 of
      TokenEq -> 
        let newfs = map (\(e1,e2) -> (Eq (e1 ++ (minus_update e2)))) combi in
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TokenGeq -> 
        let newfs = map (\(e1,e2) -> (Geq (e1 ++ (minus_update e2)))) combi in 
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TokenGT  -> 
        let newfs = map (\(e1,e2) -> (Geq ((Const (- 1)):(e1 ++ (minus_update e2))) )) combi in
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TokenLeq -> 
        let newfs = map (\(e1,e2) -> (Geq (e2 ++ (minus_update e1)))) combi in 
          if singleton newfs then (head newfs,third) else (And newfs,third)
      TokenLT  -> 
        let newfs = map (\(e1,e2) -> (Geq ((Const (- 1)):(e2 ++ (minus_update e1))) )) combi in
          if singleton newfs then (head newfs,third) else (And newfs,third)
  }
RelOp:: {Token}
RelOp: '=' {$1}
  | geq {$1}
  | '>' {$1}
  | leq {$1}
  | '<' {$1}



Exprs :: { [[Update]] }
Exprs : Exprs ',' Expr { $3:$1 }
      | Expr  { [$1] }

Expr :: { [Update] }
Expr : Expr '+' Expr { $1 ++ $3 }
     | Expr '-' Expr { $1 ++ (minus_update $3) }
     | '(' Expr ')'  { $2 }
     | int var       { [ Coef ($2, nullPtr) $1 ] }
     | int           { [ Const $1 ] }
     | '-' int %prec NEG { [ Const (- $2)] }
     | var           { [ Coef ($1, nullPtr) 1 ]}

{
happyError tokens = error ("Parse error" ++ (show tokens))

extract_rformula :: String -> RFormula
extract_rformula = omega_parser . omega_lexer

replace_var_in_formula :: Variable_name -> Variable -> Formula -> Formula
replace_var_in_formula v1_name v2 (And fs) = And (map (replace_var_in_formula v1_name v2) fs)
replace_var_in_formula v1_name v2 (Or fs) = Or (map (replace_var_in_formula v1_name v2) fs)
replace_var_in_formula v1_name v2 (Not f) = Not (replace_var_in_formula v1_name v2 f)
replace_var_in_formula v1_name v2 (Exists f) = Exists (\v -> (replace_var_in_formula v1_name v2 (f v)))
replace_var_in_formula v1_name v2 (Forall f) = Forall (\v -> (replace_var_in_formula v1_name v2 (f v)))
replace_var_in_formula v1_name v2 (Geq us) = Geq (map (replace_var_in_update v1_name v2) us)
replace_var_in_formula v1_name v2 (Eq us) = Eq (map (replace_var_in_update v1_name v2) us)
replace_var_in_formula v1_name v2 (Stride i us) = Stride i (map (replace_var_in_update v1_name v2) us)

replace_var_in_update :: Variable_name -> Variable -> Update -> Update
replace_var_in_update v1_name v2 (Coef (v3_name, v3_ptr) i) | v3_name == v1_name = Coef v2 i
                                                            | otherwise = Coef (v3_name, v3_ptr) i
replace_var_in_update v1_name v2 (Const i) = Const i

replace_var_in_rformula :: Variable_name -> Variable -> RFormula -> RFormula
replace_var_in_rformula v1_name v2 (RFormula rf) = RFormula (\v -> replace_var_in_rformula v1_name v2 (rf v) )
replace_var_in_rformula v1_name v2 (Formula f) = Formula (replace_var_in_formula v1_name v2 f)
-------Changes---------------------
replace_vars_in_rformula_With_Pre:: [HeadVariable] -> RFormula -> RFormula
replace_vars_in_rformula_With_Pre v_names (Formula f) = 
  let (pre_v_names,eqs) = runFS MkState{cnt=0} (preproc_rformula v_names) in
  replace_vars_in_rformula pre_v_names (Formula $ And (f:eqs))

-- examine [HeadVariable] for duplicates
-- introduce fresh names and equalities between fresh and one of the duplicates
-- introduce fresh names for integers and eqaulities between fresh and integers
preproc_rformula:: [HeadVariable] -> FS ([Variable_name],[Formula])
preproc_rformula [] = return ([],[])
preproc_rformula (HeadInt v_int:hv_names) =
  preproc_rformula hv_names >>= \(v_names,eqs) ->
  fresh >>= \fsh ->
  let newEq = Eq [Coef (fsh,nullPtr) 1,Const (-v_int)] in
  return (fsh:v_names,newEq:eqs)

preproc_rformula (HeadString v_name:hv_names) = 
  preproc_rformula hv_names >>= \(v_names,eqs) ->
  if isDuplicated v_name hv_names
    then 
      fresh >>= \fsh ->
      let newEq = Eq [Coef (fsh,nullPtr) 1,Coef (v_name,nullPtr) (-1)] in
      return (fsh:v_names,newEq:eqs)
    else return (v_name:v_names,eqs)

-- when looking for duplicates, only HeadString's are important
isDuplicated:: Variable_name -> [HeadVariable] -> Bool
isDuplicated v [] = False
isDuplicated v (HeadString s:v_names) = or [v == s,isDuplicated v v_names]
isDuplicated v (HeadInt i:v_names) = isDuplicated v v_names
-------Changes---------------------

replace_vars_in_rformula :: [Variable_name] -> RFormula -> RFormula
replace_vars_in_rformula [] rf = rf
replace_vars_in_rformula (v_name:v_names) rf = 
  RFormula (\v -> (replace_var_in_rformula v_name v (replace_vars_in_rformula v_names rf))) 

--HeadVariable can't be Int in the case of exists and forall
exists_vars_in_formula :: [HeadVariable] -> Formula -> Formula
exists_vars_in_formula [] f = f
exists_vars_in_formula ((HeadString v_name):v_names) f = Exists (\v -> (replace_var_in_formula v_name v (exists_vars_in_formula v_names f)))

forall_vars_in_formula :: [HeadVariable] -> Formula -> Formula
forall_vars_in_formula [] f = f
forall_vars_in_formula ((HeadString v_name):v_names) f = Forall (\v -> (replace_var_in_formula v_name v (forall_vars_in_formula v_names f)))

singleton:: [a] -> Bool
singleton [x] = True
singleton _ = False 

-------FS Fresh---------------------------
data St = MkState {cnt :: Integer}
newtype FS a = FS (St -> (St,a))

instance Monad FS where
  -- return :: a -> FS a
  return a = FS (\st -> (st, a))
  (FS a) >>= f = FS (\st -> let {(st', a') = (a st);(FS b) = (f a')} in b st')

fresh:: FS String
fresh = FS (\st -> (st{cnt = (cnt st) + 1},"fsh_" ++ show (cnt st)))

runFS:: St -> FS a -> a
runFS state (FS a) = snd $ a state
-------FS Fresh---------------------------
}
