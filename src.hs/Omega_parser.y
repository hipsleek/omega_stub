{
module Omega_parser (extract_rformula) where
import Char
import Omega_types
import Omega_tokens
import Omega_lexer
import Foreign.Ptr

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

replace_vars_in_rformula :: [Variable_name] -> RFormula -> RFormula
replace_vars_in_rformula [] rf = rf
replace_vars_in_rformula (v_name:v_names) rf = RFormula (\v -> (replace_var_in_rformula v_name v (replace_vars_in_rformula v_names rf)))

}
%name omega_parser
%tokentype { Token }
%token 
	exists		{ TokenExists }
	forall		{ TokenForall }
	union		{ TokenUnion }
	int		{ TokenInt $$ }
	var		{ TokenVar $$ }
	arrow           { TokenArrow }
	'='		{ TokenEq }
	geq		{ TokenGeq }
	leq		{ TokenLeq }
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

%%
OFormula : '{' '[' Vars ']' arrow '[' Vars ']' Formulas '}' {
--  (show $3) ++ "->" ++ (show $7) ++ " : " ++ (show $9)
  (replace_vars_in_rformula ($3 ++ $7) (Formula $9))
}

Vars : Vars ',' var { $3:$1}
     | var          { [$1] }

Formulas :: { Formula }
Formulas : OrFormulas       { Or $1 }
	 | AndFormulas      { And $1 }
	 | '(' Formulas ')' { $2 }
	 | Formula          { And [$1] }
 	 | exists '(' var ':' Formulas ')' { Exists (\v -> (replace_var_in_formula $3 v $5)) }
 	 | forall '(' var ':' Formulas ')' { Forall (\v -> (replace_var_in_formula $3 v $5)) }
	 | {- empty -}                     { And [] }
OrFormulas :: { [Formula] }
OrFormulas : OrFormulas or Formula { $3:$1 }
           | Formula or Formula { [$1,$3] }
AndFormulas :: { [Formula] }
AndFormulas : AndFormulas and Formula { $3:$1 }
            | Formula and Formula     { [$1,$3] }
Formula :: { Formula }
Formula : Eq         { Eq $1 }
	| Geq        { Geq $1 }
	| Leq        { Geq $1 }

Eq :: { [Update] }
Eq  : Expr '=' Expr  { $1 ++ (minus_update $3) }
    | Eq '=' Expr    { $1 }                             -- ignoring $3
Geq : Expr geq Expr  { $1 ++ (minus_update $3) }
    | Geq geq Expr   { $1 }                             -- ignoring $3
Leq : Expr leq Expr  { $3 ++ (minus_update $1) }
    | Leq leq Expr   { $1 }                             -- ignoring $3

Expr :: { [Update] }
Expr : Expr '+' Expr { $1 ++ $3 }
     | Expr '-' Expr { $1 ++ (minus_update $3) }
     | '(' Expr ')'  { $2 }
     | int var       { [ Coef ($2, nullPtr) $1 ] }
     | int           { [ Const $1 ] }
     | var           { [ Coef ($1, nullPtr) 1 ]}

{
happyError tokens = error ("Parse error" ++ (show tokens))

extract_rformula :: String -> RFormula
extract_rformula = omega_parser . omega_lexer
}
