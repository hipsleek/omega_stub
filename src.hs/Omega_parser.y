{
module Omega_parser where
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

-------HeadVariable----------------
_replace_vars_in_rformula :: [HeadVariable] -> [(Int,Variable)] -> [(String,[Variable])] -> RFormula -> RFormula
_replace_vars_in_rformula [] intsToVs strToVs (Formula f) = 
  let notEmpty = filter (\(s,vs) -> length vs /= 1) strToVs in
  let eqIntsToVs = map (\(i,v) -> Eq [Coef v 1,Const (-i)]) intsToVs in
  let eqVsToVs = genEqs notEmpty in
  Formula $ And ([f,eqVsToVs]++eqIntsToVs)
_replace_vars_in_rformula ((HeadString v_name):v_names) intsToVs strToVs rf = 
  RFormula (\v -> 
    let newStrToVs = case lookup v_name strToVs of
          Nothing -> (v_name,[v]):strToVs
          _ -> map (\(str,vs) -> case v_name == str of {True -> (str,v:vs);False -> (str,vs)}) strToVs in
    (replace_var_in_rformula v_name v (_replace_vars_in_rformula v_names intsToVs newStrToVs rf)))
_replace_vars_in_rformula ((HeadInt v_int):v_names) intsToVs strToVs rf = 
  RFormula (\v -> (replace_var_in_rformula ("INT"++show v_int) v (_replace_vars_in_rformula v_names ((v_int,v):intsToVs) strToVs rf)))

replace_vars_in_rformula:: [HeadVariable] -> RFormula -> RFormula
replace_vars_in_rformula v_names rf =
  _replace_vars_in_rformula v_names [] [] rf

genEqs:: [(String,[Variable])] -> Formula
genEqs strToVs = 
  let f l = map (\v -> Eq[Coef (head l) 1,Coef v (-1)]) (tail l) in
  And $ concatMap (\(s,vs) -> f vs) strToVs

--HeadVariable can't be Int in the case of exists and forall
exists_vars_in_formula :: [HeadVariable] -> Formula -> Formula
exists_vars_in_formula [] f = f
exists_vars_in_formula ((HeadString v_name):v_names) f = Exists (\v -> (replace_var_in_formula v_name v (exists_vars_in_formula v_names f)))

forall_vars_in_formula :: [HeadVariable] -> Formula -> Formula
forall_vars_in_formula [] f = f
forall_vars_in_formula ((HeadString v_name):v_names) f = Forall (\v -> (replace_var_in_formula v_name v (forall_vars_in_formula v_names f)))

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
LUnion: LUnion1 { Union (reverse $1) }

LUnion1: OFormula   {[$1]}
  | LUnion1 union OFormula  {$3:$1}

OFormula : '{' '[' Vars ']' arrow '[' Vars ']' Formulas '}' { (replace_vars_in_rformula ($3 ++ $7) (Formula $9)) }
         | '{' '[' Vars ']' arrow '[' Vars ']' ':' Formulas '}' { (replace_vars_in_rformula ($3 ++ $7) (Formula $10)) }
         | '{' '[' Vars ']' Formulas '}' { (replace_vars_in_rformula $3 (Formula $5)) }
         | '{' '[' Vars ']' ':' Formulas '}' { (replace_vars_in_rformula $3 (Formula $6)) }

Vars :: { [HeadVariable] }
Vars : Vars ',' var { $1 ++ [HeadString $3]}
     | Vars ',' int { $1 ++ [HeadInt $3]}
     | var          { [HeadString $1] }
     | int          { [HeadInt $1] }

Formulas :: { Formula }
Formulas : OrFormulas       { Or $1 }
	 | AndFormulas      { And $1 }
	 | '(' Formulas ')' { $2 }
	 | Formula          { And [$1] }
	 | {- empty -}      { And [] }
	 | true             { Or [Eq[Const 0]] }
	 | false            { And [Eq[Const 0,Const 1]] }

OrFormulas :: { [Formula] }
OrFormulas : OrFormulas or Formulas { $3:$1 }
           | Formulas or Formulas { [$1,$3] }
AndFormulas :: { [Formula] }
AndFormulas : AndFormulas and Formulas { $3:$1 }
            | Formulas and Formulas    { [$1,$3] }
Formula :: { Formula }
Formula : qs              { $1 }
        | q               { let (f,_) = $1 in f }
        | exists '(' Vars ':' Formulas ')' { (exists_vars_in_formula $3 $5) }
 	| forall '(' Vars ':' Formulas ')' { (forall_vars_in_formula $3 $5) }
        | '(' Formula ')' { $2 }

qs :: { Formula }
qs : q '=' Expr { let (f,es) = $1 in (And [f,Eq (es ++ (minus_update $3))]) }
   | q geq Expr { let (f,es) = $1 in (And [f,Geq (es ++ (minus_update $3))]) }
   | q leq Expr { let (f,es) = $1 in (And [f,Geq ($3 ++ (minus_update es))]) }
   | q '>' Expr { let (f,es) = $1 in (And [f,Geq ((Const (- 1)):(es ++ (minus_update $3)))]) }
   | q '<' Expr { let (f,es) = $1 in (And [f,Geq ((Const (- 1)):($3 ++ (minus_update es)))]) }

q :: { (Formula,[Update]) }
q : Expr '=' Expr  { ((Eq ($1 ++ (minus_update $3))),$3) }
  | Expr geq Expr  { ((Geq ($1 ++ (minus_update $3))),$3) }
  | Expr leq Expr  { ((Geq ($3 ++ (minus_update $1))),$3) }
  | Expr '>' Expr  { ((Geq ((Const (- 1)):($1 ++ (minus_update $3)))),$3) }
  | Expr '<' Expr  { ((Geq ((Const (- 1)):($3 ++ (minus_update $1)))),$3) }
  | Exprs geq Expr { ((And (map (\e -> (Geq (e ++ (minus_update $3)))) $1)),$3) }
  | Exprs leq Expr { ((And (map (\e -> (Geq ($3 ++ (minus_update e)))) $1)),$3) }
  | Exprs '>' Expr { ((And (map (\e -> (Geq ((Const (- 1)):(e ++ (minus_update $3))) )) $1)),$3) }
  | Exprs '<' Expr { ((And (map (\e -> (Geq ((Const (- 1)):($3 ++ (minus_update e))) )) $1)),$3) }

Exprs :: { [[Update]] }
Exprs : Exprs ',' Expr { $3:$1 }
      | Expr ',' Expr  { [$1,$3] }

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
}
