module Omega_tokens where

data Token
	= TokenExists
	| TokenForall
	| TokenUnion
	| TokenInt Int
	| TokenVar String
	| TokenArrow
	| TokenEq
	| TokenGeq
	| TokenLeq
	| TokenPlus
	| TokenMinus
	| TokenTimes
	| TokenDiv
	| TokenORB
	| TokenCRB
	| TokenOCB
	| TokenCCB
	| TokenOSB
	| TokenCSB
	| TokenAnd
	| TokenOr
	| TokenComma
	| TokenSColon
	deriving Show
