module Omega_lexer(omega_lexer) where

import Omega_tokens
import Char

omega_lexer :: String -> [Token]
omega_lexer [] = []
omega_lexer (c:cs) 
	| isSpace c = omega_lexer cs
	| isAlpha c = omega_lexVar (c:cs)
	| c == '_'  = omega_lexVar (c:cs)
	| isDigit c = omega_lexNum (c:cs)
omega_lexer ('-':'>':cs) = TokenArrow : omega_lexer cs
omega_lexer ('=':cs) = TokenEq : omega_lexer cs
omega_lexer ('<':'=':cs) = TokenLeq : omega_lexer cs
omega_lexer ('>':'=':cs) = TokenGeq : omega_lexer cs
omega_lexer ('<':cs) = TokenLT : omega_lexer cs
omega_lexer ('>':cs) = TokenGT : omega_lexer cs
omega_lexer ('+':cs) = TokenPlus : omega_lexer cs
omega_lexer ('-':cs) = TokenMinus : omega_lexer cs
omega_lexer ('*':cs) = TokenTimes : omega_lexer cs
omega_lexer ('/':cs) = TokenDiv : omega_lexer cs
omega_lexer ('(':cs) = TokenORB : omega_lexer cs
omega_lexer (')':cs) = TokenCRB : omega_lexer cs
omega_lexer ('{':cs) = TokenOCB : omega_lexer cs
omega_lexer ('}':cs) = TokenCCB : omega_lexer cs
omega_lexer ('[':cs) = TokenOSB : omega_lexer cs
omega_lexer (']':cs) = TokenCSB : omega_lexer cs
omega_lexer (',':cs) = TokenComma : omega_lexer cs
omega_lexer (':':cs) = TokenSColon : omega_lexer cs
omega_lexer ('&':'&':cs) = TokenAnd : omega_lexer cs
omega_lexer ('|':'|':cs) = TokenOr : omega_lexer cs
omega_lexer ('&':cs) = TokenAnd : omega_lexer cs
omega_lexer ('|':cs) = TokenOr : omega_lexer cs



omega_lexNum cs = TokenInt (read num) : omega_lexer rest
	where (num,rest) = span isDigit cs

isVarChar c = (isAlphaNum c) || (c == '\'') || (c == '_')

omega_lexVar cs =
   case span isVarChar cs of
      	("exists",rest) -> TokenExists  : omega_lexer rest
      	("forall",rest) -> TokenForall  : omega_lexer rest
      	("union",rest)  -> TokenUnion   : omega_lexer rest
      	("or",rest)     -> TokenOr      : omega_lexer rest
      	("and",rest)    -> TokenAnd     : omega_lexer rest
        ("TRUE",rest)   -> TokenTrue    : omega_lexer rest
        ("FALSE",rest)  -> TokenFalse   : omega_lexer rest
      	(var,rest)      -> TokenVar var : omega_lexer rest

-- {[x] -> [y] y = 1+x }
-- {[x] -> [y] y = 2+x }
-- {[x] -> [y] exists ( alpha : ( 1+alpha = y && 2+x = alpha )) }
-- {[x] -> [x+3] }
-- {[x] -> [y] exists ( alpha : ( 1+alpha = y && 2+x = alpha )) }
-- {[x,y] -> [z] y = 2+x && y <= z }
