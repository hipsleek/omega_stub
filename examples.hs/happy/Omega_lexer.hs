module Omega_lexer(lexer) where

import Omega_tokens

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
	| isSpace c = lexer cs
	| isAlpha c = lexVar (c:cs)
	| isDigit c = lexNum (c:cs)
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('<':'=':cs) = TokenLeq : lexer cs
lexer ('>':'=':cs) = TokenGeq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenORB : lexer cs
lexer (')':cs) = TokenCRB : lexer cs
lexer ('{':cs) = TokenOCB : lexer cs
lexer ('}':cs) = TokenCCB : lexer cs
lexer ('[':cs) = TokenOSB : lexer cs
lexer (']':cs) = TokenCSB : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (':':cs) = TokenSColon : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
	where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
	("exists",rest) -> TokenExists : lexer rest
	("forall",rest) -> TokenForall : lexer rest
	("union",rest) -> TokenUnion : lexer rest
	(var,rest)   -> TokenVar var : lexer rest

-- {[x] -> [y] y = 1+x }
-- {[x] -> [y] y = 2+x }
-- {[x] -> [y] exists ( alpha : ( 1+alpha = y && 2+x = alpha )) }
-- {[x] -> [x+3] }
-- {[x] -> [y] exists ( alpha : ( 1+alpha = y && 2+x = alpha )) }
-- {[x,y] -> [z] y = 2+x && y <= z }