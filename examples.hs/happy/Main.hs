---
-- This is an example using the new Omega module.
--
-- $Id: Main.hs,v 1.2 2003-07-28 05:11:49 raz Exp $
--

-- R1 := { [i] -> [j] : j = i + 1 };
-- R2 := { [i] -> [j] : j = i + 2 };
-- R1;
-- R2;
-- R1(R2);
-- R2(R1);
-- R1(R1);

module Main(main) where

import Omega
import Omega_types
import Foreign
import Foreign.C
import qualified Omega_stub
import Omega_util
import Omega_parser

main = do
    putStr "[START]\n"
    str <- getLine
    putStr ((show (extract_rformula str)) ++ "\n")
    putStr "[DONE]\n"

-- {[x] -> [y] y = 1+x }
-- {[x] -> [y] y = 2+x }
-- {[x] -> [y] exists ( alpha : ( 1+alpha = y && 2+x = alpha )) }
-- {[x] -> [x+3] }
-- {[x] -> [y] exists ( alpha : ( 1+alpha = y && 2+x = alpha )) }
-- {[x,y] -> [z] y = 2+x && y <= z }
-- U:={[n,s,d,t]->[n-1,s',d',t']:
--       n>0 & ((s'=s & d'=t & t'=d)
--         or exists(t2,d2,s2,t1,s1,d1: s2=s1-1 & d2=d1+1 & t2=t1
--               & s1=s-(n-1) & d1=d & t1=t+(n-1)
--               & s'=t2 & d'=d2 & t'=s2))};
