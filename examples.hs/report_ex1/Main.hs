module Main(main) where

import Omega
import PFOmega
import Omega_types
import Omega_parser
import Foreign
import Foreign.C
import qualified Omega_stub
import Prelude hiding ((&&),(||))


main = 
  putStr "[START]\n" >>
  let phiFST = extract_rformula "{[f_7,f_8,f_6]: (f_8 < f_7) || (-f_8 <= f_7 <= f_8) || (f_7 <= f_8 <= -f_7-1 && f_6 <= 0)}" in
  let phiREC = extract_rformula "{[f_7,f_8,f_6]: f_8 < f_7 || -f_8 <= f_7 <= f_8}" in
  let typeINV = extract_rformula "{[f_7,f_8,f_6]: 1 <= f_6}" in
  gist (["f_7","f_8","f_6"],[],phiFST) (["f_7","f_8","f_6"],[],typeINV) >>= \rfFST ->
  gist (["f_7","f_8","f_6"],[],phiREC) (["f_7","f_8","f_6"],[],typeINV) >>= \rfREC ->
  putStr("GIST_FST: " ++ rformula_print_formula_to_string rfFST ["f_7","f_8","f_6"]++"\n") >>
  putStr("GIST_REC: " ++ rformula_print_formula_to_string rfREC ["f_7","f_8","f_6"]++"\n") >>
  putStr "[DONE]\n"

-- main = do
--    putStr "[START]\n"
--
--    let s1_f = RFormula (\t -> Formula ( t `geq` ((1::Int) ) && ( (10::Int) `geq` t )))
--        s2_f = extract_rformula "{ [t] : 1 <= t <= 10 }"
--    putStr ("s1_f: " ++ (show s1_f) ++ "\n")
--    putStr ("s2_f: " ++ (show s2_f) ++ "\n")
--    putStr "[DONE]\n"
