module Main(main) where

import Omega
import PFOmega
import Omega_types
import Omega_parser
import Foreign
import Foreign.C
import qualified Omega_stub
import Prelude hiding ((&&),(||))


main = do
    putStr "[START]\n"

    let s1_f = RFormula (\t -> Formula ( t `geq` ((1::Int) ) && ( (10::Int) `geq` t )))
        s2_f = extract_rformula "{ [t] : 1 <= t <= 10 }"
    putStr ("s1_f: " ++ (show s1_f) ++ "\n")
    putStr ("s2_f: " ++ (show s2_f) ++ "\n")

    putStr "[DONE]\n"


