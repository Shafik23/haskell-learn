import Test.QuickCheck
import Primes
import Data.Maybe


propValidPrimesOnly val = if val < 2 || val >= length primes 
                             then result == Nothing 
                             else isJust result 
                                 where result = isPrime val

propPrimesArePrime val = if result == Just True
                            then length divisors == 0
                            else True
                                where result = isPrime val
                                      divisors = filter ((== 0) . (val `mod`)) [2 .. (val-1)]

propNonPrimesAreComposite val = if result == Just False
                                   then length divisors > 0
                                   else True
                                       where result = isPrime val
                                             divisors = filter ((== 0) . (val `mod`)) [2 .. (val-1)]

propFactorsMakeOriginal val = if result == Nothing
                                 then True
                                 else product (fromJust result) == val
                                     where result = primeFactors val
main :: IO ()
main =
    quickCheck propValidPrimesOnly >>
    quickCheckWith stdArgs {maxSuccess = 1000} propPrimesArePrime >> 
    quickCheckWith stdArgs {maxSuccess = 1000} propNonPrimesAreComposite
