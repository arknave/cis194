--Task: Implement the sieve in point-free form
--Status: Implemented correctly, but not purely point-free.
import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . badNum

badNum :: Integer -> [Integer] 
badNum n = [1..n] \\ [(x+y+2*x*y) | x <- [1..n], y <- [1..n]]

