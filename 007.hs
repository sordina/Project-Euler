module Main (main)
where
import Prime

-- Lists are zero indexed
--
main = putStrLn . show $ primes !! 10000
