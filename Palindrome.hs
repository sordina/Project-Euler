module Palindrome (isPalindrome)
where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x
