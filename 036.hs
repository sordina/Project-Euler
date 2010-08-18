-- Verified!

import Base
import Palindrome

main = print answer
answer = sum doublePalindromes
doublePalindromes = filter isDoublePalindrome [1..10^6-1]
isDoublePalindrome x = f 10 x && f 2 x
  where
    f b = isPalindrome . toBase b
