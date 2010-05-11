fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

prop_fibs = and $ zipWith (==) fibs [1,1,2,3,5,8,13,21,34,55,89,144]

smallfibs = takeWhile (<= 10^999) fibs

answer = length smallfibs - 1

prop_answer = 1000 == length ((show . last) smallfibs)

main = print answer
