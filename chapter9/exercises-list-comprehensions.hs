mySqr = [x^2 | x <- [1..10]]


pairNumbers = [x | x <- mySqr, rem x 2 == 0]

twentyOnePairs = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

threeOneTwoFourPairs = take 5 twentyOnePairs


removeLowerCaseLetters :: String -> String
removeLowerCaseLetters letters = [c | c <- letters, c `elem` ['A'..'Z']]

extractVowels xs = [x | x <- xs, x `elem` "aeiou"]
