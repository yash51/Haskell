legalChars = "aAbBcC"

onlyABC :: [Char] -> Bool
onlyABC x = minimum[elem t legalChars | t <- x]