-- THIS COVERS FIRST PART OF THE VIDEO.
-- Where we have functions of type a -> Ma
-- We created these functions with their corresponding types:
-- createListWithItem a -> Ma
-- addOneAndCreateListWithItem a -> Ma
-- doubleValueAndCreateListWithIt a -> Ma
-- doInsanceCalculation a -> Ma


-- This function has a -> Ma type from the video.
-- [] is a list in Haskell. Which is built as a Monad.
-- So our Monad type will be [] instead of M from the video.
createListWithItem :: a -> [a]
createListWithItem x = [x]

-- same as createListWithItem applies here.
addOneAndCreateListWithItem :: a -> [a]
addOneAndCreateListWithItem x = [x+1]

-- same as createListWithItem applies here.
doubleValueAndCreateListWithIt :: a -> [a]
doubleValueAndCreateListWithIt x = [x*2]

-- same as createListWithItem applies here.
doInsanceCalculation :: a -> [a]
doInsanceCalculation x = [(x + 1) * 3]

-- This function basicly has a -> Ma type from the video.
-- Here we see how we can puzzle our little building blocks together to realize
-- seemingly complex logic.
-- Ensuring the resulting type in compile time.
-- Composing as we need.
shoveIt :: a -> [a]
shoveIt a = createListWithItem a
            >>= addOneAndCreateListWithItem
            >>= doubleValueAndCreateListWithIt
            >>= doInsanceCalculation

-- The following is just synctactic sugar for the functon above. (Besides the if else)
-- That demonstrates that while every computation uses the previous value it
-- does not mutate it. Meaning in every step we take we have access to all
-- previous results. It is called "do notation".
shoveIt2 a = do
    x1 <- createListWithItem a
    x2 <- addOneAndCreateListWithItem x1
    x3 <- doInsanceCalculation x2
    if even x3 then return x3 else return x1
