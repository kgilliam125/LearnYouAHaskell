import System.Random

randoms' :: (RandomGen g, Random a) => Int -> g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen