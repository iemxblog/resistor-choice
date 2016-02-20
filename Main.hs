module Main (
    main
) where

import Data.Function
import Data.List

data Resistor = 
    Resistor Float
    | Series Resistor Resistor
    | Parallel Resistor Resistor
    deriving (Eq, Show)

-- | Returns the equivalent value of a combination of resistors.
value :: Resistor -> Float
value (Resistor v) = v
value (Series r1 r2) = value r1 + value r2
value (Parallel r1 r2) = v1 * v2 / (v1 + v2)
    where
        v1 = value r1
        v2 = value r2

-- | Returns the number of resistors in a combination of resistors.
count :: Resistor -> Int
count (Resistor _) = 1
count (Series r1 r2) = count r1 + count r2
count (Parallel r1 r2) = count r1 + count r2

-- | Generate all possible combinations of resistors.
combinations :: [Resistor]      -- ^ List of resistors to combine
                -> Int          -- ^ Maximum depth of combination
                -> [Resistor]
combinations l 0 = l
combinations l n | n > 0 = l ++ [Series r1 r2 | (r1,  r2) <- cwds] ++ [Parallel r1 r2 | (r1, r2) <- cwds, value r1 + value r2 /= 0]
    where 
        cs = combinations l (n-1)
        cwds = couplesWithoutDoubles cs

-- | Generates all couples of items from a list, avoiding doubles.
-- For example, we want (a,b) but not (b,a) : that would be a duplicate couple.
-- Example :
-- couplesWithoutDoubles [1,2,3,4] = [(1,1),(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(3,3),(3,4),(4,4)]
couplesWithoutDoubles :: [a] -> [(a, a)]
couplesWithoutDoubles [] = []
couplesWithoutDoubles (x:xs) = map (\a -> (x, a)) (x:xs) ++ couplesWithoutDoubles xs

-- | Find the best combination of 2 resistors.
findCombination :: (Resistor -> Resistor -> Float)      -- ^ f : Function we want to optimize
                    -> Float                            -- ^ Target we want to reach (we want f r1 r2 == target)
                    -> Float                            -- ^ Threshold (used to filter values that are too far from the target) 
                    -> Int                              -- ^ Maximum depth of combination (see 'combinations') 
                    -> [Resistor]                       -- ^ List of resistors to combine 
                    -> [(Float, Resistor, Resistor)]    -- ^ Result : [(f r1 r2, r1, r2)]
findCombination f tar th d rs = getSimplest . threshold $ [(f r1 r2, r1, r2) | r1 <- cs , r2 <- cs]
    where
        cs = combinations rs d
        threshold = filter (\(a,_,_)-> abs (a-tar) < th)    -- we reject all combinations that are above the threshold (to avoid too much memory usage)
        getSimplest = sortBy (compare `on` (\(_,r1,r2) -> count r1 + count r2)) -- we sort using the total number of resistors


main = mapM_ print $ take 10 $ findCombination (\r1 r2 -> 1 + value r2 / value r1) (20/3.3) 0.001 1 e12
    where
        --e12 = map Resistor $ b ++ map (*10) b   -- we take 2 decades (from 1.0 to 82.0) (if we take more, calculation is too long)
        b = [1.0, 1.2, 1.5, 1.8, 2.2, 2.7, 3.3, 3.9, 4.7, 5.6, 6.8, 8.2]  -- base list of E12 values
        e12 = map Resistor $ concat [map (*10**i) b | i <- [0..6]]
