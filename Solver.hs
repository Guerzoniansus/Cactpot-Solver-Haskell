import Data.List
import Data.Function (on)
import Data.Ord (comparing)

-- Returns the payout in Gil depending on the input 
payout :: Int -> Int
payout 6  = 10000
payout 7  = 36
payout 8  = 720
payout 9  = 360
payout 10 = 80
payout 11 = 252
payout 12 = 108
payout 13 = 72
payout 14 = 52
payout 15 = 180
payout 16 = 72
payout 17 = 180
payout 18 = 119
payout 19 = 36
payout 20 = 306
payout 21 = 1080
payout 22 = 144
payout 23 = 1800
payout 24 = 3600

type Line = (String, [Int])  -- Contains a name and 3 numbers
type ProfitCalculation = (String, Double) -- Contains the name of a line and the average payout

-- Function to split the board into 8 lines
createLines :: [Int] -> [Line]
createLines board = [
    ("Top row", [board !! 0, board !! 1, board !! 2]),
    ("Middle row", [board !! 3, board !! 4, board !! 5]),
    ("Bottom row", [board !! 6, board !! 7, board !! 8]),
    ("Left column", [board !! 0, board !! 3, board !! 6]),
    ("Middle column", [board !! 1, board !! 4, board !! 7]),
    ("Right column", [board !! 2, board !! 5, board !! 8]),
    ("Top left to bottom right", [board !! 0, board !! 4, board !! 8]),
    ("Bottom left to top right", [board !! 6, board !! 4, board !! 2])
    ]

-- Returns the line with the highest average payout
getHighestAverageLine :: [Int] -> ProfitCalculation
getHighestAverageLine board =
    let lines = createLines board
        knownNumbers = filter (/= 0) board  -- Possible numbers for 0 tiles cannot include known numbers
        
        -- Make a list of all lines with the average payout of each
        lineAverages (name, lineValues) =
            let possibleNumbers = [1..9] \\ nub knownNumbers
                averagePayout = getAveragePayout lineValues possibleNumbers
            in (name, averagePayout)
        
        -- Find best line out of all lines
        (bestLineName, bestLineAvg) = maximumBy (comparing snd) (map lineAverages lines)
    in (bestLineName, bestLineAvg)

-- Returns average payout of a line 
getAveragePayout :: [Int] -> [Int] -> Double
getAveragePayout lineValues possibleNumbers =
  let 
    sums = getAllPossibleSums lineValues possibleNumbers
    totalPayout = sum (map payout sums) -- Two loops, not good for performance
  in fromIntegral totalPayout / fromIntegral (length sums)


-- Returns all possible sums a line can have, replacing zeros with numbers from possibleNumbers
getAllPossibleSums :: [Int] -> [Int] -> [Int]
getAllPossibleSums lineValues possibleNumbers =
  let
    -- Replaces index in array with x (i needs to be in range)
    replace array i x = take i array ++ [x] ++ drop (i + 1) array

    -- Find the index of the first zero in the line
    zeroIndex = elemIndex 0 lineValues
  in
    case zeroIndex of
      Just index ->
        let
          -- Generate a list of lineValues with each zero replaced by a possible number
          updatedLineValues = [replace lineValues index number | number <- possibleNumbers]
        in
          -- Use a list comprehension to calculate all possible sums recursively
          concat [getAllPossibleSums updatedLineValue (delete number possibleNumbers) | (updatedLineValue, number) <- zip updatedLineValues possibleNumbers]
      Nothing -> [sum lineValues] -- If there are no zero tiles, return the sum of the line