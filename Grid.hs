module Grid 
(
Home(..),
City(..),
Ownership(..),
createCity,
updateSimilarityScores,
relocateHomes
)
where
import qualified Data.Vector as DV
import qualified Data.List as DL
import Debug.Trace as DT
data Ownership = O|R|B deriving (Eq, Show)

data Home = Home {
    row::Int,
    column::Int,
    owner::Ownership,
    similarity::Double
    } deriving (Show)

data City a = None | City {
    rowCount::Int,
    colCount::Int,
    homes::DV.Vector a,
    unoccpiedHomes::[Int],
    r :: Int, 
    threshold :: Double
} deriving (Show)


instance Functor City where
    fmap f city@(City r c cityHomes unoccpiedCityHomes rVal threshold) = newCity where 
        newHomes = DV.map f cityHomes
        newCity = City r c newHomes unoccpiedCityHomes rVal threshold


{-
createCity - Does the initial city creation.
-}
createCity :: Int -> Int -> Int -> Double -> [String] -> City Home
createCity rows cols neighborhoodSize thresholdVal ownershipInit = newCity where
    ownershipVals = map toOwnership ownershipInit
    unoccpiedHomes = DL.elemIndices O ownershipVals
    startingHomes = getHomes cols ownershipVals
    newCity = City rows cols startingHomes unoccpiedHomes neighborhoodSize thresholdVal

{-
    toOwernship - convert a string into a type Ownership
-}
toOwnership :: String -> Ownership
toOwnership "R" = R
toOwnership "B" = B
toOwnership "O" = O
toOwnership ownership = error ("Coult not parse " ++ show ownership ++ " as a type of ownership.")

{-
    getHomes - Used in the start to convert a list of ownership into a data vector of homes.
-}
getHomes :: Int -> [Ownership] -> DV.Vector Home
getHomes cols ownerships = homes where
    -- Tuples of index and ownership
    indexedOwnership = zip [0,1..] ownerships
    -- get row and col based on index
    toRowCol idx = (row, col) where
        col = if idx == 0 then 0 else mod idx cols
        row = div idx cols
    -- Create a home. Similarity score is defaulted to 0.
    toHome (index, ownership) = newHome where
        (nrow, ncol) = toRowCol index
        newHome = Home nrow ncol ownership 0
    -- Create list and then Data.Vector of homes to return.
    listOfHomes = map toHome indexedOwnership
    homes = DV.fromList listOfHomes 

{-
    getNeighborhood - Grabs the homes from a city that are within r of the provided home.
-}
getNeighborhood :: Home -> City Home-> DV.Vector Home
getNeighborhood (Home{row=hrow, column=hcol}) (City{homes=homes, r=r}) = neighborhood where
    -- Checks if a home is within r of the provided home.
    inRange (Home{row=orow, column=ocol}) = rowsInRange && colsInRange where
        minRow = hrow - r
        maxRow = hrow + r
        rowsInRange = orow >= minRow && orow <= maxRow 
        minCol = hcol - r
        maxCol = hcol + r
        colsInRange = ocol >= minCol && ocol <= maxCol
    -- Grabs all homes that are in range
    neighborhood = DV.filter inRange homes

{-
    getSimilarityScore - gets a home with an updated similar score based on the city. 
-}
getSimilarityScore :: Home -> City Home -> Home
getSimilarityScore home@(Home {owner=O}) city = home
getSimilarityScore home@(Home hrow hcol howner _) city = homeWithSimScore where
    neighborhood = getNeighborhood home city
    isSimilar home = owner home == howner  
    similarInNeighborhood = DV.filter isSimilar neighborhood
    len = fromIntegral.DV.length 
    simScore = len similarInNeighborhood / len neighborhood 
    homeWithSimScore =  Home hrow hcol howner simScore


{-
    updateSimilarityScores - updates the similarity scores for every home
-}
updateSimilarityScores :: City Home -> City Home
updateSimilarityScores city = cityWithSimilarityScores where
    cityWithSimilarityScores =  fmap (\x -> getSimilarityScore x city) city
{-
    relocateHomes - this processes 1 turn of iterating through the grid and moving homes around 
-}
relocateHomes :: City Home -> (City Home, Bool)
relocateHomes city@(City{homes=cityHomes}) = (movedCity, movesHappened) where
    indices = [0,1..(length cityHomes - 1)]
    (movedCity, movesHappened) = relocate city indices False

{-
    relocated - tries to move a home at the head of the provided list. down when no more indices to process.
-}
relocate :: City Home -> [Int] -> Bool -> (City Home, Bool)
relocate city [] movesHappened = DT.trace(show "exited") (city, movesHappened)
relocate city l@(x:xs) movesHappened = nextRelocation where
    (newCity, relocateHappened) = moveHome city x 
    movesHappenedAfterRelocate = movesHappened || relocateHappened
    nextRelocation = ($!) relocate newCity xs movesHappenedAfterRelocate
{-
    moveHomes - updates city if there is a better places for the given home to relocate to. 
-}
moveHome :: City Home -> Int -> (City Home, Bool)
moveHome city@(City r c cityHomes unoccpiedCityHomes rVal threshold) idx = (cityAfterRelocation, moveHappend) where
    home@(Home hrow hcolumn hOwner simScore) = cityHomes DV.! idx
    currentHomeIfRelocated = Home hrow hcolumn O 0
    -- Find other unoccipied homes that might be better
    emulatedHomes = map (\x -> (x, emulateHome city x hOwner)) unoccpiedCityHomes
    relocationOptions = filter(\(idx, home) -> similarity home >= threshold) emulatedHomes
    minSimScore = foldr (\(idx, home) accum -> if similarity home < accum then similarity home else accum) 1.0 relocationOptions
    bestRelocationOptions = filter(\(idx, home) -> similarity home == minSimScore) relocationOptions
    (bOIdx, bestOption) =  head bestRelocationOptions
    newUnoccpiedCityHomes = DL.delete bOIdx unoccpiedCityHomes ++ [idx]
    -- Swap in the homes
    newCityHomes = cityHomes DV.// [(idx, currentHomeIfRelocated), (bOIdx, bestOption)]
    newCity = City r c newCityHomes newUnoccpiedCityHomes rVal threshold
    newCityWithScores = updateSimilarityScores newCity
    (cityAfterRelocation, moveHappend)  | hOwner == O = (city, False)
                                       | simScore >= threshold = (city, False)
                                       | length bestRelocationOptions == 0 = (city, False)
                                       | otherwise = (newCityWithScores, True)

{-
    emulateHome - takes an index of an unoccupied home and returns a home as if it were occupied by the given owner type.
-}
emulateHome :: City Home -> Int -> Ownership -> Home
emulateHome city@(City r c cityHomes unoccpiedCityHomes rVal threshold) idx newOwner = homeAfterRelocate where
    (Home hrow hcolumn _ _) = cityHomes DV.! idx
    fakeHome = Home hrow hcolumn newOwner 0
    fakeCityHomes = cityHomes DV.// [(idx, fakeHome)]
    fakeCity = City r c fakeCityHomes unoccpiedCityHomes rVal threshold
    homeAfterRelocate = getSimilarityScore fakeHome fakeCity

