--{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Smhs where
    import System.Environment
    import Grid
    import Debug.Trace as DT
  --  import GridGui
    import qualified Data.Vector as DV
    import  Data.List.Split

    statement :: String
    statement = "Usage: smhs (-t  <grid file> <R> <threshold> <max_steps>) || (<grid size> <red_percentage> <blue_percentage> <empty_percentage> <max_steps>)"
    
    main = do 
        args <- getArgs
        case args of
            ("-t":gridFile:r:threshold:maxSteps:_) -> runTextMode gridFile r threshold maxSteps
            (gridSize:redPct:bluePct:emptyPct:maxSteps:_) -> runGuiMode gridSize redPct bluePct emptyPct maxSteps
            _ -> print $ statement

    {-
    getValidPct - reads in a string argument and tries to convert it to an int between 0 and 100. If it can't throw an error.
    -}
    getValidPct :: String -> String -> Int
    getValidPct variableName pctString = validPct where
        pct = read pctString :: Int
        validPct | pct >= 0 && pct <= 100 = pct
                | otherwise = error (variableName ++ " is not between 0 and 100.")
    
    {-
    getValidPct - reads in a string argument and tries to convert it to an int between 5 and 15. If it can't throw an error.
    -}
    getValidGrid :: String -> Int
    getValidGrid sizeString = validSize where
        size = read sizeString :: Int
        validSize | size >= 5 && size <= 15 = size
                | otherwise = error ("Grid size is not between 5 and 15.")


    {-
        runTextMode - opens a grid file and runs the simulation according to 
        the provided r, threshold and max steps.
    -}
    runTextMode :: String -> String -> String -> String -> IO ()
    runTextMode gridFile r threshold maxSteps = do
        let neighborhoodSize = read r :: Int
        let thresholdVal = read threshold :: Double
        let maxStepsVal = read maxSteps :: Int
        (rows, cols, ownershipInit) <- readGridFile gridFile
        let city = createCity rows cols neighborhoodSize thresholdVal ownershipInit
        let cityWithSimScores = updateSimilarityScores city
        runTextSimulation cols 0 maxStepsVal city


    runTextSimulation ::  Int -> Int -> Int -> City Home -> IO()
    runTextSimulation cols currentStep maxSteps city = do
        let (cityAfterRelocation, moveHappened) =  relocateHomes city
        let shouldStop = currentStep == maxSteps || moveHappened == False 
        let owners = DV.map owner (homes $ city)
        let ownersList = chunksOf cols (DV.toList owners)
        if shouldStop then mapM (print $ show) ownersList
                     else runTextSimulation cols (currentStep + 1) maxSteps cityAfterRelocation

    readGridFile :: String -> IO (Int, Int, [String])
    readGridFile gridFile = do
        gridFileContent <- readFile gridFile
        let (rows:cols:startingSpots) = lines gridFileContent
        let rowVal = read rows ::Int
        let colVal = read cols ::Int
        let ownershipStrings = mconcat.map words $ startingSpots
        return(rowVal, colVal, ownershipStrings)

    {-
        runGuiMode - creates a fake city according to the provided parameters and runs
        the similuation according to how the user interacts with the GUI.
    -}
    runGuiMode :: String -> String -> String -> String -> String -> IO ()
    runGuiMode gridSize redPct bluePct emptyPct maxSteps = do
        -- Convert args to ints
        let maxStepsVal = read maxSteps :: Int
        let rPct = getValidPct "Red Percent" redPct
        let bPct = getValidPct "Blue Percent" bluePct
        let ePct = getValidPct "Empty Percent" emptyPct
        let gSize = getValidGrid gridSize
        -- Determine number of cells that are red, blue or empty.
        let totalCells = gSize^2
        let getCells pct maxCells = div (pct * maxCells) 100  
        let emptyCells = getCells ePct totalCells
        let occupiedCells = totalCells - emptyCells
        let redCells = getCells rPct occupiedCells
        let blueCells = getCells bPct occupiedCells
        -- Create list of R, B, and O 
        let ownershipInit = take redCells (repeat "R") ++ take blueCells (repeat "B") ++ take emptyCells (repeat "O") 
       -- let ctx = GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize
        print "hi"
        --launch ctx
    