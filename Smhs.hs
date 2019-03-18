{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Smhs where
    import System.Environment
    import Grid
    import Debug.Trace as DT
    import GridGui
    import qualified Data.Vector as DV

    statement :: String
    statement = "Usage: smhs (-t  <grid file> <R> <threshold> <max_steps>) || (<grid size> <red_percentage> <blue_percentage> <empty_percentage> <max_steps>)"
    
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

    main = do 
        args <- getArgs
        case args of
            ("-t":gridFile:r:threshold:maxSteps:_) -> runTextMode gridFile r threshold maxSteps
            (gridSize:redPct:bluePct:emptyPct:maxSteps:_) -> runGuiMode gridSize redPct bluePct emptyPct maxSteps
            _ -> print $ statement
    
    runTextMode :: String -> String -> String -> String -> IO ()
    runTextMode = undefined
        
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
        let ctx = GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize
        launch ctx
    