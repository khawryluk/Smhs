{- 
    GridGui - This creates an interactive GUI for users to control and run housing segregation simulations.

    Key Presses 
      'space bar' -> To start the simulation of the labels switching colors 
      'r'         -> To reset the the labels back to white 
      'Up'        -> To increase the similarity threshold
      'Down'      -> To decrease the similarity theshold
      'Left'      -> To increase the neighborhood size
      'Right'     -> To decrease the neighborhood size.

-}

{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module GridGui(
    GuiCityCtx(..),
    launch
)
where

import qualified GI.Gtk as Gtk 
import qualified GI.Gdk as Gdk
import Data.GI.Base
import GI.GLib as GL (timeoutAdd)
import Data.Text as DT
import Data.Text.Encoding as DTE
import System.Random
import Data.Array.IO
import Control.Monad
import Data.IORef
import System.IO  
import qualified Data.Int as DI
import qualified Debug.Trace as DTT
import qualified Data.Vector as DV
import Grid
data Color = White | Red | Blue | Pink | LightBlue  
data Increment = Increase | Decrease
data GuiCityCtx = GuiCityCtx {
    bluePct::Int,
    redPct::Int,
    emptyPct::Int,
    startingOwnership::[String],
    maxSteps::Int,
    gridSize::Int
    }  
data GUIComponents = GUIComponents { labels :: [Gtk.Label],
                                   rSizeLbl :: Gtk.Label,
                                   simLbl :: Gtk.Label,
                                   rbLbl :: Gtk.Label,
                                   eLbl :: Gtk.Label,
                                   sizeLbl :: Gtk.Label,
                                   stepsLbl :: Gtk.Label,
                                   isRunningLbl :: Gtk.Label,
                                   pctSatLbl :: Gtk.Label
                                   }

data GUIState = GUIState {  cityCtx   :: GuiCityCtx, 
                            city :: City Home,
                            currentStep :: Int,
                            isRunning    :: Bool, 
                           shouldReset   :: Bool, 
                           components    :: GUIComponents
                         }
{-
updateCityAndStep - updates the GUIState with a new city and increments the step.
-}
updateCityAndStep :: GUIState -> City Home -> GUIState
updateCityAndStep (GUIState ctx curCity curStep isRunningVal shouldResetVal compnentsVal) newCity = GUIState ctx newCity (curStep + 1) isRunningVal shouldResetVal compnentsVal

{-
updateNeighborhoodSize - updates the r value in the city used by the simulation and changes the label.
-}
updateNeighborhoodSize :: GUIState -> Increment -> IO(GUIState)
updateNeighborhoodSize state Increase = do
    let (newState, newR) = updateNeighborhoodSizeVal state $ (+) 1
    updateRLabel newState newR
    return newState

updateNeighborhoodSize state Decrease = do
    let (newState, newR) = updateNeighborhoodSizeVal state $ (+) (-1)
    updateRLabel newState newR
    return newState

{-
updateNeighborhoodSizeVal - changes the r value in the city used by the simulation.
-}
updateNeighborhoodSizeVal :: GUIState -> (Int -> Int) -> (GUIState, Int)
updateNeighborhoodSizeVal (GUIState ctx curCity curStep isRunningVal shouldResetVal compnentsVal) changeR = (newState, newR) where
    city@(City r c cityHomes unoccpiedCityHomes rVal threshold) = curCity
    newR = DTT.trace (show rVal ++ "  " ++ show (changeR rVal)) max 0 (changeR rVal)
    newCity = City r c cityHomes unoccpiedCityHomes newR threshold
    newState = GUIState ctx newCity curStep isRunningVal shouldResetVal compnentsVal


{-
updateRLabel - updates the step label. 
-}
updateRLabel :: GUIState -> Int -> IO()
updateRLabel state@(GUIState {currentStep=currentStepVal, cityCtx=ctx}) newR = do
    let newRLabelVal = DT.pack $  "R-size: " ++ show newR
    let currentRLabel = rSizeLbl.components $ state
    set currentRLabel [ #label := newRLabelVal]


{-
updateThreshold - updates the r value in the city used by the simulation and changes the label.
-}
updateThreshold :: GUIState -> Increment -> IO(GUIState)
updateThreshold state Increase = do
    let (newState, newT) = updateThresholdVal state $ (+) 0.05
    updateThresholdLabel newState newT
    return newState

updateThreshold state Decrease = do
    let (newState, newT) = updateThresholdVal state  $ (+) (-0.05)
    updateThresholdLabel newState newT
    return newState

{-
updateThreshold - changes the threshold value in the city used by the simulation.
-}
updateThresholdVal :: GUIState -> (Double -> Double) -> (GUIState, Double)
updateThresholdVal (GUIState ctx curCity curStep isRunningVal shouldResetVal compnentsVal) changeThreshold = (newState, capNewThreshold) where
    city@(City r c cityHomes unoccpiedCityHomes rVal threshold) = curCity
    newThreshold = (changeThreshold threshold)
    capNewThreshold = if newThreshold > 1 then 1 else if newThreshold < 0 then 0 else newThreshold
    newCity = City r c cityHomes unoccpiedCityHomes rVal capNewThreshold
    newState = GUIState ctx newCity curStep isRunningVal shouldResetVal compnentsVal


{-
updateThresholdLabel - updates the step label. 
-}
updateThresholdLabel :: GUIState -> Double -> IO()
updateThresholdLabel state@(GUIState {currentStep=currentStepVal, cityCtx=ctx}) newThreshold = do
    let newThresholdInt = round $ 100 * newThreshold
    let newRLabelVal = DT.pack $  "Similarity: " ++ show newThresholdInt ++ "%"
    let currentRLabel = simLbl.components $ state
    set currentRLabel [ #label := newRLabelVal]
   
{-
updatePctSatLbl - changes the satified lable to show the pct of occupants that are satisfied.
-}
updatePctSatLbl :: GUIState -> IO()
updatePctSatLbl state@(GUIState {city=cityVal}) = do
    let cityHomes = homes cityVal
    let thresholdVal = threshold cityVal
    let satTotal = DV.length.DV.filter (\x -> similarity x >= thresholdVal) $ cityHomes
    let len = DV.length.DV.filter (\x -> owner x /= O) $ cityHomes
    let satPct = (100 * satTotal) `div` len
    let satValText = show $ satPct
    let satLabelText = DT.pack $ "Satisfied: " ++ satValText ++ "%"
    let currentPctSatLbl = pctSatLbl.components $ state
    set currentPctSatLbl [ #label := satLabelText]
{-
updateStepLabel - updates the step label. 
-}
updateStepLabel :: GUIState -> IO()
updateStepLabel state@(GUIState {currentStep=currentStepVal, cityCtx=ctx}) = do
    let maxStepsVal = maxSteps ctx
    let currentStepLabelVal = DT.pack $  "Round: " ++ show currentStepVal ++ " of " ++ show maxStepsVal 
    let currentStepLabel = stepsLbl.components $ state
    set currentStepLabel [ #label := currentStepLabelVal]

paddingStr = "#label_red,\n" ++ 
             "#label_blue,\n" ++ 
             "#label_pink,\n" ++ 
             "#label_lightBlue,\n" ++ 
             "#label_default {\n" ++ 
             "  padding: " 


{- Based on the your grid size and the width and height of the grid, it will 
   create a CSS style for the cells with equal cell dimensions. 
-}
initLabelSize :: Int -> Int -> Int -> String 
initLabelSize gridSize gridWidth gridHeight = let 
  width_padding = ((gridWidth `div` gridSize) `div` 2)
  height_padding = ((gridHeight `div` gridSize) `div` 2)
  top = show height_padding
  left = show width_padding
  bottom =  show height_padding
  right = show width_padding
  in 
    paddingStr ++ top ++ "px " ++ left ++ "px " ++ bottom ++ "px "  ++ right ++ "px \n}\n"

{-
initializeCSSFromString - load css file.
-}
initializeCSSFromString :: String ->  String -> Gtk.Window -> IO () 
initializeCSSFromString labelCSS cssFile window = do 
    provider <- Gtk.cssProviderNew
    cssFileData <- readFile cssFile 
    let contents = cssFileData ++ labelCSS
    Gtk.cssProviderLoadFromData provider (DTE.encodeUtf8.DT.pack $ contents)
    screen <- #getScreen window
    Gtk.styleContextAddProviderForScreen screen provider  (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

{-
initState - Creates a GUIState  
-}
initState :: GuiCityCtx -> IO (IORef GUIState)  
initState ctx@(GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize) = do  
    rSizeLbl <- new Gtk.Label [#label := DT.pack "R-size: 2" , 
                              #name := "status_labels", 
                              #justify := Gtk.JustificationLeft]
    simLbl <- new Gtk.Label [#label := DT.pack "Similarity: 50%"  , 
                            #name := "status_labels", 
                            #justify := Gtk.JustificationLeft]
    let rbText = "Red/Blue: " ++ show rPct ++ "% / " ++ show bPct  ++ "%"
    rbLbl <- new Gtk.Label [#label := DT.pack rbText , 
                           #name := "status_labels", 
                           #justify := Gtk.JustificationLeft]
    let eText = "Empty: " ++ show ePct ++ "%"
    eLbl <- new Gtk.Label [#label := DT.pack eText  , 
                          #name := "status_labels", 
                          #justify := Gtk.JustificationLeft]
    let sizeText = "Size: " ++ show gSize ++ "X" ++ show gSize
    sizeLbl <- new Gtk.Label [#label := DT.pack sizeText , 
                             #name := "status_labels", 
                             #justify := Gtk.JustificationLeft]
    let stepsText =  "Round: 0 of " ++ show maxStepsVal 
    stepsLbl<- new Gtk.Label [#label := DT.pack stepsText , 
                             #name := "status_labels", 
                             #justify := Gtk.JustificationLeft]
    isRunningLbl <- new Gtk.Label [#label := DT.pack "Start" , 
                                  #name := "status_labels", 
                                  #justify := Gtk.JustificationLeft]
    pctSatLbl <- new Gtk.Label [#label := DT.pack "Satisfied: 0%" , 
                               #name := "status_labels", 
                               #justify := Gtk.JustificationLeft]
    labels <- sequence $ Prelude.replicate (gSize^2) (new Gtk.Label [#name := "label_default"])

    let components = GUIComponents labels rSizeLbl simLbl rbLbl eLbl sizeLbl stepsLbl isRunningLbl pctSatLbl

    -- Define the GUIState 
    let state = GUIState ctx None 0 False False components

    -- Define a newIORef for the state 
    newIORef state 

{-
convertTo32 - converts int to int32 for working with grids.
-}
convertTo32 :: Int -> DI.Int32
convertTo32 int = fromIntegral int

createLabelGrid ::  IORef (GUIState) -> Int -> IO (Gtk.Grid)
createLabelGrid stateRef gridWidth = do 

    -- Define a new Grid layout for the labels 
    {- 
        http://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects-Grid.html#g:3  
    -}
    gridLayout <- Gtk.gridNew

    -- Retrieve the pure state from the stateRef 
    state <- readIORef stateRef

   -- Add grid labels
    let gridLen = Prelude.length $ labels.components $ state
    let indices = [0 , 1..gridLen - 1] 
    let getRow idx = convertTo32.div idx $ gridWidth
    let getCol idx = convertTo32 $ if idx == 0 then 0 else mod idx gridWidth
    let attachToLayout = Gtk.gridAttach gridLayout
    let states = labels.components $ state
    let attachToGrid idx =  let item = states !! idx
                                row = getRow idx
                                col = getCol idx
                            in attachToLayout item col row 1 1   

    mapM attachToGrid indices

    set gridLayout [ #margin := 10 ]

    -- Set the CSS style for the container 
    set gridLayout [#name :="labels_container"]

    return gridLayout 

-- | Randomly shuffle a list
--   /O(N)/
-- Taken from here: https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    where
    n = Prelude.length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
{-
initCityGUIState - creates the starting state of the city. 
-}
initCityGUIState :: IORef (GUIState) -> Int -> Double -> IO (GUIState)
initCityGUIState stateRef neighborhoodSize thresholdVal= do
    state <- readIORef stateRef
    let ctx@(GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize) = cityCtx state
    shuffleOwnershipInit <- shuffle ownershipInit
    let city = createCity gSize gSize neighborhoodSize thresholdVal shuffleOwnershipInit
    let cityWithSimScores = updateSimilarityScores city
    let newGUIState = GUIState ctx cityWithSimScores 0 False False (components state)
    writeIORef stateRef newGUIState
    return newGUIState

{-
getRAndThreshold - if the boolean is true, return defualt values, otherwise return the r and threshold from the city. 
-}
getRAndThreshold :: City Home -> Bool -> (Int, Double)
getRAndThreshold _ True = (2, 0.5)
getRAndThreshold city _ = (neighborhoodSize, thresholdVal) where
    (City{r=neighborhoodSize, threshold = thresholdVal}) = city
{-
setUpSimulation - generate a new random city and update the grid accordingly.
-}            
setUpSimulation ::   IORef (GUIState) -> Bool -> IO (GUIState)
setUpSimulation stateRef useDefaults = do
    -- Create the initial city and update the stateRef
    state <- readIORef stateRef
    let (neighborhoodSize, thresholdVal) = getRAndThreshold (city state) useDefaults
    initGuiState <- initCityGUIState stateRef neighborhoodSize thresholdVal
    writeIORef stateRef initGuiState
    updateGridColors stateRef
    return  initGuiState

{-
handleKeyPress - listens for user input.
-}
hanldeKeyPress :: IORef (GUIState) -> Gdk.EventKey -> IO Bool
hanldeKeyPress stateRef eventKey = do 
    keyVal <- get eventKey #keyval

    -- Retrieve the pure state from the stateRef 
    state <- readIORef stateRef

    newGUIState <- case keyVal of 
        -- 'space bar' maps to Unicode value 32 
        32 -> do   
            {- 'space bar' starts or pauses the simulation 
            -}
            if not(isRunning state)  
                then do 
                    let startGUIState = GUIState (cityCtx state) (city state) 0 True False (components state)
                    let runningLbl = isRunningLbl.components $ startGUIState
                    set runningLbl [ #label := "Running"]
                    timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 750 (handleTimeout stateRef)
                    return startGUIState
                else return state 
        -- 'r' maps to Unicode value 114
        114 -> do 
            {- 'r' means we want to reset the grid of labels back to zero and pause the simulation of switching
                   between the colors of the labels 
            -}
            -- We stop the running of the simulation and once stopped we specify that we should reset the grid 
            print "Resetting Simulation"
            let newGUIState = GUIState (cityCtx state) (city state) 0 False True (components state)
            writeIORef stateRef newGUIState
            newSimState <- setUpSimulation stateRef False
            return newSimState
        -- Left
        0xff51 -> do
            if not(isRunning state)  
                then do 
                    adjGUIState <- updateNeighborhoodSize state Increase
                    return adjGUIState
                else return state 

        -- Right
        0xff53 -> do
            if not(isRunning state)  
                then do 
                    adjGUIState <- updateNeighborhoodSize state Decrease
                    return adjGUIState
                else return state 

        -- Up
        0xff52 -> do
            adjGUIState <- updateThreshold state Increase
            return adjGUIState

        -- Down
        0xff54 -> do
            adjGUIState <- updateThreshold state Decrease
            return adjGUIState

        _ -> return state 
    -- Update the IORef for the GUIState 
    writeIORef stateRef newGUIState
    return True 

{-
   getColor - Given a home and threshold value return a color for the grid label.
-}
getColor :: Home -> Double -> Color 
getColor (Home {owner=R, similarity=sim}) threshold  | sim >= threshold = Red
                                                    | otherwise = Pink
getColor (Home {owner=B, similarity=sim}) threshold  | sim >= threshold = Blue
                                                    | otherwise = LightBlue
getColor (Home {owner=O}) _  = White

{-
labelHomeClass - given a home and threshold value get the class for the color label. 
-}
labelHomeClass :: Double -> Home -> Text
labelHomeClass thresholdVal home = labelClass.getColor home $ thresholdVal

{-
labelClass - get the color label for css from a given color.
-}
labelClass :: Color -> Text
labelClass color = case color of 
    White -> "label_default"
    Red ->  "label_red"
    Blue ->  "label_blue"
    Pink ->  "label_pink"
    LightBlue ->  "label_lightBlue"

updateGridColors :: IORef(GUIState) -> IO()
updateGridColors stateRef = do
    state <- readIORef stateRef
    let cityVal@(City{threshold=thresholdVal}) = city state
    let(cityAfterRelocation, moveHappened) =  relocateHomes cityVal

    -- functions for updateing the labels of the grid.
    let changeLabel labelClassVal idx = set ((labels.components $ state) !! idx) [#name := labelClassVal]
    let updateLabel idx = let labelClassVal = labelHomeClass thresholdVal $ (homes cityAfterRelocation) DV.! idx
                      in changeLabel labelClassVal idx 
    let indices = [0,1.. (DV.length (homes cityAfterRelocation) - 1)]
    mapM updateLabel indices
    return ()

handleTimeout :: IORef (GUIState) -> IO Bool 
handleTimeout stateRef = do 
    -- Retrieve the pure state from the stateRef 
    state <- readIORef stateRef
    let cityVal@(City{threshold=thresholdVal}) = city state
    let(cityAfterRelocation, moveHappened) =  relocateHomes cityVal

    let resetGui =  GUIState (cityCtx state) cityVal 0 False False (components state)

    -- Call this to change the label to indicate the simulation is complete and then return a reset GUIState.
    let endSimulation = do
        let runningLbl = isRunningLbl.components $ state
        set runningLbl [ #label := "Complete"]
        return (False, resetGui)

    
    {- Check to see if isRunning is set. If it is then we need to change 
       to the next color in the sequnce. If isRunning is False and shouldReset is True then 
       we need to cancel this timer and reset the grid of labels and our state information. If 
       isRunning is False then we just need to cancel this timer. 
     -} 
    (result, newGUIState) <- case ((isRunning state), (shouldReset state)) of 
        (True, _) -> do
            let  endSim = currentStep state == (maxSteps $ cityCtx state) || moveHappened == False
            if endSim then endSimulation else do 
                updateGridColors stateRef

                -- Update the GUIState with the nxtColor 
                let guiStateWithRelocatedCity = updateCityAndStep state cityAfterRelocation
                updateStepLabel guiStateWithRelocatedCity
                updatePctSatLbl guiStateWithRelocatedCity
                -- Return a tuple. The first component is whether this timer should be canceled or not
                -- and second component is the updated GUI state. 
                return (True, guiStateWithRelocatedCity)

        (False, True) -> do 
            -- Reset the state! 
            endSimulation
        (False, _) -> endSimulation

    -- Update the IORef for the GUIState 
    writeIORef stateRef newGUIState

    -- Return True if you want this handleTimeout function to continue to be called after performing  
    -- otherwise cancel the timer by returning False. 
    return result  

launch :: GuiCityCtx -> IO ()
launch ctx@(GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize)= do

  Gtk.init Nothing

  win <- new Gtk.Window [ #title  := "Schelling’s Model of Housing Segregation", 
                          #defaultHeight  := 600,  
                          #defaultWidth := 800,
                          #name := "window"
                        ]

  -- If the user clicks the close button on the window then shutdown the program 
  on win #destroy Gtk.mainQuit

  -- Use this function inside your final-project to get the label sizing to work correctly inside 
  -- the gui 
  let gridWidth = (800 - 20) -- Using the width of the window minus some marigin spacing 
  let gridHeight = (400 - 20)  -- Using the height of the window minus some marigin spacing 

  let labelStyleCSS = initLabelSize gSize gridWidth gridHeight 
  
  -- Initialize the Application level styling with CSS styling information for the 
  -- label sizes. 
  initializeCSSFromString labelStyleCSS "grid.css" win 

  -- Initialize the state of the program 
  stateRef <- initState ctx

  -- Create the widgets for the application 
  labelsGridLayout  <- createLabelGrid stateRef gSize

  -- Create the application main layout 
  layoutWindow <- Gtk.boxNew Gtk.OrientationVertical 0
  #add layoutWindow labelsGridLayout 
  state <- readIORef stateRef
  let comp = components state
  -- Add Initial Text Compenents

  textContainer <- Gtk.boxNew Gtk.OrientationHorizontal 0 
  -- Lables that change each step
  leftTextWindow <- Gtk.boxNew Gtk.OrientationVertical 0 
  #add leftTextWindow $ stepsLbl comp
  #add leftTextWindow $ sizeLbl comp
  #add leftTextWindow $ rSizeLbl comp

  -- Lables that never change
  centerTextWindow <- Gtk.boxNew Gtk.OrientationVertical 0 
  #add centerTextWindow $ pctSatLbl comp
  #add centerTextWindow $ rbLbl comp
  #add centerTextWindow $ simLbl comp

  -- Lables that users can modify
  rightTextWindow <- Gtk.boxNew Gtk.OrientationVertical 0 
  #add rightTextWindow $ isRunningLbl comp
  #add rightTextWindow $ eLbl comp

  -- Add them to the layout
  #add textContainer leftTextWindow 
  #add textContainer centerTextWindow 
  #add textContainer rightTextWindow
  #add layoutWindow textContainer

  -- Add the main application layout to the window 
  #add win layoutWindow 


  -- Show the window 
  #showAll win

  -- Call the main action of the GTK library 
 
  setUpGuiState <- setUpSimulation stateRef True
  writeIORef stateRef setUpGuiState
    -- Add a global function to handle key presses in application 
  -- You need to make sure you pass in your IORef of the GUIState 
  -- to the key press 
  on win #keyPressEvent (hanldeKeyPress stateRef) 
  Gtk.main