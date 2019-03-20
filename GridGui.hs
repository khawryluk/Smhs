{- 
    This program contains a 2x2 grid of labels that switch between the colors Red, Blue, Yellow. 

    Key Presses 
      'space bar' -> To start the simulation of the labels switching colors 
      'r'         -> To reset the the labels back to white 
      'p'         -> To pause the simulation 

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
updateCityAndStep :: GUIState -> City Home -> GUIState
updateCityAndStep (GUIState ctx curCity curStep isRunningVal shouldResetVal compnentsVal) newCity = GUIState ctx newCity (curStep + 1) isRunningVal shouldResetVal compnentsVal

updateStepLabel :: GUIState -> IO()
updateStepLabel state@{GUIState {currentStep=currentStepVal, maxSteps=maxStepsVal}} = do
    let currentStepLabelVal = "Round: " ++ show currentStepVal ++ " of " ++ show maxStepsVal 
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

initializeCSSFromString :: String ->  String -> Gtk.Window -> IO () 
initializeCSSFromString labelCSS cssFile window = do 
    provider <- Gtk.cssProviderNew
    cssFileData <- readFile cssFile 
    let contents = cssFileData ++ labelCSS
    Gtk.cssProviderLoadFromData provider (DTE.encodeUtf8.DT.pack $ contents)
    screen <- #getScreen window
    Gtk.styleContextAddProviderForScreen screen provider  (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

-- Creates a GUIState  
initState :: GuiCityCtx -> IO (IORef GUIState)  
initState ctx@(GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize) = do  
    rSizeLbl <- new Gtk.Label [#label := DT.pack "R-size: 2" , 
                              #name := "status_labels", 
                              #justify := Gtk.JustificationLeft]
    simLbl <- new Gtk.Label [#label := DT.pack "Similarity: TBD"  , 
                            #name := "status_labels", 
                            #justify := Gtk.JustificationLeft]
    let rbText = "Red/Blue: " ++ show rPct ++ " " ++ show bPct
    rbLbl <- new Gtk.Label [#label := DT.pack rbText , 
                           #name := "status_labels", 
                           #justify := Gtk.JustificationLeft]
    let eText = "Empty: " ++ show ePct
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
                                row = DTT.trace(show $ getRow idx) getRow idx
                                col = DTT.trace(show $ getCol idx) getCol idx
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
initCityGUIState :: IORef (GUIState) -> IO (GUIState)
initCityGUIState stateRef = do
    state <- readIORef stateRef
    let ctx@(GuiCityCtx bPct rPct ePct ownershipInit maxStepsVal gSize) = cityCtx state
    shuffleOwnershipInit <- shuffle ownershipInit
    let defaultNeighborhoodSize = 2
    let defaultThresholdVal = 0.50
    let city = createCity gSize gSize defaultNeighborhoodSize defaultThresholdVal shuffleOwnershipInit
    let cityWithSimScores = updateSimilarityScores city
    let newGUIState = GUIState ctx cityWithSimScores 0 True False (components state)
    writeIORef stateRef newGUIState
    return newGUIState
            

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
                    print "Running Simulation"
                    -- Create the initial city and update the stateRef
                    initGuiState <- initCityGUIState stateRef
                    let runningLbl = isRunningLbl.components $ initGuiState
                    set runningLbl [ #label := "Running"]
                    timeoutAdd (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION) 500 (handleTimeout stateRef)
                    return  initGuiState
                else return state 
        -- 'r' maps to Unicode value 114
        114 -> do 
            {- 'r' means we want to reset the grid of labels back to zero and pause the simulation of switching
                   between the colors of the labels 
            -}
            -- We stop the running of the simulation and once stopped we specify that we should reset the grid 
            print "Resetting Simulation"
            let newGUIState = GUIState (cityCtx state) (city state) 0 False True (components state)
            return newGUIState

        _ -> return state 
    -- Update the IORef for the GUIState 
    writeIORef stateRef newGUIState
    print "exited event handler"
    return True 

getColor :: Home -> Double -> Color 
getColor (Home {owner=R, similarity=sim}) threshold  | sim >= threshold = Red
                                                    | otherwise = Pink
getColor (Home {owner=B, similarity=sim}) threshold  | sim >= threshold = Blue
                                                    | otherwise = LightBlue
getColor (Home {owner=O}) _  = White

labelHomeClass :: Double -> Home -> Text
labelHomeClass thresholdVal home = labelClass.getColor home $ thresholdVal

labelClass :: Color -> Text
labelClass color = case color of 
    White -> "label_default"
    Red ->  "label_red"
    Blue ->  "label_blue"
    Pink ->  "label_pink"
    LightBlue ->  "label_lightBlue"

handleTimeout :: IORef (GUIState) -> IO Bool 
handleTimeout stateRef = do 
    -- Check the console window to seee that this is being print ~5 seconds 
    putStrLn "Calling handleTimeout"

    -- Retrieve the pure state from the stateRef 
    state <- readIORef stateRef
    print $ unoccpiedHomes.city $ state
    let cityVal@(City{threshold=thresholdVal}) = city state
    let(cityAfterRelocation, moveHappened) =  relocateHomes cityVal

    -- functions for updateing the labels of the grid.
    let changeLabel labelClassVal idx = set ((labels.components $ state) !! idx) [#name := labelClassVal]
    let updateLabel idx = let labelClassVal = labelHomeClass thresholdVal $ (homes cityAfterRelocation) DV.! idx
                      in changeLabel labelClassVal idx 
    let indices = [0,1.. (DV.length (homes cityAfterRelocation) - 1)]
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
            let endSim = currentStep state == (maxSteps $ cityCtx state) || moveHappened == False
            if endSim then endSimulation else do 
                mapM updateLabel indices

                -- Update the GUIState with the nxtColor 
                let guiStateWithRelocatedCity = updateCityAndStep state cityAfterRelocation

                -- Return a tuple. The first component is whether this timer should be canceled or not
                -- and second component is the updated GUI state. 
                return (True, guiStateWithRelocatedCity)

        (False, True) -> do 
            let updateWhite = changeLabel.labelClass $ White
            mapM updateWhite indices

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

  win <- new Gtk.Window [ #title  := "Animated Labels Program", 
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
 
  -- Add a global function to handle key presses in application 
  -- You need to make sure you pass in your IORef of the GUIState 
  -- to the key press 
  on win #keyPressEvent (hanldeKeyPress stateRef) 

  -- Add the main application layout to the window 
  #add win layoutWindow 


  -- Show the window 
  #showAll win

  -- Call the main action of the GTK library 
  Gtk.main