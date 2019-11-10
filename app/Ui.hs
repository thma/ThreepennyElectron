module Ui where

import           GHC.IO.Handle.Types         (Handle)
import           System.Info                 (os)
import           System.Process              (ProcessHandle, createProcess, shell)

import           Control.Monad               (void)
import           Graphics.UI.Threepenny      hiding (map, start, Color, color )
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core (defaultConfig, startGUI)

import           Calc                        (Command(..), Digit(..), Operation(..), processCommand,
                                             toString, initialState, lbl)
import           Data.Char                   (toLower)

-- | main entry point from main.js launch script
start :: Int -> IO ()
start port = startGUI defaultConfig
    { jsPort = Just port
    , jsStatic = Just "static"
    } setup

-- | setup window layout and event handling
setup :: Window -> UI ()
setup win = void $ do
  -- define page
  return win # set title "3PennyCalc"
  UI.addStyleSheet win "semantic.min.css"
  
  -- define UI controls
  outputBox <- UI.input
                # set (attr "readonly") "true"
                # set (attr "style") "text-align: right; min-width: 324px"
  -- define the button grid
  buttons   <- mapM (mapM mkButton) buttonDefinitions

  -- define page DOM with 3penny html combinators
  UI.getBody win # set (attr "style") "overflow: hidden" #+
    [ UI.div #. "ui raised very padded text container segment" #+
      [UI.table #+ [UI.row [UI.div #. "ui input" #+ [element outputBox]]] #+ 
                    map (UI.row . map element) buttons]
    ]

  let  
      -- keys :: Event String
      -- keys = fmap (UI.keypress outputBox) (: [])
      -- map buttons to Command. (buttonMap :: [(Element, Command)] )
      buttonMap = zip (concat buttons) (concatMap (map fst) buttonDefinitions)
      -- register mouse click events to all buttons. (clicks :: Event Command )
      clicks  = buttonClicks buttonMap
      -- use (processCommand :: Command -> State -> State) to build a command that computes a
      -- calculator state transition (commands :: Event (State -> State))
      commands  = fmap processCommand clicks

  -- calculate behaviour by accumulating all commands, starting with the initial state    
  calcBehaviour <- accumB initialState commands
  -- use Calc.toString to extract the display string from the calculator state
  let outText  = fmap toString calcBehaviour
  -- write outText to the outputBox UI element
  element outputBox # sink value outText
  where
    mkButton :: (Command, Color) -> UI Element
    mkButton (cmd, clr) =
      let btnLabel = lbl cmd -- get the button text
      in  UI.button #. ("ui " ++ color clr ++ " button")
                    # set text btnLabel # set value btnLabel
                    # set (attr "type")  "button"
                    # set (attr "style") "min-width: 60px"

    color :: Color -> String
    color = map toLower . show

    buttonDefinitions :: [[(Command, Color)]]
    buttonDefinitions =
      [ [(Digit Seven, Grey), (Digit Eight, Grey), (Digit Nine,  Grey), (ClearError,   Orange), (Clear,        Orange)]
      , [(Digit Four,  Grey), (Digit Five,  Grey), (Digit Six,   Grey), (Operation Add, Brown), (Operation Sub, Brown)]
      , [(Digit One,   Grey), (Digit Two,   Grey), (Digit Three, Grey), (Operation Mul, Brown), (Operation Div, Brown)]
      , [(Dot,  Grey),        (Digit Zero,  Grey), (Flush, Black)] ]

    buttonClicks :: [(Element, Command)] -> Event Command
    buttonClicks = foldr1 (UI.unionWith const) . map makeClick
      where
        makeClick (elmnt, cmd) = UI.pure cmd <@ UI.click elmnt


-- | Button colors
data Color = Grey | Orange | Brown | Black deriving (Show)

-- | launch application in default web browser
up :: IO ()
up = do
  let port = 8023
  launchAppInBrowser port
  start port

-- | convenience function that opens the 3penny UI in the default web browser
launchAppInBrowser:: Int -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchAppInBrowser port = case os of
  "mingw32" -> createProcess  (shell $ "start "    ++ url)
  "darwin"  -> createProcess  (shell $ "open "     ++ url)
  _         -> createProcess  (shell $ "xdg-open " ++ url)
  where url = "http://localhost:" ++ show port

