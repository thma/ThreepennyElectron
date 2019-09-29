module Ui where

import           GHC.IO.Handle.Types         (Handle)
import           System.Info                 (os)
import           System.Process              (ProcessHandle, createProcess,
                                              shell)

import           Control.Monad               (void)
import           Data.Default                (def)
import           Graphics.UI.Threepenny      hiding (map, start)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core (defaultConfig, startGUI)

import           Calc                        (State, populate, display, initialState )
import           Paths                       (getStaticDir)

-- | main entry point from electron.js launch script
start :: Int -> IO ()
start port = do
  staticDir <- getStaticDir
  startGUI defaultConfig
    { jsPort = Just port
    , jsStatic = Just staticDir
    , jsCallBufferMode = BufferRun
    } setup

-- | launch site automatically in default web browser
up :: IO ()
up = do
    launchSiteInBrowser
    start 8023

-- | setup window layout
setup :: Window -> UI ()
setup win =
  void $ do
    -- define page
    return win # set title "3PennyCalc"
    UI.addStyleSheet win "semantic.css"
    -- define UI controls
    outputBox <- UI.input # set (attr "readonly") "true" 
                    # set (attr "style") "text-align: right; min-width: 320px" 
                    # set value "0"
    buttons   <- mapM (mapM mkButton) buttonLabels

    -- define page DOM with 3penny html combinators
    UI.getBody win # set (attr "style") "overflow: hidden" #+
       [ UI.div #. "ui raised very padded text container segment" #+
          [ UI.div #. "ui input focus" #+ [element outputBox]
          , UI.table #+
              map (UI.row . map element) buttons
          ]
       ]
       
    -- define event handling for button clicks
    let clicks   = buttonClicks (zip (concat buttons) (concat buttonLabels))
        commands = fmap populate clicks
        state    = initialState --def :: State
    calcBehaviour <- accumB state commands
    let outText  = fmap display calcBehaviour
    element outputBox # sink value outText
  where
    mkButton s =
      UI.button #. "ui button" # set text s # set value s # set (attr "type") "button" #
      set (attr "style") "min-width: 60px"
    buttonClicks :: [(Element, String)] -> Event String
    buttonClicks = foldr1 (UI.unionWith const) . map makeClick
      where makeClick (e, s) = UI.pure s <@ UI.click e
    buttonLabels :: [[String]]
    buttonLabels = 
      [
        ["7", "8", "9", "CE", "C"]
      , ["4", "5", "6", "+",  "-"]
      , ["1", "2", "3", "*",  "/"]
      , [".", "0", "="] -- , "pi"]
      ]


-- | convenience function that opens the 3penny UI in the default web browser
launchSiteInBrowser:: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchSiteInBrowser =
    case os of
    "mingw32" -> createProcess  (shell $ "start " ++ url)
    "darwin"  -> createProcess  (shell $ "open " ++ url)
    _         -> createProcess  (shell $ "xdg-open " ++ url)
    where
    url = "http://localhost:8023"
