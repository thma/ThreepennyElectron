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

import           Calc
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
    return win # set title "3PennyCalc"
    UI.addStyleSheet win "semantic.css"
    out <- UI.input # set (attr "readonly") "true" 
                    # set (attr "style") "text-align: right; min-width: 320px" 
                    # set value "0"
    buttons <- mapM (mapM mkButton) buttonLabels

    UI.getBody win #+
      [ UI.div #. "ui raised very padded text container segment" #+
        [ UI.div #. "ui input focus" #+ [element out]
        , UI.table #+
            map (UI.row . map element) buttons
        ]
      ]
    let clicks = buttonClicks (zip (concat buttons) (concat buttonLabels))
        commands = fmap populate clicks
    calcBehaviour <- accumB (def :: State) commands
    let outText = fmap display calcBehaviour
    element out # sink value outText
  where
    mkButton s =
      UI.button #. "ui button" # set text s # set value s # set (attr "type") "button" #
      set (attr "style") "min-width: 60px"
    buttonClicks :: [(Element, String)] -> Event String
    buttonClicks = foldr1 (UI.unionWith const) . map makeClick
      where
        makeClick (e, s) = UI.pure s <@ UI.click e
    buttonLabels :: [[String]]
    buttonLabels = map words $ lines "7 8 9 CE C\n4 5 6 + -\n1 2 3 * /\n . 0 ="


-- | convenience function that opens the 3penny UI in the default web browser
launchSiteInBrowser:: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
launchSiteInBrowser =
    case os of
    "mingw32" -> createProcess  (shell $ "start " ++ url)
    "darwin"  -> createProcess  (shell $ "open " ++ url)
    _         -> createProcess  (shell $ "xdg-open " ++ url)
    where
    url = "http://localhost:8023"
