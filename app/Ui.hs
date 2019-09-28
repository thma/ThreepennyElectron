module Ui where

import           System.Process           (createProcess, shell, ProcessHandle)
import           System.Info              (os)
import           GHC.IO.Handle.Types (Handle)

import           Control.Monad               (void)
import           Data.Default                (def)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny      (set, attr, title, text, value,
                                                (#), (#+), (<@),
                                                Event, Element, UI, Window,
                                                sink, accumB, jsPort, jsCallBufferMode, CallBufferMode(..))
import           Graphics.UI.Threepenny.Core (startGUI, defaultConfig)

import           Calc

up :: IO ()
up = do
    launchSiteInBrowser
    startGUI defaultConfig setup

start :: Int -> IO ()
start port =
  startGUI defaultConfig
    { jsPort = Just port
    , jsCallBufferMode = BufferRun
    } setup

    
setup :: Window -> UI ()
setup win = void $ do
    return win # set title "3PennyCalc"

    out <- UI.input # set (attr "readonly") "true"
                    # set (attr "style") "text-align: right; min-width: 245px"
                    # set value "0"

    buttons <- mapM (mapM mkButton) buttonLabels

    let clicks = buttonClicks (zip (concat buttons) (concat buttonLabels))
        commands = fmap populate clicks
    calcBehaviour <- accumB (def :: State) commands
    return out # sink value (fmap display calcBehaviour)

    UI.getBody win #+ [return out]
                   #+ map (UI.row .map return) buttons

    where
    mkButton s = UI.input # set text s
                            # set value s
                            # set (attr "type") "button"
                            # set (attr "style") "min-width: 50px"

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
