import Control.Monad (void)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Map (Map)
import Data.Time (getCurrentTime)
import Network.URL (URL(..), URLType(..), Host(..), Protocol(..), exportURL)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.WindowNavigation
import qualified DBus.Notify as DBus
import qualified Data.Map as Map

main :: IO ()
main = do
    spawn "xsetroot -solid black"
    dbus <- DBus.connectSession
    dbusNotify dbus "XMonad" "config loaded"
    xmonad $ xConfig dbus

xConfig :: DBus.Client -> XConfig (ModifiedLayout WindowNavigation (Choose Tall (Choose (Mirror Tall) Full)))
xConfig dbus = def
    { terminal           = "termite"
    , layoutHook         = windowNavigation $ layoutHook def
    , manageHook         = windowTypes <+> manageHook def
    , normalBorderColor  = "#663399"
    , focusedBorderColor = "#9966ee"
    , modMask            = mod4Mask
    , borderWidth        = 2
    , keys               = xKeys dbus <+> keys def
    }

windowTypes :: ManageHook
windowTypes = composeAll
    [ title =? "xmessage" --> doFloat
    ]

xKeys :: DBus.Client -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
xKeys dbus conf = Map.fromList
    [ ((modm,               xK_p),   shellPrompt xpConfig)
    , ((modm,               xK_Tab), toggleWS)
    , ((modm,               xK_m),   xmonadPrompt xpConfig)
    , ((modm,               xK_b),   inputPrompt browseConfig "browse" ?+ browse [])
    , ((modm .|. shiftMask, xK_b),   inputPrompt browseIncognitoConfig "incognito" ?+ browse ["--incognito"])
    , ((modm,               xK_t),   notifyTime dbus)
    , ((modm,               xK_h),   sendMessage $ Go L)
    , ((modm,               xK_j),   sendMessage $ Go D)
    , ((modm,               xK_k),   sendMessage $ Go U)
    , ((modm,               xK_l),   sendMessage $ Go R)
    , ((modm .|. shiftMask, xK_h),   sendMessage $ Swap L)
    , ((modm .|. shiftMask, xK_j),   sendMessage $ Swap D)
    , ((modm .|. shiftMask, xK_k),   sendMessage $ Swap U)
    , ((modm .|. shiftMask, xK_l),   sendMessage $ Swap R)
    , ((modm,               xK_semicolon),   xmonadPrompt xpConfig)
    ]
    where
    modm = modMask conf

notifyTime :: DBus.Client -> X ()
notifyTime dbus = liftIO $ getCurrentTime >>= dbusNotify dbus "time" . show

dbusNotify :: (Monad m, MonadIO m) => DBus.Client -> String -> String -> m ()
dbusNotify dbus summary body = liftIO . void $ DBus.notify dbus DBus.blankNote
    { DBus.summary = summary ++ ":"
    , DBus.body    = Just $ DBus.Text body
    }

browser :: String
browser = "firefox"

browse :: [String] -> String -> X ()
browse flags s = spawn . intercalate " " $ browser:flags ++ [url]
    where
    url | isURL s   = s
        | otherwise = exportURL $ URL ddg "" [("q",s)]
    ddg = Absolute $ Host (HTTP True) "duckduckgo.com" Nothing

isURL :: String -> Bool
isURL s = "!" `isntPrefixOf` s && ("://" `isInfixOf` s || (any (=='.') s && all (/=' ') s))
  where
  isntPrefixOf s = not . isPrefixOf s

xpConfig :: XPConfig
xpConfig = def
    { font              = "Terminus"
    , borderColor       = "#663399"
    , fgColor           = "#9966ee"
    , bgColor           = "#000000"
    , fgHLight          = "#339966"
    , bgHLight          = "#000000"
    , promptBorderWidth = 2
    , position          = CenteredAt 0.3 0.5
    }

browseConfig :: XPConfig
browseConfig = xpConfig
    { historySize   = 100
    , historyFilter = filter isURL
    }

browseIncognitoConfig :: XPConfig
browseIncognitoConfig = xpConfig
    { historySize = 0
    }
