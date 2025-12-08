module Main where

import Control.Exception (SomeException, try)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import XMonad qualified
import XMonad.Actions.Navigation2D (Navigation2DConfig (..), centerNavigation, lineNavigation, singleWindowRect, windowGo, windowSwap, withNavigation2DConfig)
import XMonad.Hooks.DynamicLog (xmobarProp)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.StatusBar (StatusBarConfig)
import XMonad.Hooks.StatusBar qualified as StatusBar
import XMonad.Hooks.StatusBar.PP (PP (..))
import XMonad.Hooks.StatusBar.PP qualified as PP
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.Gaps (Gaps, gaps)
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import XMonad.Layout.NoBorders (SetsAmbiguous (..), noBorders)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Renamed (Rename (..), named, renamed)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.SimpleFloat (shrinkText)
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.Spacing (Spacing, spacing)
import XMonad.Layout.SubLayouts (GroupMsg (..), pullGroup, subLayout)
import XMonad.Layout.Tabbed (Direction2D (..), Theme (..), addTabs)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.StackSet qualified as W
import XMonad.Util.Dmenu (menuMapArgs)
import XMonad.Util.EZConfig (mkKeymap)

--------------------------------------------------------------------------------
-- Theme

background :: String
background = "#2d2d2d"

altBackground :: String
altBackground = "#333333"

foreground :: String
foreground = "#cccccc"

comment :: String
comment = "#999999"

red :: String
red = "#f2777a"

orange :: String
orange = "#f99157"

blue :: String
blue = "#6699cc"

myFont :: String
myFont = "xft:Meslo LG M:style=Regular:size=10"

myNormalBorderColor :: String
myNormalBorderColor = blue

myFocusedBorderColor :: String
myFocusedBorderColor = red

myTabTheme :: Theme
myTabTheme =
  XMonad.def
    { fontName = myFont,
      activeColor = altBackground,
      inactiveColor = background,
      activeBorderColor = altBackground,
      inactiveBorderColor = background,
      activeTextColor = foreground,
      inactiveTextColor = comment
    }

--------------------------------------------------------------------------------
-- Layouts

gap :: Int
gap = 4

myBorder :: XMonad.Dimension
myBorder = 1

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacing gap

myGaps :: l a -> ModifiedLayout Gaps l a
myGaps = gaps [(U, gap), (D, gap), (L, gap), (R, gap)]

trimNamed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimNamed w n = renamed [CutWordsLeft w, PrependWords n]

suffixed :: String -> l a -> ModifiedLayout Rename l a
suffixed n = renamed [AppendWords n]

data TABBED = TABBED
  deriving (Show, Read, Eq, XMonad.Typeable)

myLayoutHook = avoidStruts $ mirrorToggle $ fullScreenToggle $ flex XMonad.||| tabs

data FocusedOnly = FocusedOnly
  deriving (Show, Read)

instance SetsAmbiguous FocusedOnly where
  hiddens :: FocusedOnly -> XMonad.WindowSet -> XMonad.Rectangle -> Maybe (W.Stack XMonad.Window) -> [(XMonad.Window, XMonad.Rectangle)] -> [XMonad.Window]
  hiddens _ wset _lr _mst wrs =
    case W.peek wset of
      Nothing -> fmap fst wrs
      Just focused -> filter (/= focused) $ fmap fst wrs

fullScreenToggle = mkToggle $ single FULL

mirrorToggle = mkToggle $ single MIRROR

tabs = named "Tabs" . noBorders $ addTabs shrinkText myTabTheme Simplest

flex =
  trimNamed 5 "Flex"
    . noBorders
    . windowNavigation
    . addTabs shrinkText myTabTheme
    . subLayout [] Simplest
    $ ifWider 1920 wideLayouts standardLayout
  where
    wideThreeCol = suffixed "Wide 3Col" (ThreeColMid 1 (1 / 20) (1 / 2))
    wideLayouts = mySpacing . myGaps $ wideThreeCol XMonad.||| standardLayout
    standardLayout =
      mySpacing . myGaps . named "Std 2/3" $
        ResizableTall 1 (1 / 20) (2 / 3) []

myTerminal = "st"

myWorkspaces = map (show @Int) [1 .. 9]

myManageHook =
  XMonad.composeAll
    [ XMonad.className XMonad.=? "Firefox" XMonad.--> XMonad.doShift "2:web",
      XMonad.className XMonad.=? "Slack" XMonad.--> XMonad.doShift "3:slack",
      XMonad.className XMonad.=? "trayer" XMonad.--> XMonad.doIgnore,
      manageDocks
    ]

--------------------------------------------------------------------------------
-- Keybindings

-- | Keybinding with description: (key, description, action)
type KeyBinding = (String, String, XMonad.X ())

workSpaceNav :: XMonad.XConfig a -> [KeyBinding]
workSpaceNav c = do
  (i, j) <- zip (map (show @Int) [1 .. 9]) $ XMonad.workspaces c
  (m, f, desc) <- [("M-", W.greedyView, "View workspace "), ("M-S-", W.shift, "Move to workspace ")]
  return (m <> i, desc <> i, XMonad.windows $ f j)

showKeyBindings :: [KeyBinding] -> XMonad.X ()
showKeyBindings bindings = do
  selection <- menuMapArgs "dmenu" ["-l", "20"] bindingMap
  fromMaybe (pure ()) selection
  where
    bindingMap = M.fromList [(key <> "  " <> desc, action) | (key, desc, action) <- bindings]

myKeyBindings :: XMonad.XConfig a -> [KeyBinding]
myKeyBindings c =
  -- System
  [ ("M-<Space> q", "Exit prompt", exitPrompt),
    ("M-<Space> b", "Battery status", XMonad.spawn "dmenu-acpi.sh"),
    ("M-/", "Show keybindings", showKeyBindings $ myKeyBindings c),
    ("M-w", "Next layout", nextLayout),
    ("M-e", "Emoji prompt", emojiPrompt),
    ("M-<Backspace>", "Kill focused window", killWindow),
    ("M-S-<Backspace>", "Kill unfocused window", XMonad.withUnfocused XMonad.killWindow),
    ("<XF86AudioMute>", "Toggle mute", toggleMute),
    ("<XF86AudioRaiseVolume>", "Volume up", volumeUp),
    ("<XF86AudioLowerVolume>", "Volume down", volumeDown),
    ("<XF86MonBrightnessUp>", "Brightness up", XMonad.spawn "brightnessctl set 5%+"),
    ("<XF86MonBrightnessDown>", "Brightness down", XMonad.spawn "brightnessctl set 5%-"),
    ("M-<XF86AudioMute>", "Toggle dunst and mute", toggleDunst >> toggleMute),
    ("<Print>", "Take screenshot", takeScreenshot),
    ("C-<Space>", "Close notification", dunstClose),
    ("C-S-<Space>", "Close all notifications", dunstCloseAll)
  ]
    <>
    -- Navigation
    [ ("M-j", "Go down", windowGo D False),
      ("M-k", "Go up", windowGo U False),
      ("M-h", "Go left", windowGo L False),
      ("M-l", "Go right", windowGo R False),
      ("M-;", "Focus previous tab", XMonad.windows W.focusUp),
      ("M-'", "Focus next tab", XMonad.windows W.focusDown)
    ]
    <>
    -- Window Management
    [ ("M-S-;", "Swap with previous", XMonad.windows W.swapUp),
      ("M-S-'", "Swap with next", XMonad.windows W.swapDown),
      ("M-S-j", "Swap down", windowSwap D False),
      ("M-S-k", "Swap up", windowSwap U False),
      ("M-S-h", "Swap left", windowSwap L False),
      ("M-S-l", "Swap right", windowSwap R False),
      ("M-[", "Shrink window", XMonad.sendMessage XMonad.Shrink),
      ("M-]", "Expand window", XMonad.sendMessage XMonad.Expand),
      ("M-r", "Toggle mirror", XMonad.sendMessage $ Toggle MIRROR),
      ("M-<F11>", "Toggle fullscreen", XMonad.sendMessage $ Toggle FULL)
    ]
    <>
    -- Sublayouts
    [ ("M-C-h", "Merge left", XMonad.sendMessage . pullGroup $ L),
      ("M-C-l", "Merge right", XMonad.sendMessage . pullGroup $ R),
      ("M-C-j", "Merge down", XMonad.sendMessage . pullGroup $ D),
      ("M-C-k", "Merge up", XMonad.sendMessage . pullGroup $ U),
      ("M-g", "Unmerge window", XMonad.withFocused (XMonad.sendMessage . UnMerge))
    ]
    <> workSpaceNav c
    <>
    -- Launchers
    [ ("M-<Return>", "Open terminal", XMonad.spawn myTerminal),
      ("M-p", "Open launcher", myLauncher),
      ("M-t", "Open bookmark", openBookmark)
    ]
  where
    nextLayout = XMonad.sendMessage XMonad.NextLayout
    toggleDunst = XMonad.spawn "dunstctl set-paused toggle"
    toggleMute = XMonad.spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    dunstClose = XMonad.spawn "dunstctl close"
    dunstCloseAll = XMonad.spawn "dunstctl close-all"
    volumeUp = XMonad.spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    volumeDown = XMonad.spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    openBookmark = XMonad.spawn ".local/share/bookmarks"
    takeScreenshot = XMonad.spawn "screenshot-prompt"
    emojiPrompt = XMonad.spawn "emoji-prompt"
    killWindow = XMonad.spawn "kill-window-prompt"
    exitPrompt = XMonad.spawn "exit-prompt"
    myLauncher = XMonad.spawn "dmenu_run"

myKeys :: XMonad.XConfig a -> M.Map (XMonad.KeyMask, XMonad.KeySym) (XMonad.X ())
myKeys c = mkKeymap c [(key, action) | (key, _, action) <- myKeyBindings c]

myNav2DConf :: Navigation2DConfig
myNav2DConf =
  XMonad.def
    { defaultTiledNavigation = centerNavigation,
      floatNavigation = centerNavigation,
      screenNavigation = lineNavigation,
      layoutNavigation = pure ("Full", centerNavigation),
      unmappedWindowRect = pure ("Full", singleWindowRect)
    }

--------------------------------------------------------------------------------
-- Status Bar

statusBarConfig :: StatusBarConfig
statusBarConfig =
  StatusBar.statusBarProp "xmobar-solomon" $
    pure $
      PP.def
        { ppCurrent = (<> "=visible"),
          ppLayout = id,
          ppTitle = id,
          ppHidden = \ws -> if ws == "NSP" then mempty else ws <> "=hidden",
          ppHiddenNoWindows = (<> "=empty"),
          ppWsSep = ",",
          ppSep = "|"
        }

--------------------------------------------------------------------------------
-- Main

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path =
  fmap (either (const Nothing) Just) $ try @SomeException $ readFile path

myStartupHook :: XMonad.X ()
myStartupHook = do
  commands <- XMonad.liftIO $ readFileMaybe "/home/solomon/.config/startup.sh"
  traverse_ (traverse_ XMonad.spawn . lines) commands

myConfig =
  StatusBar.withSB statusBarConfig $
    XMonad.def
      { XMonad.layoutHook = myLayoutHook,
        XMonad.manageHook = myManageHook <> XMonad.manageHook XMonad.def,
        XMonad.modMask = XMonad.mod4Mask,
        XMonad.keys = myKeys,
        XMonad.workspaces = myWorkspaces,
        XMonad.normalBorderColor = myNormalBorderColor,
        XMonad.focusedBorderColor = myFocusedBorderColor,
        XMonad.startupHook = myStartupHook,
        XMonad.borderWidth = myBorder
      }

main :: IO ()
main =
  XMonad.xmonad . ewmhFullscreen . ewmh . xmobarProp . pagerHints . docks $ withNavigation2DConfig myNav2DConf myConfig
