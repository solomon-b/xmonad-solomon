module Main where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, isSpace)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (isInfixOf)
import Data.Map qualified as M
import Data.Monoid (All (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitSuccess)
import XMonad qualified
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.Navigation2D (Navigation2DConfig (..), centerNavigation, lineNavigation, singleWindowRect, windowGo, windowSwap, withNavigation2DConfig)
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
import XMonad.Prompt (XPConfig (..), XPPosition (..), emacsLikeXPKeymap)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPromptCT)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Hooks.DynamicLog (xmobarProp)

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
    --  $ standardLayouts
    $ ifWider 1920 wideLayouts standardLayout
  where
    wideThreeCol = suffixed "Wide 3Col" (ThreeColMid 1 (1 / 20) (1 / 2))
    wideLayouts = mySpacing . myGaps $ wideThreeCol XMonad.||| standardLayout
    standardLayout =
      mySpacing . myGaps . named "Std 2/3" $
        ResizableTall 1 (1 / 20) (2 / 3) [] --  ||| Mirror (ResizableTall 1 (1/20) (2/3) [])

myBrowser = "/usr/bin/firefox"

myTerminal = "st"

myLauncher = XMonad.Prompt.Shell.shellPrompt promptConfig

myWorkspaces = map (show @Int) [1 .. 9]

myManageHook =
  XMonad.composeAll
    [ XMonad.className XMonad.=? "Firefox" XMonad.--> XMonad.doShift "2:web",
      XMonad.className XMonad.=? "Slack" XMonad.--> XMonad.doShift "3:slack",
      XMonad.className XMonad.=? "trayer" XMonad.--> XMonad.doIgnore,
      manageDocks
    ]

--------------------------------------------------------------------------------
-- Prompts

promptConfig :: XPConfig
promptConfig =
  XMonad.def
    { position = Top,
      height = 20,
      font = myFont,
      bgColor = background,
      fgColor = orange,
      fgHLight = "#d33682",
      bgHLight = "#073642",
      promptBorderWidth = 0,
      maxComplRows = Just 12,
      alwaysHighlight = True,
      promptKeymap = emacsLikeXPKeymap,
      searchPredicate = isInfixOf `on` map toLower
    }

-- | X Session logout and system shutdown/reboot prompt
exitPrompt :: XMonad.X ()
exitPrompt = xmonadPromptCT "Exit" commands promptConfig
  where
    commands =
      [ ("1: Logout", XMonad.io exitSuccess),
        ("2: Shutdown", XMonad.spawn "systemctl poweroff"),
        ("3: Reboot", XMonad.spawn "systemctl reboot"),
        ("4: Reload XMonad", restart)
      ]
    restart = do
      XMonad.spawn "pkill trayer"
      XMonad.spawn "xmonad --restart"

-- | Kill the focused window
closeWindowPrompt :: XMonad.X ()
closeWindowPrompt = confirmPrompt promptConfig "Close Window" kill1

-- | Screenshot prompt
scrotPrompt :: XMonad.X ()
scrotPrompt = xmonadPromptCT "Screenshot Options" commands promptConfig
  where
    commands =
      [ ("1: Capture Screen", XMonad.spawn "scrot -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png"),
        ("2: Capture Selection", XMonad.spawn "scrot -s -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png"),
        ("3: Capture All Screens", XMonad.spawn "scrot -m -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png"),
        ("4: Capture with 3 second countdown", XMonad.spawn "scrot -d 3 -c -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png")
      ]

layoutPrompt :: XMonad.X ()
layoutPrompt = xmonadPromptCT "Window Commands" commands promptConfig
  where
    commands =
      [ ("1: Tabbed Layout", XMonad.sendMessage (XMonad.JumpToLayout "Tabs")),
        ("2: Two Column Layout", XMonad.sendMessage (XMonad.JumpToLayout "Flex"))
      ]

emojiPrompt :: XMonad.X ()
emojiPrompt = do
  emojis <- liftIO $ Text.lines <$> Text.IO.readFile "/home/solomon/.local/share/emoji"
  let action e = XMonad.spawn $ "echo " <> e <> " | xclip -sel clip"
      promptData = fmap (\e -> (Text.unpack e, action $ Text.unpack $ Text.takeWhile (not . isSpace) e)) emojis
  xmonadPromptCT "Emojis" promptData promptConfig

bookmarkPrompt :: XMonad.X ()
bookmarkPrompt = do
  bookmarks <- liftIO $ Text.lines <$> Text.IO.readFile "/home/solomon/.local/share/bookmarks"
  let action e = XMonad.spawn $ "xdg-open " <> e
      promptData = fmap (\e -> (Text.unpack e, action $ Text.unpack e)) bookmarks
  xmonadPromptCT "Emojis" promptData promptConfig

--------------------------------------------------------------------------------
-- Keybindings

workSpaceNav :: XMonad.XConfig a -> [(String, XMonad.X ())]
workSpaceNav c = do
  (i, j) <- zip (map (show @Int) [1 .. 9]) $ XMonad.workspaces c
  (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  return (m ++ i, XMonad.windows $ f j)

myKeys :: XMonad.XConfig a -> M.Map (XMonad.KeyMask, XMonad.KeySym) (XMonad.X ())
myKeys c =
  mkKeymap c $
    -- System
    [ ("M-<Space> q", exitPrompt),
      ("M-<Space> w", layoutPrompt),
      ("M-<Space> <Space>", myLauncher),
      ("M-<Space> e", emojiPrompt),
      ("M-<Backspace>", closeWindowPrompt),
      ("M-S-<Backspace>", XMonad.withUnfocused XMonad.killWindow),
      ("<XF86AudioMute>", toggleMute),
      ("<XF86AudioRaiseVolume>", volumeUp),
      ("<XF86AudioLowerVolume>", volumeDown),
      ("<XF86MonBrightnessUp>", XMonad.spawn "brightnessctl set 5%+"),
      ("<XF86MonBrightnessDown>", XMonad.spawn "brightnessctl set 5%-"),
      ("M-<XF86AudioMute>", toggleDunst >> toggleMute),
      ("<Print>", scrotPrompt),
      ("C-<Space>", dunstClose),
      ("C-S-<Space>", dunstCloseAll)
    ]
      <>
      -- Navigate between windows
      [ ("M-j", windowGo D False),
        ("M-k", windowGo U False),
        ("M-h", windowGo L False),
        ("M-l", windowGo R False),
        -- Navigate between tabs
        ("M-;", XMonad.windows W.focusUp),
        ("M-'", XMonad.windows W.focusDown),
        -- Shift tabs
        ("M-S-;", XMonad.windows W.swapUp),
        ("M-S-'", XMonad.windows W.swapDown),
        -- Swap adjacent windows
        ("M-S-j", windowSwap D False),
        ("M-S-k", windowSwap U False),
        ("M-S-h", windowSwap L False),
        ("M-S-l", windowSwap R False),
        -- Shrink/Expand windows
        ("M-[", XMonad.sendMessage XMonad.Shrink),
        ("M-]", XMonad.sendMessage XMonad.Expand),
        ("M-r", XMonad.sendMessage $ Toggle MIRROR),
        -- Float/Sink floated window
        ("M-t", floatFocused),
        ("M-C-t", XMonad.withFocused toggleSticky),
        -- Full Screen a window
        ("M-<F11>", XMonad.sendMessage $ Toggle FULL),
        -- "merge with sublayout"
        ("M-C-h", XMonad.sendMessage . pullGroup $ L),
        ("M-C-l", XMonad.sendMessage . pullGroup $ R),
        ("M-C-j", XMonad.sendMessage . pullGroup $ D),
        ("M-C-k", XMonad.sendMessage . pullGroup $ U),
        -- Unmerge a window
        ("M-g", XMonad.withFocused (XMonad.sendMessage . UnMerge))
      ]
      <> workSpaceNav c
      <> [ ("M-<Return>", XMonad.spawn myTerminal),
           ("M-\\", XMonad.spawn myBrowser),
           ("M-p", myLauncher),
           ("M-b", bookmarkPrompt)
         ]
  where
    toggleDunst = XMonad.spawn "dunstctl set-paused toggle"
    toggleMute = XMonad.spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    dunstClose = XMonad.spawn "dunstctl close"
    dunstCloseAll = XMonad.spawn "dunstctl close-all"
    volumeUp = XMonad.spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    volumeDown = XMonad.spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"

toggleSticky :: XMonad.Window -> XMonad.X ()
toggleSticky w = XMonad.windows $ \s ->
  if M.member w (W.floating s)
    then copyToAll s
    else s

toggleFloat :: XMonad.Window -> XMonad.X ()
toggleFloat w = XMonad.windows $ \s ->
  if M.member w (W.floating s)
    then W.sink w s
    else W.float w (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)) s

floatFocused :: XMonad.X ()
floatFocused = do
  XMonad.withFocused toggleFloat
  killAllOtherCopies

myNav2DConf :: Navigation2DConfig
myNav2DConf =
  XMonad.def
    { defaultTiledNavigation = centerNavigation,
      floatNavigation = centerNavigation,
      screenNavigation = lineNavigation,
      layoutNavigation = pure ("Full", centerNavigation),
      unmappedWindowRect = pure ("Full", singleWindowRect)
    }

myMouseBindings :: XMonad.XConfig l -> M.Map (XMonad.KeyMask, XMonad.Button) (XMonad.Window -> XMonad.X ())
myMouseBindings XMonad.XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, XMonad.button1), \w -> XMonad.focus w >> XMonad.mouseMoveWindow w >> XMonad.windows W.shiftMaster), -- Set window to float and move by dragging
      ((modm XMonad..|. XMonad.controlMask, XMonad.button1), \w -> XMonad.focus w >> XMonad.mouseResizeWindow w >> XMonad.windows W.shiftMaster) -- Set window to float and resize by dragging
    ]

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

restartEventHook :: XMonad.Event -> XMonad.X All
restartEventHook = \case
  XMonad.ClientMessageEvent {XMonad.ev_message_type} -> do
    atom <- XMonad.getAtom "XMONAD_RESTART"
    when (ev_message_type == atom) $ XMonad.restart "xmonad-solomon" True
    pure $ All True
  _ -> pure $ All True

myConfig =
  StatusBar.withSB statusBarConfig $
    XMonad.def
      { XMonad.layoutHook = myLayoutHook,
        XMonad.manageHook = myManageHook <> XMonad.manageHook XMonad.def,
        XMonad.handleEventHook = restartEventHook,
        XMonad.modMask = XMonad.mod4Mask,
        XMonad.keys = myKeys,
        XMonad.mouseBindings = myMouseBindings,
        XMonad.workspaces = myWorkspaces,
        XMonad.normalBorderColor = myNormalBorderColor,
        XMonad.focusedBorderColor = myFocusedBorderColor,
        XMonad.startupHook = myStartupHook,
        XMonad.borderWidth = myBorder
      }

main :: IO ()
main =
  XMonad.xmonad . ewmhFullscreen . ewmh . xmobarProp . pagerHints . docks $ withNavigation2DConfig myNav2DConf myConfig
