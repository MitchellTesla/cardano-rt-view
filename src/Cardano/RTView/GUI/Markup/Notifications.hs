{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.RTView.GUI.Markup.Notifications
    ( mkNotifications
    ) where

import           Control.Monad (forM_, void)
import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, liftIO, set, string, (#), (#+))

import           Cardano.BM.Configuration (Configuration)

import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.GUI.Elements (HTMLClass (..), (#.), hideIt, showIt)

mkNotifications
  :: Configuration
  -> RTViewParams
  -> Element
  -> UI Element
mkNotifications _config _params notificationsButton = do
  closeButton <- UI.img #. [W3DisplayTopright, RTViewInfoClose]
                        # set UI.src "/static/images/times.svg"
                        # set UI.title__ "Save changes and close"

  (mainSwitch, mainSwitchSpan, mainSwitchRoot)
    <- mkSwitch True "Click to disable notifications"
  mainSwitchLabel <- string "Enabled"

  eventsTab <- UI.button #. [W3BarItem, W3Button, W3Mobile]
                         # makeItActive
                         # set UI.text "Events to notify"
  howToTab  <- UI.button #. [W3BarItem, W3Button, W3Mobile]
                         # set UI.text "How to notify"

  eventsTabContent <- mkEventsTabContent
  howToTabContent  <- mkHowToTabContent # hideIt

  let tabs :: [((Element, Element), Int)]
      tabs = let allTabs = [ (eventsTab, eventsTabContent)
                           , (howToTab,  howToTabContent)
                           ]
             in zip allTabs [1..length allTabs]

  registerClicksOnTabs tabs

  settings
    <- UI.div #. [W3Container] #+
         [ UI.div #. [W3Bar, W3BorderBottom, NotificationsBar] #+
             [ element eventsTab
             , element howToTab
             ]
         , UI.div #+
             [ element eventsTabContent
             , element howToTabContent
             ]
         ]

  notifications <-
    UI.div #. [W3Modal] #+
      [ UI.div #. [W3ModalContent, W3AnimateTop, W3Card4] #+
          [ UI.div #. [W3Container, RTViewInfoTop] #+
              [ element closeButton
              , UI.h2 #+ [ string "Notifications" ]
              ]
          , UI.div #. [W3Container, RTViewInfoContainer] #+
              [ UI.div #. [W3Row, NotificationsMainSwitch] #+
                  [ UI.div #. [W3Col, SwitchContainer] #+
                      [ element mainSwitchRoot
                      ]
                  , UI.div #. [W3Rest] #+
                      [ element mainSwitchLabel ]
                  ]
              ]
          , element settings
          ]
      ]

  void $ UI.onEvent (UI.click closeButton) $ \_ ->
    element notifications # hideIt

  void $ UI.onEvent (UI.checkedChange mainSwitch) $ \isChecked -> do
    let (label, label', bell, iconClass, action) =
          if isChecked
            then ("disable", "Enabled",  "bell",       NotificationsIcon,      showIt)
            else ("enable",  "Disabled", "bell-slash", NotificationsIconSlash, hideIt)
    void $ element mainSwitchLabel # set UI.text label'
    void $ element mainSwitchSpan # set UI.title__ ("Click to " <> label <> " notifications")
    void $ element notificationsButton #. [iconClass]
                                       # set UI.src ("/static/images/" <> bell <> ".svg")
    void $ element settings # action

  return notifications

registerClicksOnTabs
  :: [((Element, Element), Int)]
  -> UI ()
registerClicksOnTabs tabs =
  forM_ tabs $ \((tab, _), tabNum) ->
    void $ UI.onEvent (UI.click tab) $ \_ -> showTabAndMakeItActive tabNum
 where
  showTabAndMakeItActive num =
    forM_ tabs $ \((tab', tabContent), tabNum') ->
      if num == tabNum'
        then do
          void $ element tabContent # showIt
          void $ element tab' # makeItActive
        else do
          void $ element tabContent # hideIt
          void $ element tab' # makeItInactive

makeItActive, makeItInactive :: UI Element -> UI Element
makeItActive el   = el #. [W3BarItem, W3Button, W3Mobile, ActiveTab]
makeItInactive el = el #. [W3BarItem, W3Button, W3Mobile]

mkSwitch
  :: Bool
  -> String
  -> UI (Element, Element, Element)
mkSwitch isChecked aTitle = do
  switch
    <- UI.input # set UI.type_ "checkbox" # set UI.checked isChecked
  switchSpan
    <- UI.span #. [Slider, Round] # set UI.title__ aTitle
  switchRoot
    <- UI.label #. [Switch] #+
         [ element switch
         , element switchSpan
         ]
  return (switch, switchSpan, switchRoot)

-- | Here we describe events the user will be notified on.
mkEventsTabContent
  :: UI Element
mkEventsTabContent = do
  warnings    <- switchSection "Warnings"    "warnings"    fake
  errors      <- switchSection "Errors"      "errors"      fake
  criticals   <- switchSection "Criticals"   "criticals"   fake
  alerts      <- switchSection "Alerts"      "alerts"      fake
  emergencies <- switchSection "Emergencies" "emergencies" fake

  missedSlots <- switchSection "Missed slots"      "missed slots"           fake
  cannotLead  <- switchSection "Node cannot forge" "node that cannot forge" fake

  nodeIsIdle       <- switchSection "Node is idle"               "idle nodes"           fake
  wrongNodeVersion <- switchSection "Unsupported nodes versions" "wrong nodes versions" fake

  UI.div #. [NotificationsTabContainer] #+
    [ UI.div #. [NotificationsEventsHeader] #+
        [ string "Node errors" ]
    , UI.div #. [NotificationsSwitches] #+
        [ element warnings
        , element errors
        , element criticals
        , element alerts
        , element emergencies
        ]
    , UI.div #. [NodeMetricsVSpacer]
    , UI.div #. [NotificationsEventsHeader] #+
        [ string "Blockchain errors" ]
    , UI.div #. [NotificationsSwitches] #+
        [ element missedSlots
        , element cannotLead
        ]
    , UI.div #. [NodeMetricsVSpacer]
    , UI.div #. [NotificationsEventsHeader] #+
        [ string "Common problems" ]
    , UI.div #. [NotificationsSwitches] #+
        [ element nodeIsIdle
        , element wrongNodeVersion
        ]
    ]
 where
  switchSection aLabel titleSpec checkedHandler = do
    (switch, _, switchRoot)
      <- mkSwitch True ("Click to enable/disable notifications on " <> titleSpec)

    void $ UI.onEvent (UI.checkedChange switch) $ \isChecked ->
      checkedHandler isChecked

    UI.div #. [W3Row, NotificationsSwitch] #+
      [ UI.div #. [W3Col, SwitchContainer] #+
          [ element switchRoot
          ]
      , UI.div #. [W3Rest] #+
          [ string aLabel ]
      ]

  fake :: Bool -> UI ()
  fake _ = return ()

-- | Here we describe how the user will be notified about events.
mkHowToTabContent
  :: UI Element
mkHowToTabContent = do
  UI.div #. [NotificationsTabContainer] #+
    [ UI.div #. [NotificationsEventsHeader] #+
        [ string "Email settings"
        , infoMark "Current release supports email notifications only"
        ]
    , UI.form #+
        [ UI.div #. [W3RowPadding] #+
            [ UI.div #. [W3Half] #+
                [ UI.label # set UI.text "Server host"
                , string "*" #. [RequiredInput]
                , UI.input #. [W3Input, NotificationsInput] # set UI.type_ "url"
                ]
            , UI.div #. [W3Half] #+
                [ UI.label # set UI.text "Server port"
                , string "*" #. [RequiredInput]
                , UI.input #. [W3Input, NotificationsInput] # set UI.type_ "number"
                ]
            ]
        , UI.div #. [NotificationsVSpacer]
        , UI.div #. [W3RowPadding] #+
            [ UI.div #. [W3Third] #+
                [ UI.label # set UI.text "Username"
                , string "*" #. [RequiredInput]
                , UI.input #. [W3Input, NotificationsInput] # set UI.type_ "text"
                ]
            , UI.div #. [W3Third] #+
                [ UI.label # set UI.text "Password"
                , string "*" #. [RequiredInput]
                , UI.input #. [W3Input, NotificationsInput] # set UI.type_ "password"
                ]
            , UI.div #. [W3Third] #+
                [ UI.label # set UI.text "SSL"
                , string "*" #. [RequiredInput]
                , UI.select #. [W3Select] # set UI.name "option" #+
                    [ UI.option # set UI.value "TLS"      # set UI.text "TLS" # set UI.selected True
                    , UI.option # set UI.value "STARTTLS" # set UI.text "STARTTLS"
                    , UI.option # set UI.value "NO"       # set UI.text "No SSL"
                    ]
                ]
            ]
        , UI.div #. [NotificationsVSpacer]
        , UI.div #. [W3RowPadding] #+
            [ UI.div #. [W3Half] #+
                [ UI.label # set UI.text "Email To"
                , string "*" #. [RequiredInput]
                , UI.input #. [W3Input, NotificationsInput] # set UI.type_ "email"
                ]
            , UI.div #. [W3Half] #+
                [ UI.label # set UI.text "Subject"
                , UI.input #. [W3Input, NotificationsInput]
                           # set UI.type_ "text" # set UI.value "Cardano RTView Notification"
                ]
            ]
        , UI.div #. [NodeMetricsVSpacer]
        ]
    ]

infoMark :: String -> UI Element
infoMark aTitle =
  UI.span #. [InfoMark]
          #  set UI.title__ aTitle
          #+ [ UI.img #. [InfoMarkImg]
                      # set UI.src "/static/images/question.svg" #+ []
             ]
