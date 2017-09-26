module Notification exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (..)
import Dict

import Helper as H
import Types exposing (..)
import Style exposing (..)


-- TODO: download author name and phoot to show in notifciation

--type Notification
--    = UserTookCardNotification { id : String, userId : String, userId : String, cardId : String }
--    | UserAssignedToCardNotification { id : String, userId : String, userId : String, cardId : String }


--type alias UserAssignedToCardNotification =
--    { id : String
--    , name : String
--    , userId : String
--    , cardId : String
--    }


--type alias UserTookCardNotification =
--    { id : String
--    , name : String
--    , userId : String
--    , cardId : String
--    }

viewNotificationsListPopup : Model -> Html Msg
viewNotificationsListPopup model =
    ul
        [ class "notification-list"
        , style
            [ ("position", "absolute")
            , ("max-width", "500px")
            , ("background", "white")
            , ("margin", "0")
            , ("border", "1px solid #ddd")
            , ("white-space", "nowrap")
            , ("line-height", "1em")
            , ("max-height", toString (model.appHeight - 150) ++ "px" )
            , ("overflow-y", "scroll")
            , ("-webkit-overflow-scrolling", "touch")
            ]
        ]
        (List.map (viewNotification model) model.notifications)


viewNotification : Model -> Notification -> Html Msg
viewNotification model notification =
    let
        card =
            case Dict.get notification.cardId model.cards of
                Nothing -> emptyCard
                Just c -> c

        content = case notification.name of
            "userTookCard" -> viewUserTookCardNotification card notification
            "userAssignedToCard" -> viewUserAssignedToCardNotification card notification
            "helpConfirmed" -> viewHelpConfirmedNotification card notification
            _ -> div [] [ text notification.name ]
    in
        li [ style [ ("color", grayColor) ] ] [ content ]


viewUserTookCardNotification : Card -> Notification -> Html Msg
viewUserTookCardNotification card notification =
    div [ H.onClickPreventDefault (HidePopup (SetPage (PageCard notification.cardId))) ]
        [ span
            [ class "light-btn"
            , H.onClickPreventDefault (HidePopup (SetPage (PageUser notification.userId)))
            ]
            [ text notification.userName ]
        , text " хочет вам помочь в "
        , span
            [ class "light-btn"
            , H.onClickPreventDefault (HidePopup (SetPage (PageCard notification.cardId)))
            ]
            [ text "деле" ]
        , div
            [ style notificationStyle ]
            [ text card.body ]
        ]


viewUserAssignedToCardNotification : Card -> Notification -> Html Msg
viewUserAssignedToCardNotification card notification =
    div [ H.onClickPreventDefault (HidePopup (SetPage (PageCard notification.cardId))) ]
        [ span
            [ class "light-btn"
            , H.onClickPreventDefault (HidePopup (SetPage (PageUser notification.cardAuthorId)))
            ]
            [ text notification.userName ]
        , text " выбрал вас помощником в "
        , span
            [ class "light-btn"
            , H.onClickPreventDefault (HidePopup (SetPage (PageCard notification.cardId)))
            ]
            [ text "деле" ]
        , div
            [ style notificationStyle ]
            [ text card.body ]
        ]


viewHelpConfirmedNotification : Card -> Notification -> Html Msg
viewHelpConfirmedNotification card notification =
    div [ H.onClickPreventDefault (HidePopup (SetPage (PageCard notification.cardId))) ]
        [ span
            [ class "light-btn"
            , H.onClickPreventDefault (HidePopup (SetPage (PageUser notification.cardAuthorId)))
            ]
            [ text notification.userName ]
        , span [] [ text " просил о " ]
        , span
            [ class "light-btn"
            , H.onClickPreventDefault (HidePopup (SetPage (PageCard notification.cardId)))
            ]
            [ text "помощи" ]
        , span [] [ text " и она пришла!" ]
        , div
            [ style notificationStyle ]
            [ text card.body ]
        ]


notificationStyle : List (String, String)
notificationStyle =
    [ ("max-width", "460px")
    , ("text-overflow", "ellipsis")
    , ("white-space", "nowrap")
    , ("overflow", "hidden")
    ]
