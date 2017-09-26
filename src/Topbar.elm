module Topbar exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (..)
import Dict

import Helper as H
import Types exposing (..)
import Style exposing (..)
import Pages exposing (toHash)
import Notification
import Profile
import CityList


viewTopbar : Model -> Html Msg
viewTopbar model =
    let
        hasNotifications = List.length model.notifications > 0
        unreadNotificationsCount = List.length <| List.filter (\notification -> not notification.read) model.notifications
        city = if String.isEmpty model.user.city
            then "Город"
            else case Dict.get model.user.city model.cities.engToRus of
                Just c -> c
                Nothing -> model.user.city
    in
        div
            [ id "topbar"
            , style
                [ ("position", "fixed")
                , ("z-index", "2")
                , ("top", "0")
                , ("left", "0")
                , ("right", "0")
                , ("height", "46px")
                , ("background", brandColor)
                , ("border-radius", "0 0 8px 8px")
                , ("visible", "false")
                ]
            ]
            [ div
                [ id "topbar-content"
                , style
                    [ ("max-width", "560px")
                    , ("margin", "0 auto")
                    ]
                ]
                [ ul
                    [ id "nav"
                    , class "horizontal-list"
                    , style
                        [ ("display", "inline-block")
                        , ("margin", "0 12px")
                        --, ("max-width", "75%")
                        ]
                    ]
                    [ li [ class "nav-item" ] [ a [ href (toHash PageHome) ] [ text "Дела" ] ]
                    --, li [ class "nav-item", style [ ("color", "#ffb494") ] ] [ text "Призы" ]
                    --, li [ class "nav-item", style [ ("color", "#ffb494") ] ] [ text "Рейтинг" ]
                    , li
                        [ classList
                            [ ("nav-item", True)
                            , ("_disabled", unreadNotificationsCount == 0)
                            ]
                        , H.onClickShowPopup model ShowNotificationsPopup
                        ]
                        [ 
                        --    img
                        --    [ src "img/notification.svg"
                        --    , width 24
                        --    , height 24
                        --    ]
                        --    []
                        --,
                        text "Уведомления"
                        , if model.popup == NotificationsListPopup
                            then Notification.viewNotificationsListPopup model
                            else text ""
                        ]
                    , li
                        [ classList
                            [ ("nav-item", True)
                            , ("_disabled", Dict.isEmpty model.rooms)
                            ]
                        , onClick (SetPage PageChatList)
                        ]
                        [
                        --    img
                        --    [ src "img/mail.svg"
                        --    , width 24
                        --    , height 24
                        --    ]
                        --    []
                        --,
                        text "Сообщения"
                        ]
                            
                    ]
                , if model.loggedIn
                    then div
                            [ style
                                [ ("position", "relative")
                                , ("float", "right")
                                --, ("width", "46px")
                                , ("margin-right", "4px")
                                ]
                            ]
                            [ div
                                [ style
                                    [ ("display", "inline-block")
                                    ]
                                ]
                                [ span
                                    [ class "nav-item"
                                    , H.onClickShowPopup model (ShowCityListPopup "topbar")
                                    ]
                                    [ text city ]
                                , case model.popup of
                                    CityListPopup target -> if target == "topbar"
                                        then CityList.viewCityListPopup target model
                                        else text ""
                                    _ -> text ""
                                ]
                            , div
                                [ style
                                    [ ("position", "relative")
                                    , ("float", "right")
                                    , ("width", "46px")
                                    , ("margin-right", "4px")
                                    ]
                                ]
                                [ img
                                    [ H.onClickShowPopup model ShowProfileMenuPopup
                                    , class "topbar__user-photo"
                                    , src model.user.photoURL
                                    , width 38
                                    , height 38
                                    , style
                                        [ ("border-radius", "100%")
                                        , ("margin", "4px")
                                        , ("cursor", "pointer")
                                        ]
                                    ]
                                    []
                                , if model.popup == ProfileMenuPopup
                                    then Profile.viewProfileMenuPopup model
                                    else text ""
                                ]
                            ] 
                    else a [ href "/auth/vkontakte" ]
                        [
                        div
                        [ class "topbar__login-btn"
                        --, onClick Login
                        , style
                            [ ("margin", "4px 10px 4px 4px")
                            , ("float", "right")
                            , ("color", "bisque")
                            , ("font-weight", "500")
                            , ("height", "38px")
                            , ("line-height", "38px")
                            , ("text-align", "center")
                            , ("cursor", "pointer")
                            ]
                        ]
                        [ text "Войти" ]
                        ]
                ]
            ]