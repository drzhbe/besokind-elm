module Main exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, targetValue, keyCode, on, onWithOptions, Options)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (at, string, value, succeed)
import Navigation
import UrlParser as Url exposing (Parser, (</>), int, oneOf, s, string, map)
import String
import Set
import Dict

import Types exposing (..)
import Ports exposing (..)
import Update exposing (update)
import Pages exposing (toHash, defaultPage, updatePage)
import Data exposing (fetchDataForPage)
import ChatPage


main =
    Navigation.program HandleUrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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


init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        page = defaultPage location
        model =
            { page = page
            , loggedIn = False
            , title = ""
            , cardText = ""
            , cardInputFocus = False
            , place = ""
            , user = (User "" "" "" "" 0 False)
            , cards = []
            , userCards = []
            , activeCard = (Card "" "" "" "" 0 "" 0 "" "" "" "")
            , activeRoom = (Room "" [])
            , activeCardVolunteers = []
            , activeUser = (User "" "" "" "" 0 False)
            , userTakenCards = []
            , karma = ("", 0)
            , popup = NoPopup
            , notifications = Dict.empty
            , rooms = []
            , usersOnline = Set.empty
            }
    in
        updatePage model page


-- VIEW

-- coral
brandColor : String
brandColor = "#f2836b"
brandLighterColor : String
brandLighterColor = "#f2c3ab"
brandLightestColor : String
brandLightestColor = "#ffe3cb"
-- almbost black
darkestColor : String
darkestColor = "#333"
-- lightest gray
grayColor : String
grayColor = "#999"
-- lightest
grayLightestColor : String
grayLightestColor = "#f9f9f9"
-- blue
linkColor : String
linkColor = "#1da1f2"

buttonStyle : List (String, String)
buttonStyle =
    [ ("cursor", "pointer")
    , ("padding", "4px 8px")
    , ("color", "white")
    , ("border-radius", "4px")
    ]
deleteButtonStyle : List (String, String)
deleteButtonStyle =
    [ ("background", darkestColor)
    , ("position", "absolute")
    , ("right", "0")
    , ("top", "-4px")
    ]
takeButtonStyle : List (String, String)
takeButtonStyle =
    [ ("background", brandColor)
    , ("display", "inline")
    , ("margin-left", "8px")
    ]


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        PageHome ->
            div []
                [ if model.loggedIn
                    then viewCreateCard model
                    else div [] []
                , viewCards model model.cards
                ]

        PageNotFound ->
            -- TODO: сделать 404
            viewCards model model.cards

        PageCards ->
            viewCards model model.cards

        PageCard id ->
            viewCardFull model model.activeCard

        PagePrizes ->
            viewCards model model.cards

        PagePrize id ->
            viewCards model model.cards

        PageUser id ->
            div []
            [ viewProfile model.loggedIn model.activeUser
            , if not (List.isEmpty model.userCards)
                then h3 [] [ text "Дела пользователя:" ]
                else text ""
            , viewCards model model.userCards
            , if not (List.isEmpty model.userTakenCards)
                then h3 [] [ text "Волонтер в делах:" ]
                else text ""
            , viewCards model model.userTakenCards
            ]

        PageChatList ->
            ChatPage.viewChatListPage model

        PageChat id ->
            ChatPage.viewChatPage model id
            

view : Model -> Html Msg
view model =
    div [ onClick HidePopup ]
    [ viewTopbar model
    , div
        [ id "page-container"
        , style [ ("max-width", "560px"), ("margin", "0 auto") ]
        ]
        [ div
            [ id "content-cointainer"
            , style [ ("margin", "56px 8px 8px 8px") ]
            ]
            [ viewPage model ]
        ]
    ]


viewTopbar : Model -> Html Msg
viewTopbar model =
    let
        hasNotifications = Dict.size model.notifications > 0
        unreadNotificationsCount = Dict.size <| Dict.filter (\_ notification -> not notification.read) model.notifications
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
                        , onWithOptions
                            "click"
                            (Options True True)
                            (Json.succeed
                                (if model.popup /= NoPopup
                                    then HidePopup
                                    else ShowNotificationsPopup))
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
                            then viewNotificationsListPopup model
                            else text ""
                        ]
                    , li
                        [ class "nav-item"
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
                            , ("width", "46px")
                            , ("margin-right", "4px")
                            ]
                        ]
                        [ img
                            [ onWithOptions
                                "click"
                                (Options True True)
                                (Json.succeed
                                    (if model.popup /= NoPopup
                                        then HidePopup
                                        else ShowProfileMenuPopup))
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
                            then viewProfileMenuPopup model
                            else text ""
                        ]
                    else div
                        [ class "topbar__login-btn"
                        , onClick Login
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


viewNotificationsListPopup : Model -> Html Msg
viewNotificationsListPopup model =
    ul
        [ class "notification-list"
        , style
            [ ("position", "absolute")
            , ("width", "auto")
            , ("background", "white")
            , ("margin", "0")
            , ("border", "1px solid #ddd")
            , ("white-space", "nowrap")
            , ("line-height", "1em")
            ]
        ]
        (List.map viewNotification (Dict.values model.notifications))


viewNotification : Notification -> Html Msg
viewNotification notification =
    li
        [ style
            [ ("color", grayColor)
            ]
        ]
        [ if notification.name == "userTookCard"
            then viewUserTookCardNotification notification
            else if notification.name == "userAssignedToCard"
            then viewUserAssignedToCardNotification notification
            else div [] [ text notification.name ]
        ]


viewUserTookCardNotification : Notification -> Html Msg
viewUserTookCardNotification notification =
    div []
        [ span
            [ class "light-btn"
            , onClick (SetPage (PageUser notification.userId))
            ]
            [ text notification.userName ]
        , text " хочет вам "
        , span
            [ class "light-btn"
            , onClick (SetPage (PageCard notification.cardId))
            ]
            [ text "помочь" ]
        ]


viewUserAssignedToCardNotification : Notification -> Html Msg
viewUserAssignedToCardNotification notification =
    div []
        [ span
            [ class "light-btn"
            , onClickPreventDefault (SetPage (PageUser notification.cardAuthorId))
            ]
            [ text notification.userName ]
        , text " ждет вашей "
        , span
            [ class "light-btn"
            , onClickPreventDefault (SetPage (PageCard notification.cardId))
            ]
            [ text "помощи" ]
        ]


viewCreateCard : Model -> Html Msg
viewCreateCard model =
    let
        emptyText = String.isEmpty model.cardText
        expandedTextArea = not emptyText || model.cardInputFocus
        createButtonActivityColors =
            if emptyText
            then [ ("background", brandLighterColor), ("color", grayLightestColor) ]
            else [ ("background", brandColor), ("color", "white") ]
    in
        div [ style
                [ ("min-height", "50px")
                , ("padding", "10px")
                , ("background", brandLightestColor)
                , ("border-radius", "4px")
                ]
            ]
            [ a [ href (toHash (PageUser model.user.uid)) ]
                [ img
                    [ src model.user.photoURL
                    , width 48, height 48
                    , style [ ("float", "left"), ("border-radius", "4px") ]
                    ] []
                ]
            , div [ style [ ("padding", "0 8px 0 56px") ] ]
                [ textarea
                    [ class "card-input"
                    , placeholder "Какая помощь вам требуется?"
                    , Html.Attributes.value model.cardText
                    , rows (if expandedTextArea then 4 else 2)
                    , onInput SetCardText
                    , Html.Events.onFocus (CardInputFocus True)
                    , Html.Events.onBlur (CardInputFocus False)
                    , style
                        [ ("outline", "none")
                        , ("border", "0")
                        , ("resize", "none")
                        , ("font-size", "14px")
                        , ("width", "100%")
                        , ("padding", "8px 0 4px 8px")
                        , ("border", "1px solid " ++ brandLighterColor)
                        ]
                    ] []
                ]
            , div
                [ style
                    [ ("display", (if expandedTextArea then "block" else "none"))
                    , ("height", "40px")
                    ]
                ]
                [ div
                    [ onClick CreateCard
                    , style (
                        [ ("float", "right")
                        , ("margin", "4px 6px 0 0")
                        , ("padding", "10px 20px")
                        , ("border-radius", "4px")
                        , ("max-width", "100px")
                        , ("cursor", (if emptyText then "default" else "pointer"))
                        ] ++ createButtonActivityColors)
                    ]
                    [ text "Создать дело" ]
                ]
            ]


viewCards : Model -> (List Card) -> Html Msg
viewCards model cards =
    ul [] (List.map (viewCard model) cards)


viewCard : Model -> Card -> Html Msg
viewCard model card =
    li
        [ class (if not (String.isEmpty card.assignedTo) then "_assigned" else "")
        , style [ ("padding", "10px") ]
        ]
        [ viewCardHeader card (Set.member card.authorId model.usersOnline)
        , div [ class "card-title" ] [ text card.title ]
        , div
            [ class "card-body"
            , style [ ("margin-top", "10px"), ("word-wrap", "break-word") ]
            ]
            [ text card.body ]
        , div [ style [ ("margin-top", "8px"), ("position", "relative") ] ]
            [ viewCardKarmaPrice model.user.moderator card ]
        ]


viewCardFull : Model -> Card -> Html Msg
viewCardFull model card =
    div [ style [ ("padding", "10px") ] ]
    [ viewCardHeader card (Set.member card.authorId model.usersOnline)
    , div [ class "card-title"] [ text card.title ]
    , div
        [ class "card-body"
        , style [ ("margin-top", "10px"), ("word-wrap", "break-word") ]
        ]
        [ text card.body ]
    , div [ style [ ("margin-top", "8px"), ("position", "relative") ] ]
        [ viewCardKarmaPrice model.user.moderator card
        , if not (String.isEmpty model.user.uid) && model.user.uid == card.authorId
            then div
                [ onClick (RemoveCard card)
                , style (buttonStyle ++ deleteButtonStyle)
                ]
                [ text "Удалить" ]
            else text ""
        , if not (String.isEmpty model.user.uid)
            && model.user.uid /= card.authorId
            && not (List.member model.user model.activeCardVolunteers)
            then div
                [ onClick (TakeCard model.user card)
                , style (buttonStyle ++ takeButtonStyle)
                ]
                [ text "Помочь" ]
            else text ""
        ]
    , if not (List.isEmpty model.activeCardVolunteers)
        then div []
        [ h3 [] [ text "Желающие помочь:" ]
        , ul [] (List.map (viewVolunteer card model.user) model.activeCardVolunteers)
        ]
        else text ""
    ]


viewCardHeader : Card -> Bool -> Html Msg
viewCardHeader card authorOnline =
    div [ class "card-header" ]
        [ a [ href (toHash (PageUser card.authorId))
            , classList
                [ ("card-header__author-photo", True)
                , ("online", authorOnline)
                ]
            , style
                [ ("position", "relative")
                , ("display", "block")
                , ("float", "left")
                ]
            ]
            [ img
                [ src card.authorPhotoURL
                , width 48, height 48
                , style [ ("float", "left"), ("border-radius", "4px") ]
                ] []
            ]
        , div
            [ style
                [ ("height", "40px")
                , ("padding-top", "8px")
                , ("margin-left", "58px")
                ]
            ]
            [ ul []
                [ li
                    [ style [ ("color", darkestColor) , ("font-weight", "500") ] ]
                    [ viewLink (PageUser card.authorId) card.authorName ]
                , li
                    [ style [ ("color", grayColor) ] ]
                    [ viewLink (PageCard card.id) card.creationTimeFriendly ]
                ]
            ]
        ]

viewCardKarmaPrice : Bool -> Card -> Html Msg
viewCardKarmaPrice userIsModerator card =
    span []
        [ span [ style [ ("color", grayColor) ] ] [ text "Карма: " ]
        , span
            [ contenteditable userIsModerator
            , on "blur" (Json.map (UpdateKarma card.authorId card.id) textContentDecoder)
            ]
            [ text (toString card.karma) ]
        ]



viewVolunteer : Card -> User -> User -> Html Msg
viewVolunteer card currentUser volunteer =
    li []
        [ a [ href (toHash (PageUser volunteer.uid)) ]
            [ img
                [ src volunteer.photoURL
                , width 25, height 25
                , style [ ("float", "left"), ("border-radius", "4px") ]
                ] []
            ]
        , span
            [ style [ ("line-height", "25px"), ("margin-left", "4px") ] ]
            [ viewLink (PageUser volunteer.uid) volunteer.name ]
        , if (String.isEmpty card.assignedTo)
            && not (String.isEmpty currentUser.uid)
            && currentUser.uid == card.authorId
            then div
                [ onClick (AssignVolunteer card volunteer currentUser.name)
                , style (buttonStyle ++ takeButtonStyle)
                ]
                [ text "Принять помощь" ]
            else text ""
        ]


viewProfile : Bool -> User -> Html Msg
viewProfile loggedIn user =
    div [ style [ ("position", "relative"), ("margin-top", "10px") ] ]
    [ img [ src user.photoURL, width 200, height 200 ] []
    , ul
        [ style
            [ ("position", "absolute")
            , ("margin-left", "10px")
            , ("display", "inline-block")
            ]
        ]
        [ li [] [ text user.name ]
        , li []
            [ span [ style [ ("color", grayColor) ] ] [ text "Карма: " ]
            , span [] [ text (toString user.karma) ]
            ]
        ]
    ]


viewProfileMenuPopup : Model -> Html Msg
viewProfileMenuPopup model =
    ul
        [ style
            [ ("position", "absolute")
            , ("right", "0")
            , ("top", "54px") -- 46 topbar + 8 margin
            , ("width", "auto")
            , ("background", "white")
            , ("margin", "0")
            , ("padding", "10px")
            , ("border-radius", "5px")
            , ("border", "1px solid #ddd")
            , ("white-space", "nowrap")
            ]
        ]
        [ li [] [ a [ href (toHash (PageUser model.user.uid)), class "light-btn" ] [ text "Открыть профиль" ] ]
        , li [ onClick Logout, class "light-btn" ] [ text "Выйти" ]
        ]


viewLink : Page -> String -> Html msg
viewLink page description =
  a [ href (toHash page) ] [ text description ]


textContentDecoder : Json.Decoder String
textContentDecoder =
    Json.at ["target", "textContent"] Json.string

innerHTMLDecoder : Json.Decoder String
innerHTMLDecoder =
    Json.at ["target", "innerHTML"] Json.string

onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault clickHandler =
    onWithOptions
        "click"
        (Options True True)
        (Json.succeed
            clickHandler)