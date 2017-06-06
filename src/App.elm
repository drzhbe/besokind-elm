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

import Helper as H
import Types exposing (..)
import Ports exposing (..)
import Update exposing (update)
import Pages exposing (toHash, defaultPage, updatePage)
import ChatPage
import Style exposing (..)


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
            { time = 0
            , page = page
            , appWidth = 0
            , appHeight = 0
            --, cities = { english = [], russian = [] }
            , cities = { engToRus = Dict.empty, list = [] }
            , filteredCityList = []
            , filterCityListQuery = ""
            , loggedIn = False
            , title = ""
            , cardText = ""
            , cardInputFocus = False
            , messageText = ""
            , messageInputFocus = False
            , messageInputHeight = 57
            , place = ""
            , user = emptyUser
            , cardList = []
            , userCards = []
            , activeCard = emptyCard
            , activeRoomId = ""
            , activeCardVolunteers = []
            , activeUser = emptyUser
            , userTakenCards = []
            , karma = ("", 0)
            , popup = NoPopup
            , notifications = []
            , usersOnline = Set.empty
            , cards = Dict.empty
            , users = Dict.empty
            , rooms = Dict.empty
            }
    in
        updatePage model page


-- VIEW

viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        PageHome ->
            div []
                [ viewCreateCard model
                , viewCards model model.cardList
                ]

        PageNotFound ->
            -- TODO: сделать 404
            viewCards model model.cardList

        PageCards ->
            viewCards model model.cardList

        PageCard id ->
            viewCardFull model model.activeCard

        PagePrizes ->
            viewCards model model.cardList

        PagePrize id ->
            viewCards model model.cardList

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
    div []
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
                        , onClickShowPopup model ShowNotificationsPopup
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
                                    , onClickShowPopup model ShowCityListPopup
                                    ]
                                    [ text city ]
                                , if model.popup == CityListPopup
                                    then viewCityListPopup model
                                    else text ""
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
                                    [ onClickShowPopup model ShowProfileMenuPopup
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


onClickShowPopup : Model -> Msg -> Attribute Msg
onClickShowPopup model msg =
    H.onClickPreventDefault
        (if model.popup /= NoPopup
            then (HidePopup NoOp)
            else msg)


viewCityListPopup : Model -> Html Msg
viewCityListPopup model =
    let
        hasError = List.length model.cities.list == 0
            || not (String.isEmpty model.filterCityListQuery)
                && List.length model.filteredCityList == 0

        errorText =
            if List.length model.cities.list == 0
            then div []
                [ span [] [ text "Список городов не загрузился. Видимо, сейчас во всем мире столько добра, что в нашем сайте нет необходимости! Позвоните близким, скажите, что любите их. А также можете попробовать обновить страницу." ]
                ]
            else
                if not (String.isEmpty model.filterCityListQuery)
                && List.length model.filteredCityList == 0
                then div []
                    [ span [] [ text "По запросу " ]
                    , span [ style [ ("color", grayColor) , ("font-weight", "bold") ] ]
                        [ text model.filterCityListQuery ]
                    , span [] [ text " не нашлось города. Напишите нам, если хотите чтобы " ]
                    , span [ style [ ("color", darkColor) , ("font-weight", "bold") ] ]
                        [ text "Будь Добр" ]
                    , span [] [ text " был в вашем славном городе" ]
                    ]
                else text ""
    in
        div [ style
                [ ("position", "absolute")
                , ("width", "300px")
                , ("left", "-200px")
                , ("background", "white")
                , ("border", "1px solid #ddd")
                ]
            ]
            [ input
                [ H.onClickPreventDefault NoOp
                , onInput SetFilterCityListText
                , placeholder "Введите название города"
                , autofocus True
                , width 100
                --, Html.Events.onFocus (FilterCityListInputFocus True)
                --, Html.Events.onBlur (FilterCityListInputFocus False)
                , style
                    [ ("margin", "10px 10px 5px 10px")
                    , ("width", "260px")
                    , ("border", "0")
                    , ("outline", "0")
                    , ("font-size", "14px")
                    , ("line-height", "20px")
                    ]
                ]
                []
            , div
                [ style
                    [ ("display", if hasError then "block" else "none")
                    , ("padding", "10px")
                    ]
                ]
                [ errorText ]
            , ul
                [ style
                    [ ("max-height", toString (model.appHeight - 150) ++ "px" )
                    , ("overflow-y", "scroll")
                    , ("-webkit-overflow-scrolling", "touch")
                    ]
                ]
                (List.map (viewCityListItem model.filterCityListQuery) model.filteredCityList)
            ]


viewCityListItem : String -> String -> Html Msg
viewCityListItem query city =
    -- TODO highlight with query
    li 
        [ onClick (SetCity city)
        , class "city-list-item"
        , style [ ("padding", "5px 10px 5px 10px") ]
        ]
        [ text city ]


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


viewCreateCard : Model -> Html Msg
viewCreateCard model =
    let
        emptyText = String.isEmpty model.cardText
        expandedTextArea = not emptyText || model.cardInputFocus
        createButtonActivityColors =
            if emptyText
            then [ ("background", brandLighterColor), ("color", grayLightestColor) ]
            else [ ("background", brandColor), ("color", "white") ]
        sendAction =
            if model.loggedIn
            then CreateCard
            else Login
        city = if String.isEmpty model.user.city
            then "Город"
            else case Dict.get model.user.city model.cities.engToRus of
                Just c -> c
                Nothing -> model.user.city
            
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
                    --, Html.Events.onBlur (CardInputFocus False)
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
                    []
                    [ span
                        [ onClickShowPopup model ShowCityListPopup
                        ]
                        [ text city ]
                    , if model.popup == CityListPopup
                        then viewCityListPopup model
                        else text ""
                    ]
                , div
                    [ onClick sendAction
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
    ul
        [ id "card-stream"
        , style
            [ ("overflow", "scroll")
            , ("-webkit-overflow-scrolling", "touch")
            -- TODO store topbar height, input height in model
            -- 56 topbar
            -- 72 input
            , ("height", toString (model.appHeight - 56 - 82) ++ "px")
            ]
        ]
        (List.map (viewCard model) cards)


viewCard : Model -> Card -> Html Msg
viewCard model card =
    li
        [ classList
            [ ("card", True)
            , ("_assigned", not (String.isEmpty card.assignedTo))
            ]
        , style [ ("padding", "10px") ]
        , onClick (SetPage (PageCard card.id))
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
    let
        background =
            case card.status of
                0 -> grayLightestColor
                1 -> "#fff"
                2 -> "#C0DF85"
                _ -> "#fff"

        deleteButton =
            if
                not (String.isEmpty model.user.uid)
                && model.user.uid == card.authorId
                && card.status < 2
            then
                div
                [ onClick (RemoveCard card)
                , style (buttonStyle ++ deleteButtonStyle)
                ]
                [ text "Удалить" ]
            else
                text ""

        helpButton =
            if
                not (String.isEmpty model.user.uid)
                && model.user.uid /= card.authorId
                && not (List.member model.user.uid model.activeCardVolunteers)
            then
                div
                [ onClick (TakeCard model.user card)
                , style (buttonStyle ++ takeButtonStyle)
                ]
                [ text "Помочь" ]
            else
                text ""

        volunteerList =
            if
                not (List.isEmpty model.activeCardVolunteers)
            then
                div []
                    [ h3 [] [ text "Желающие помочь:" ]
                    , ul [] (List.map (viewVolunteer model card model.user) model.activeCardVolunteers)
                    ]
            else
                text ""
    in
        div [ style
                [ ("padding", "10px")
                , ("background", background)
                ]
            ]
            [ viewCardHeader card (Set.member card.authorId model.usersOnline)
            , div [ class "card-title"] [ text card.title ]
            , div
                [ class "card-body"
                , style [ ("margin-top", "10px"), ("word-wrap", "break-word") ]
                ]
                [ text card.body ]
            , div [ style [ ("margin-top", "8px"), ("position", "relative") ] ]
                [ viewCardKarmaPrice model.user.moderator card
                , deleteButton
                , helpButton
                ]
            , volunteerList
            ]


viewCardHeader : Card -> Bool -> Html Msg
viewCardHeader card authorOnline =
    div [ class "card-header" ]
        [ a [ href (toHash (PageUser card.authorId))
            , H.onClickPreventDefault (SetPage (PageUser card.authorId))
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



viewVolunteer : Model -> Card -> User -> String -> Html Msg
viewVolunteer model card currentUser volunteerId =
    let
        volunteer =
            case Dict.get volunteerId model.users of
                Nothing -> emptyUser
                Just user -> user

        assignVolunteerToCardButton =
            if
                (String.isEmpty card.assignedTo)
                && not (String.isEmpty currentUser.uid)
                && currentUser.uid == card.authorId
            then
                div [ onClick (AssignVolunteer card volunteer currentUser.name)
                    , style (buttonStyle ++ takeButtonStyle)
                    ]
                    [ text "Принять помощь" ]
            else
                text ""

        confirmHelpButton =
            if
                currentUser.uid == card.authorId
                && card.assignedTo == volunteer.uid
                && card.status == 1
            then
                div [ onClick (ConfirmHelp card)
                    , style (buttonStyle ++ assignedToLabelStyle)
                    ]
                    [ text "Спасибо" ]
            else
                text ""

        helpingLabel =
            if
                card.assignedTo == volunteer.uid
                && card.status == 1
            then
                span [ style italicLabelStyle ]
                    [ text "помогает" ]
            else
                text ""

        successfulHelperLabel =
            if
                card.assignedTo == volunteer.uid
                && card.status == 2
            then
                span [ style italicLabelStyle ]
                    [ text "помог" ]
            else
                text ""
    in
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
        , helpingLabel
        , successfulHelperLabel
        , assignVolunteerToCardButton
        , confirmHelpButton
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


viewLink : Page -> String -> Html Msg
viewLink page description =
  a [ href (toHash page)
    , H.onClickPreventDefault (SetPage page)
    ]
    [ text description ]


textContentDecoder : Json.Decoder String
textContentDecoder =
    Json.at ["target", "textContent"] Json.string

innerHTMLDecoder : Json.Decoder String
innerHTMLDecoder =
    Json.at ["target", "innerHTML"] Json.string
