module ChatPage exposing (viewChatListPage, viewChatPage)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Html.Attributes exposing (..)
import Dict
import Array
import Date exposing (fromTime, year, month, day, hour, minute, dayOfWeek, Month, Day)
import Time
import Json.Decode

import Helper as H
import Types exposing (..)
import Style exposing (..)
import Pages exposing (toHash)


-- ::::::::: ChatList :::::::::


viewChatListPage : Model -> Html Msg
viewChatListPage model =
    viewChatList model

viewChatList : Model -> Html Msg
viewChatList model =
    div []
        [ ul [] (List.map (viewChatListItem model) (Dict.values model.rooms))
        ]

viewChatListItem : Model -> Room -> Html Msg
viewChatListItem model room =
    let
        opponent = List.head <|
            List.filter (notMe model.user.uid) room.users
        user = case opponent of
            Nothing -> emptyUser
            Just userId ->
                case Dict.get userId model.users of
                    Nothing -> emptyUser
                    Just u -> u
        avatar = user.photoURL
        lastMsgIndex = (Array.length room.messages) - 1
        lastMsg = Array.get lastMsgIndex room.messages

        msgText = case lastMsg of
            Nothing -> [ span [ style [ ("color", grayColor), ("font-style", "italic") ] ] [ text "Сообщений пока не было" ] ]
            Just msg ->
                let
                    mainText =
                        span [] [ text msg.text ]
                in
                    if msg.userId == model.user.uid
                    then
                        [ span
                            [ style [ ("color", grayColor) ] ]
                            [ text "Вы: " ]
                        , mainText
                        ]
                    else [ mainText ]
        --date = fromTime (toFloat im.date)
        --time = (fmtTime (hour date)) ++ ":" ++ (fmtTime (minute date))
        time = "00:00"
        dateRepresentation = ""
    in
        li
            [ H.onClickPreventDefault (SetPage (PageChat room.id))
            , style
                [ ("margin-bottom", "10px")
                ]
            ]
            [ if String.isEmpty dateRepresentation
                then text ""
                else div
                    [ style
                        [ ("color", grayColor)
                        , ("margin-bottom", "16px")
                        , ("padding-top", "6px")
                        , ("text-align", "center")
                        ]
                    ]
                    [ text dateRepresentation ]
            , div []
                [ if String.isEmpty avatar
                    then text ""
                    else
                        a 
                            [ href (toHash (PageUser user.uid))
                            , H.onClickPreventDefault (SetPage (PageUser user.uid))
                            ]
                            [ img
                                [ src user.photoURL
                                -- 34 is 2 lines height + 2 margin between them (16 x 2 + 2)
                                , width 34, height 34
                                , style [ ("float", "left"), ("border-radius", "4px") ]
                                ] []
                            ]
                , if String.isEmpty avatar
                    then text ""
                    else
                        span
                            [ style
                                --[ ("line-height", "25px")
                                [ ("margin-left", "8px")
                                , ("color", darkestColor)
                                , ("font-weight", "500")
                                ]
                            ]
                            [ a [ href (toHash (PageUser user.uid))
                                , H.onClickPreventDefault (SetPage (PageUser user.uid))
                                ]
                                [ text user.name ]
                            ]
                , span
                    [ style
                        [ ("color", grayColor)
                        , ("font-size", "12px")
                        , ("float", "right")
                        ]
                    ]
                    [ text time ]
                , div
                    [ style
                        [ ("margin", "2px 42px 0 42px")
                        , ("text-overflow", "ellipsis")
                        , ("white-space", "nowrap")
                        , ("overflow", "hidden")
                        , ("max-width", "460px")
                        ]
                    ]
                    msgText
                ]
            ]


notMe : String -> String -> Bool
notMe currentUserId userId =
    currentUserId /= userId


-- ::::::::: Chat :::::::::


viewChatPage : Model -> String -> Html Msg
viewChatPage model chatId =
    div [ style
            [ ("position", "relative") ]
        ]
        --[ viewChatMemberList model chatId
        [ viewChat model chatId
        , viewChatInput model chatId
        ]

viewChat : Model -> String -> Html Msg    
viewChat model chatId =
    let
        messages = case Dict.get chatId model.rooms of
            Nothing -> Array.empty
            Just room -> room.messages
        -- 56 topbar height
        -- 33 input margins
        historyHeight = toString (model.appHeight - 56 - model.messageInputHeight - 33) ++ "px"
    in
        if Array.length messages > 0
        then
            -- bottom margin would be from the last message
            ul
                [ id "chat-history"
                , style
                    [ ("margin", "10px 10px 0 10px")
                    , ("overflow", "scroll")
                    , ("-webkit-overflow-scrolling", "touch")
                    , ("height", historyHeight)
                    ]
                ]
                (Array.toList (Array.indexedMap (viewMessage model messages) messages))
        else
            div [ style
                    [ ("width", "80%")
                    , ("margin", "0px auto")
                    , ("padding", "10px 0 20px 0")
                    , ("text-align", "center")
                    ]
                ]
                [ div [] [ text "Сообщений пока не было." ]
                , div [] [ text "Можете начать беседу с пожелания добра или любви." ]
                , div
                    [ style
                        [ ("text-align", "right")
                        , ("color", grayColor)
                        , ("margin-top", "10px")
                        ]
                    ]
                    [ span [] [ text "С любовью, " ]
                    , span
                        [ style
                            [ ("color", darkColor)
                            , ("font-weight", "bold")
                            ]
                        ]
                        [ text "Будь Добр" ]
                    ]
                ]


fmtTime : Int -> String
fmtTime timePart =
    if timePart < 10
    then "0" ++ toString timePart
    else toString timePart

trMonth : Date.Month -> String
trMonth month =
    case month of
        Date.Jan -> "января"
        Date.Feb -> "февраля"
        Date.Mar -> "марта"
        Date.Apr -> "апреля"
        Date.May -> "мая"
        Date.Jun -> "июня"
        Date.Jul -> "июля"
        Date.Aug -> "августа"
        Date.Sep -> "сентября"
        Date.Oct -> "октября"
        Date.Nov -> "ноября"
        Date.Dec -> "декабря"

trDayOfWeek : Date.Day -> String
trDayOfWeek day =
    case day of
        Date.Mon -> "Понедельник"
        Date.Tue -> "Вторник"
        Date.Wed -> "Среда"
        Date.Thu -> "Четверг"
        Date.Fri -> "Пятница"
        Date.Sat -> "Суббота"
        Date.Sun -> "Воскресенье"

dateToString : Date.Date -> String
dateToString date =
    trDayOfWeek (dayOfWeek date)
    ++ ", "
    ++ toString (day date)
    ++ " "
    ++ trMonth (month date)
    ++ ", "
    ++ toString (year date)

viewMessage : Model -> Array.Array IM -> Int -> IM -> Html Msg
viewMessage model messages idx im =
    let
        user = case Dict.get im.userId model.users of
            Just u -> u
            Nothing -> emptyUser
        date = fromTime (toFloat im.date)
        time = (fmtTime (hour date)) ++ ":" ++ (fmtTime (minute date))

        prevMsgMaybe = Array.get (idx-1) messages
        dateRepresentation = case prevMsgMaybe of
            Just prevMsg ->
                let
                    prevDate = fromTime (toFloat prevMsg.date)
                in
                    if year date == year prevDate
                    && month date == month prevDate
                    && day date == day prevDate
                    then ""
                    else dateToString date
            Nothing -> dateToString date
        avatar = case prevMsgMaybe of
            Just prevMsg ->
                if im.userId /= prevMsg.userId || not (String.isEmpty dateRepresentation)
                then user.photoURL
                else ""
            Nothing -> user.photoURL
    in
        -- не рисуем сообщения, если информация об авторах еще не загрузилась
        if String.length user.uid > 0
        then viewMessagePrepared dateRepresentation time user avatar im.text
        else text ""


viewMessagePrepared : String -> String -> User -> String -> String -> Html Msg
viewMessagePrepared dateRepresentation time user avatar msgText =
    li [ style [ ("margin-bottom", "10px") ] ]
        [ if String.isEmpty dateRepresentation
            then text ""
            else div
                [ style
                    [ ("color", grayColor)
                    , ("margin-bottom", "16px")
                    , ("padding-top", "6px")
                    , ("text-align", "center")
                    ]
                ]
                [ text dateRepresentation ]
        , div []
            [ if String.isEmpty avatar
                then text ""
                else
                    a [ href (toHash (PageUser user.uid)) ]
                        [ img
                            [ src user.photoURL
                            -- 34 is 2 lines height + 2 margin between them (16 x 2 + 2)
                            , width 34, height 34
                            , style [ ("float", "left"), ("border-radius", "4px") ]
                            ] []
                        ]
            , if String.isEmpty avatar
                then text ""
                else
                    span
                        [ style
                            --[ ("line-height", "25px")
                            [ ("margin-left", "8px")
                            , ("color", darkestColor)
                            , ("font-weight", "500")
                            ]
                        ]
                        [ a [ href (toHash (PageUser user.uid)) ] [ text user.name ] ]
            , span
                [ style
                    [ ("color", grayColor)
                    , ("font-size", "12px")
                    , ("float", "right")
                    ]
                ]
                [ text time ]
            , div
                [ style [ ("margin", "2px 42px 0 42px") ] ]
                [ text msgText ]
            ]
        ]

viewChatInput : Model -> String -> Html Msg
viewChatInput model chatId =
    --div []
    --    [ text "Input"
    --    , span
            --[ onClick (SendMessage
            --    { chatId = chatId
            --    , im = (IM "" model.user.uid "text message" 0)
            --    })
    --        ]
    --        [ text "Send" ]
    --    ]
    let
        emptyText = String.isEmpty model.messageText
        expandedTextArea = not emptyText || model.cardInputFocus
        createButtonActivityColors =
            if emptyText
            then [ ("background", brandLighterColor), ("color", grayLightestColor) ]
            else [ ("background", brandColor), ("color", "white") ]
        rowsCount = List.length (String.split "\n" model.messageText)
        bottomPosition = toString -model.messageInputHeight ++ "px"
    in
        div [ style
                [ ("min-height", "30px")
                , ("padding", "10px")
                , ("background", brandLightestColor)
                , ("border-radius", "4px")
                , ("width", "530px")
                , ("position", "absolute")
                , ("bottom", bottomPosition)
                ]
            ]
            [ a [ href (toHash (PageUser model.user.uid)) ]
                [ img
                    [ src model.user.photoURL
                    , width 34, height 34
                    , style [ ("float", "left"), ("border-radius", "4px") ]
                    ] []
                ]
            , div
                [ onClick (SendMessage
                    { chatId = chatId
                    , im = (IM "" model.user.uid model.messageText 0)
                    })
                , style (
                    [ ("float", "right")
                    --, ("margin", "4px 6px 0 0")
                    , ("padding", "8px")
                    , ("border-radius", "4px")
                    , ("max-width", "100px")
                    , ("cursor", (if emptyText then "default" else "pointer"))
                    ] ++ createButtonActivityColors)
                ]
                [ text "Отпр" ]
            , div [ style [ ("padding", "0 72px 0 42px") ] ]
                [ textarea
                    [ class "card-input"
                    , placeholder "Напишите сообщение..."
                    , Html.Attributes.value model.messageText
                    , rows rowsCount
                    , onInput SetMessageText
                    , Html.Events.onFocus (MessageInputFocus True)
                    , Html.Events.onBlur (MessageInputFocus False)
                    , style
                        [ ("outline", "none")
                        , ("border", "0")
                        , ("resize", "none")
                        , ("font-size", "14px")
                        , ("line-height", "20px")
                        , ("width", "100%")
                        , ("padding", "6px 4px")
                        , ("border", "1px solid " ++ brandLighterColor)
                        ]
                    ] []
                ]

            ]


viewChatMemberList : Model -> String -> Html Msg
viewChatMemberList model chatId =
    let
        members = case Dict.get chatId model.rooms of
            Just room ->
                List.filter
                    (\user -> List.member user.uid room.users)
                    (Dict.values model.users)
            Nothing -> []
    in
        if List.length members > 0
        then ul [] (List.map viewChatMember members)
        else text ""

viewChatMember : User -> Html Msg
viewChatMember user =
    li []
        [ a [ href (toHash (PageUser user.uid)) ]
            [ img
                [ src user.photoURL
                , width 25, height 25
                , style [ ("float", "left"), ("border-radius", "4px") ]
                ] []
            ]
        , span
            [ style [ ("line-height", "25px"), ("margin-left", "4px") ] ]
            [ a [ href (toHash (PageUser user.uid)) ] [ text user.name ] ]

        ]


