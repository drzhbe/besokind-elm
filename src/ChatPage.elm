module ChatPage exposing (viewChatListPage, viewChatPage)
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Dict
import Date exposing (fromTime, year, month, day, hour, minute)
import Time

import Types exposing (..)
import Style exposing (..)
import Pages exposing (toHash)


-- ChatList

viewChatListPage : Model -> Html Msg
viewChatListPage model =
    viewChatList model

viewChatList : Model -> Html Msg
viewChatList model =
    div []
        [ text "Chat List"
        , ul [] (List.map viewChatListItem (Dict.values model.rooms))
        ]

viewChatListItem : Room -> Html Msg
viewChatListItem room =
    li [ onClick (SetPage (PageChat room.id)) ] [ text room.id ]


-- Chat

viewChatPage : Model -> String -> Html Msg
viewChatPage model chatId =
    div []
        [ viewChatMemberList model chatId
        , viewChat model chatId
        , viewChatInput model chatId
        ]

viewChat : Model -> String -> Html Msg    
viewChat model chatId =
    let
        messages = case Dict.get chatId model.rooms of
            Just room -> room.messages
            Nothing -> []
    in
        -- bottom margin would be from the last message
        div [ style [ ("margin", "10px 10px 0 10px") ] ]
            [ ul [] (List.indexedMap (viewMessage model messages) messages)
            ]

fmtTime : Int -> String
fmtTime timePart =
    if timePart < 10
    then "0" ++ toString timePart
    else toString timePart

viewMessage : Model -> List IM -> Int -> IM -> Html Msg
viewMessage model messages idx im =
    let
        user = case Dict.get im.userId model.users of
            Just u -> u
            Nothing -> emptyUser
        -- TODO (salnikov): dont paint user avatar if prev msg was his
        --prevIm = List
        date = fromTime (toFloat im.date)
        dateNow = fromTime model.time
        today =
            year date == year dateNow
            && month date == month dateNow
            && day date == day dateNow
        time = (fmtTime (hour date)) ++ ":" ++ (fmtTime (minute date))
    in
        li [ style [ ("margin-bottom", "10px") ] ]
            [ a [ href (toHash (PageUser user.uid)) ]
                [ img
                    [ src user.photoURL
                    -- 34 is 2 lines height + 2 margin between them (16 x 2 + 2)
                    , width 34, height 34
                    , style [ ("float", "left"), ("border-radius", "4px") ]
                    ] []
                ]
            , span
                [ style
                    --[ ("line-height", "25px")
                    [ ("margin-left", "8px")
                    , ("color", darkestColor)
                    , ("font-weight", "500")
                    ]
                ]
                [ a [ href (toHash (PageUser user.uid)) ] [ text user.name ] ]
            , span
                [ style [ ("color", grayColor), ("margin-left", "8px") ] ]
                [ text time ]
            , div
                [ style [ ("margin", "2px 0 0 42px") ] ]
                [ text im.text ]
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
        rowsCount = Debug.log "ROWS" (List.length (String.split "\n" model.messageText))
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
                    , width 34, height 34
                    , style [ ("float", "left"), ("border-radius", "4px") ]
                    ] []
                ]
            , div [ style [ ("padding", "0 8px 0 42px") ] ]
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
                    [ onClick (SendMessage
                        { chatId = chatId
                        , im = (IM "" model.user.uid model.messageText 0)
                        })
                    , style (
                        [ ("float", "right")
                        , ("margin", "4px 6px 0 0")
                        , ("padding", "10px 20px")
                        , ("border-radius", "4px")
                        , ("max-width", "100px")
                        , ("cursor", (if emptyText then "default" else "pointer"))
                        ] ++ createButtonActivityColors)
                    ]
                    [ text "Отправить" ]
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


