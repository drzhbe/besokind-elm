module ChatPage exposing (viewChatListPage, viewChatPage)
import Html exposing (..)
import Html.Events exposing (onClick)
import Dict

import Types exposing (..)


-- ChatList

viewChatListPage : Model -> Html Msg
viewChatListPage model =
    viewChatList model

viewChatList : Model -> Html Msg
viewChatList model =
    div []
        [ text "Chat List"
        , ul [] (List.map viewChatListItem (Debug.log "rooms" model.rooms))
        ]

viewChatListItem : Room -> Html Msg
viewChatListItem room =
    li [ onClick (SetPage (PageChat room.id)) ] [ text room.id ]


-- Chat

viewChatPage : Model -> String -> Html Msg
viewChatPage model chatId =
    div []
        [ viewChat model chatId
        , viewChatInput model chatId
        ]

viewChat : Model -> String -> Html Msg    
viewChat model chatId =
    div []
        [ text "Message List"
        , ul [] (List.map viewMessage model.activeRoom.messages)
        ]

viewMessage : IM -> Html Msg
viewMessage im =
    li []
        [ text im.userId
        , text (toString im.date)
        , text im.text
        ]

viewChatInput : Model -> String -> Html Msg
viewChatInput model chatId =
    div []
        [ text "Input"
        , span
            [ onClick (SendMessage
                { chatId = chatId
                , im = (IM "" model.user.uid "text message" 0)
                })
            ]
            [ text "Send" ]
        ]
