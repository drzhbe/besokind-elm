module Data exposing (fetchDataForPage)

import Dict
import Array

import Types exposing (..)
import Ports exposing (..)

fetchDataForPage : Model -> Page -> Cmd Msg
fetchDataForPage model page =
    case page of
        PageHome ->
            watchStreamCards "TODO port should recieve at least 1 arg :/"

        PageNotFound ->
            Cmd.none

        PageCards ->
            Cmd.none

        PageCard id ->
            Cmd.batch
                [ fetchCard id
                , fetchCardVolunteers id
                ]

        PagePrizes ->
            Cmd.none

        PagePrize id ->
            Cmd.none

        PageUser id ->
            Cmd.batch
                [ fetchUser { id = id, purpose = "openUserPage" }
                , fetchUserCards id
                , fetchUserTakenCards id
                ]

        PageChatList ->
            let
                chatIdList = Dict.keys model.rooms
            in
                Cmd.batch
                    <| List.map (\id -> fetchRoomMetadata id) chatIdList
                    --++ List.map (\id -> fetchLastChatMessage id) chatIdList

        PageChat id ->
            let
                room =
                    case Dict.get id model.rooms of
                        Nothing -> emptyRoom
                        Just r -> r
                lastMessageId =
                    case Array.get 0 room.messages of
                        Nothing -> ""
                        Just message -> message.id
                needToFetch = Array.length room.messages < 12
            in
                if needToFetch
                then
                    Cmd.batch
                        [ fetchRoomMetadata id
                        , fetchChatMessages { chatId = id, lastMessageId = lastMessageId }
                        ]
                else fetchRoomMetadata id
