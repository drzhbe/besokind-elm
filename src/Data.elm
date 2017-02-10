module Data exposing (fetchDataForPage)

import Types exposing (..)
import Ports exposing (..)

fetchDataForPage : Page -> Cmd Msg
fetchDataForPage page =
    case page of
        PageHome ->
            fetchStreamCards "TODO port should recieve at least 1 arg :/"

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
            Cmd.none

        PageChat id ->
            Cmd.batch
                [ fetchRoomMetadata id
                , watchChat id
                ]
