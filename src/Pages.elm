module Pages exposing (toHash, defaultPage, getPageByLocation, updatePage)

import UrlParser as Url exposing (Parser, (</>), int, oneOf, s, string, map)
import Navigation
import Task
import Time exposing (Time)
import Array
import Dict

import Types exposing (..)
import Data exposing (fetchDataForPage)
import Ports exposing (..)


defaultPage : Navigation.Location -> Page
defaultPage location =
    let
        page = getPageByLocation location
    in
        if page == PageNotFound
        then PageHome
        else page


getPageByLocation : Navigation.Location -> Page
getPageByLocation location =
    ensurePage (Url.parseHash pageParser location)


ensurePage : Maybe Page -> Page
ensurePage page =
    case page of
        Nothing ->
            PageNotFound
        Just page ->
            page


updatePage : Model -> Page -> (Model, Cmd Msg)
updatePage model page =
    let
        newModel = case page of
            PageChat id ->
                { model
                | page = page
                , activeRoomId = id }

            _ -> { model | page = page }

        defaultCmd =
            [ fetchDataForPage model page
            , Task.perform NewTime Time.now
            ]

        cmd = case page of
            PageHome ->
                let
                    lastCard =
                        case List.head (List.reverse model.cardList) of
                            Nothing -> emptyCard
                            Just card -> card
                in
                    Cmd.batch defaultCmd
                    --    <| fetchStreamCards lastCard.id
                    --    :: defaultCmd
            PageChat id ->
                let
                    room =
                        case Dict.get model.activeRoomId model.rooms of
                            Nothing -> emptyRoom
                            Just r -> r
                in
                    Cmd.batch
                        <| scrollElementToEnd { elementId = "chat-history", count = Array.length room.messages }
                        :: defaultCmd

            _ -> Cmd.batch defaultCmd
    in
        ( newModel, cmd)


toHash : Page -> String
toHash page =
    case page of
        PageHome ->
            "#/home"

        PageNotFound ->
            "#/404"

        PageCards ->
            "#/cards"

        PageCard id ->
            "#/card/" ++ id

        PagePrizes ->
            "#/prizes"

        PagePrize id ->
            "#/prize/" ++ id

        PageUser id ->
            "#/user/" ++ id

        PageChatList ->
            "#/chats"

        PageChat id ->
            "#/chat/" ++ id


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ Url.map PageHome (Url.s "home")
        , Url.map PageCards (Url.s "cards")
        , Url.map PageCard (Url.s "card" </> Url.string)
        , Url.map PagePrizes (Url.s "prizes")
        , Url.map PagePrize (Url.s "prize" </> Url.string)
        , Url.map PageUser (Url.s "user" </> Url.string)
        , Url.map PageChatList (Url.s "chats")
        , Url.map PageChat (Url.s "chat" </> Url.string)
        ]
