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
import Style exposing (..)

import Topbar
import ChatPage
import Profile
import Card
import CardCreate


main =
    Navigation.program HandleUrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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
            , cardCity = ""
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
            

view : Model -> Html Msg
view model =
    div []
        [ Topbar.viewTopbar model
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


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        PageHome ->
            div []
                [ CardCreate.viewCreateCard model
                , Card.viewCards model model.cardList
                ]

        PageNotFound ->
            -- TODO: сделать 404
            Card.viewCards model model.cardList

        PageCards ->
            Card.viewCards model model.cardList

        PageCard id ->
            Card.viewCardFull model model.activeCard

        PagePrizes ->
            Card.viewCards model model.cardList

        PagePrize id ->
            Card.viewCards model model.cardList

        PageUser id ->
            div []
            [ Profile.viewProfile model.loggedIn model.activeUser
            , if not (List.isEmpty model.userCards)
                then h3 [] [ text "Дела пользователя:" ]
                else text ""
            , Card.viewCards model model.userCards
            , if not (List.isEmpty model.userTakenCards)
                then h3 [] [ text "Волонтер в делах:" ]
                else text ""
            , Card.viewCards model model.userTakenCards
            ]

        PageChatList ->
            ChatPage.viewChatListPage model

        PageChat id ->
            ChatPage.viewChatPage model id
