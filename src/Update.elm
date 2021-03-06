module Update exposing (update)

import Task
import Time exposing (Time)
import Set
import Dict
import Array
import Navigation
import Char
import Json.Decode exposing (Decoder)

import Types exposing (..)
import Ports exposing (..)
import Pages exposing (toHash, getPageByLocation, updatePage)
import Data exposing (fetchDataForPage)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- костыль для того, чтобы принимать порт и вызывать Msg без аргументов,
        -- но т.к. порт всегда должен что-то принимать, делаем такую обертку
        PortWithNoArgs actualMsg _ ->
            update actualMsg model

        NewTime time ->
            ( { model | time = time }, Cmd.none )

        HandleUrlChange location ->
            updatePage model (getPageByLocation location)

        SetPage page ->
            ( model, Navigation.newUrl <| toHash page )

        SetCardCity city ->
            ( { model | cardCity = city }, Cmd.none )

        SetCardText text ->
            ( { model | cardText = text }, Cmd.none )

        CardInputFocus focused ->
            let
                command =
                    if focused
                    then Cmd.none
                    else persistCardText model.cardText
            in
                ( { model | cardInputFocus = focused }, command )

        SetMessageText text ->
            let
                rowsCount = List.length (String.split "\n" text)
                -- 20 lineheight
                -- 3 magic additional height when wrap textarea
                -- 12 padding from top and bottom 6
                -- 20 padding from top and bottom 10
                -- 2 border from top and bottom 1
                inputHeight = rowsCount * 20 + 3 + 34

                newModel =
                    if inputHeight /= model.messageInputHeight
                    then { model | messageText = text, messageInputHeight = inputHeight }
                    else { model | messageText = text }

                room =
                    case Dict.get model.activeRoomId model.rooms of
                    Nothing -> emptyRoom
                    Just r -> r

                cmd =
                    if inputHeight /= model.messageInputHeight
                    then scrollElementToEnd { elementId = "chat-history", count = Array.length room.messages }
                    else Cmd.none
            in
                ( newModel, cmd )

        MessageInputFocus focused ->
            let
                command =
                    if focused
                    then Cmd.none
                    else Cmd.none
                    --else persistCardText model.cardText
            in
                ( { model | messageInputFocus = focused }, command )

        SetFilterCityListText text ->
            update FilterCityList { model | filterCityListQuery = text }

        FilterCityList ->
            let
                query = case String.uncons model.filterCityListQuery of
                    Just (head, tail) -> String.cons (Char.toUpper head) tail
                    Nothing -> ""
                _ = Debug.log ":::: query" query
                filteredCityList = List.filter (\city -> String.startsWith query city) model.cities.list
            in
                ( { model | filteredCityList = filteredCityList }, Cmd.none )

        SetCity city ->
            ( model, setCity { userId = model.user.uid, city = city } )

        Login ->
            ( model, login "google" )

        Logout ->
            ( model, logout "TODO port should recieve at least 1 arg :/" )

        SetUser user ->
            ( { model
                | user = user
                , loggedIn = not (String.isEmpty user.name)
              }, Cmd.none )

        CreateCard ->
            let
                city =
                    if not (String.isEmpty model.cardCity)
                    then model.cardCity
                    else model.user.city
            in
            ( { model | cardText = "" }
            , Cmd.batch
                [ createCard
                    { id = ""
                    , status = 0
                    , authorId = model.user.uid
                    , authorName = model.user.name
                    , authorPhotoURL = model.user.photoURL
                    , creationTime = 0
                    , creationTimeFriendly = ""
                    , karma = 0
                    , city = city
                    , place = model.place
                    , title = model.title
                    , body = model.cardText
                    , assignedTo = ""
                    , assignedAt = 0
                    }
                , persistCardText ""
                ]
            )

        ShowCards cards ->
            ( { model | cardList = cards }, Cmd.none )

        ShowUserCards cards ->
            ( { model | userCards = cards }, Cmd.none )

        AddCardToList card ->
            case List.head model.cardList of
                Just c ->
                    if card.creationTime > c.creationTime
                        then
                            ( { model
                                | cardList = card :: model.cardList
                                , cards = Dict.insert card.id card model.cards }
                            , Cmd.none )
                        else ( model, Cmd.none )
                Nothing ->
                    ( { model
                        | cardList = card :: model.cardList
                        , cards = Dict.insert card.id card model.cards }
                    , Cmd.none )

        AddCardsToList newCards ->
            let
                lastCard =
                    case List.head (List.reverse newCards) of
                        Nothing -> emptyCard
                        Just card -> card
            in
                ( { model
                    | cardList = List.append model.cardList newCards
                    , cards = Dict.union
                        (Dict.fromList <| List.map (\card -> (card.id, card)) newCards)
                        model.cards
                    }
                , enableCardStreamInfiniteScroll { elementId = "card-stream", lastCardId = lastCard.id } )

        UpdateCard card ->
            ( { model
                | cardList = List.map (replaceCard card) model.cardList
                , cards = Dict.insert card.id card model.cards
                , activeCard = replaceCard card model.activeCard
            }, Cmd.none )

        ShowCard card ->
            ( { model | activeCard = card }, Cmd.none )

        ShowVolunteers userIdList ->
            ( { model | activeCardVolunteers = userIdList }, Cmd.none )

        UserFetched user ->
            ( { model | users = Dict.insert user.uid user model.users }, Cmd.none )

        SetActiveUser user ->
            ( { model | activeUser = user }, Cmd.none )

        ShowUserTakenCards cards ->
            ( { model | userTakenCards = cards }, Cmd.none )

        UpdateKarma authorId cardId karma ->
            ( model, updateKarma
                { authorId = authorId
                , cardId = cardId
                , karma = Result.withDefault 0 (String.toInt karma)
                }
            )

        TakeCard user card ->
            ( model, takeCard
                { user = user
                , card = card
                }
            )

        RemoveCard card ->
            ( model, removeCard card )

        HandleRemoveCard card ->
            ( model, Navigation.newUrl (toHash PageHome) )

        AssignVolunteer card user cardAuthorName ->
            ( model, assignVolunteer { card = card, user = user, userName = cardAuthorName } )

        ConfirmHelp card ->
            ( model, confirmHelp card )

        ShowProfileMenuPopup ->
            ( { model | popup = ProfileMenuPopup }, Cmd.none )

        ShowNotificationsPopup ->
            let
                notReadNotificationIdList =
                    List.map
                        (\notification -> notification.id)
                        <| List.filter
                            (\notification -> not notification.read)
                            model.notifications
            in
                ( { model
                    | popup = NotificationsListPopup
                    , notifications = List.map updateNotificationAsRead model.notifications
                }, markNotificationsAsRead { userId = model.user.uid, notificationIdList = notReadNotificationIdList } )

        ShowCityListPopup target ->
            let
                focusTarget = case target of
                    "topbar" -> "topbar__citylist"
                    "card-input" -> "card-input__citylist"
                    _ -> ""
            in
            ( { model
                | popup = CityListPopup target
                , filterCityListQuery = ""
                , filteredCityList = model.cities.list }
            , focusElement focusTarget )

        HidePopup callback ->
            let
                newModel =
                    if model.popup == NoPopup
                    then model
                    else { model | popup = NoPopup }
            in
                case callback of
                    NoOp -> ( newModel, Cmd.none )
                    _ -> update callback newModel

        AddNotification notification ->
            ( { model
                | notifications = notification :: model.notifications }
            , Cmd.none )

            -- TODO not used
        --MarkNotificationsAsRead notificationIdList ->
        --    ( { model
        --        | notifications = Dict.map updateNotificationAsRead model.notifications }
        --    , markNotificationsAsRead { userId = model.user.uid, notificationIdList = notificationIdList } )

        RoomAdded room ->
            let
                rooms =
                    if Dict.member room.id model.rooms
                    then model.rooms
                    else Dict.insert room.id room model.rooms

                newModel = { model | rooms = rooms }

                cmd =
                    if model.page == PageChatList
                    then fetchDataForPage newModel model.page
                    else Cmd.none
            in
                ( newModel, cmd )

        AddOnlineUser userId ->
            ( { model
                | usersOnline = Set.insert userId model.usersOnline }
            , Cmd.none )

        RemoveOnlineUser userId ->
            ( { model
                | usersOnline = Set.remove userId model.usersOnline }
            , Cmd.none )

        ShowRoom room ->
            ( { model | activeRoomId = room.id }, Cmd.none )

        MessageAdded chatMessage ->
            let
                room = case Dict.get chatMessage.chatId model.rooms of
                    Nothing -> Room chatMessage.chatId [] (Array.repeat 1 chatMessage.im)
                    Just r ->
                        case Array.get ((Array.length r.messages) - 1) r.messages of
                            Nothing ->
                                { r | messages = Array.push chatMessage.im r.messages }
                            -- filter the same last message
                            Just msg ->
                                if msg.id == chatMessage.im.id
                                then r
                                else { r | messages = Array.push chatMessage.im r.messages }
            in
                ( { model | rooms = Dict.insert room.id room model.rooms }, Cmd.none )

        MessagePackAdded chatMessagePack ->
            let
                room = case Dict.get chatMessagePack.chatId model.rooms of
                    Just r -> { r | messages = Array.append (Array.fromList chatMessagePack.messages) r.messages }
                    Nothing -> Room chatMessagePack.chatId [] (Array.fromList chatMessagePack.messages)
                firstMessage = case Array.get 0 room.messages of
                    Nothing -> emptyIM
                    Just im -> im
                cmdList =
                    [ enableChatHistoryInfiniteScroll
                        { elementId = "chat-history"
                        , chatId = room.id
                        , lastMessageId = firstMessage.id
                        }
                    ]
                scrollCmd =
                    if Array.length room.messages <= 12
                    then scrollElementToEnd
                        { elementId = "chat-history"
                        , count = Array.length room.messages
                        }
                    else Cmd.none
            in
                ( { model | rooms = Dict.insert room.id room model.rooms }
                , Cmd.batch (scrollCmd :: cmdList) )

        SendMessage chatMessage ->
            ( { model | messageText = "" }, sendMessage chatMessage )

        RoomMetadataFetched roomMetadata ->
            let
                newRooms = case Dict.get roomMetadata.id model.rooms of
                    Just room -> Dict.insert roomMetadata.id { room | users = roomMetadata.users } model.rooms
                    Nothing -> model.rooms
                cmd = case Dict.get roomMetadata.id newRooms of
                    Just room ->
                        Cmd.batch
                            (List.filterMap (fetchMissingUser model.users) room.users)
                    Nothing -> Cmd.none
            in
                ( { model | rooms = newRooms }, cmd )

        WindowResized { width, height } ->
            ( { model | appWidth = width, appHeight = height }, Cmd.none )

        ChangeCityList citiesJSON ->
            let
                engToRus = case Json.Decode.decodeString citiesDecoder citiesJSON of
                    Ok value -> value
                    Err error -> Dict.empty
                cities = { engToRus = engToRus, list = Dict.values engToRus }
                --cities = { engToRus = dict, list = Dict.values dict }
            in
            --( { model
            --    | cities = { english = english, russian = russian }
            --    , filteredCityList = russian }
            ( { model
                | cities = cities
                , filteredCityList = cities.list }
            , Cmd.none )


citiesDecoder : Decoder (Dict.Dict String String)
citiesDecoder =
    Json.Decode.dict Json.Decode.string


fetchMissingUser : (Dict.Dict String User) -> String -> Maybe (Cmd Msg)
fetchMissingUser users userId =
    if Dict.member userId users
    then Nothing
    else Just (fetchUser { id = userId, purpose = "openChatPage" })


updateNotificationAsRead : Notification -> Notification
updateNotificationAsRead notification =
    if not notification.read
    then { notification | read = True }
    else notification


replaceCard : Card -> Card -> Card
replaceCard newCard oldCard =
    if newCard.id == oldCard.id
    then newCard
    else oldCard
