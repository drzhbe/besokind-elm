module Update exposing (update)
import Set
import Dict
import Navigation

import Types exposing (..)
import Ports exposing (..)
import Pages exposing (toHash, getPageByLocation, updatePage)
import Data exposing (fetchDataForPage)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        HandleUrlChange location ->
            updatePage model (getPageByLocation location)

        SetPage page ->
            ( model, Navigation.newUrl <| toHash page )

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
            ( { model | cardText = "" }
            , Cmd.batch
                [ createCard
                    { id = ""
                    , authorId = model.user.uid
                    , authorName = model.user.name
                    , authorPhotoURL = model.user.photoURL
                    , creationTime = 0
                    , creationTimeFriendly = ""
                    , karma = 0
                    , place = model.place
                    , title = model.title
                    , body = model.cardText
                    , assignedTo = ""
                    }
                , persistCardText ""
                ]
            )

        ShowCards cards ->
            ( { model | cards = (List.reverse cards) }, Cmd.none )

        ShowUserCards cards ->
            ( { model | userCards = (List.reverse cards) }, Cmd.none )

        AddCardToList card ->
            case List.head model.cards of
                Just c ->
                    if card.creationTime > c.creationTime
                        then ( { model | cards = card :: model.cards }, Cmd.none )
                        else ( model, Cmd.none )
                Nothing ->
                    ( { model | cards = card :: model.cards }, Cmd.none )

        UpdateCard card ->
            ( { model
                | cards = List.map (replaceCard card) model.cards
                , activeCard = replaceCard card model.activeCard
            }, Cmd.none )

        ShowCard card ->
            ( { model | activeCard = card }, Cmd.none )

        ShowVolunteers users ->
            ( { model | activeCardVolunteers = users }, Cmd.none )

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

        ShowProfileMenuPopup ->
            ( { model | popup = ProfileMenuPopup }, Cmd.none )

        ShowNotificationsPopup ->
            let
                notReadNotificationIdList = Dict.keys <| Dict.filter (\key notification -> not notification.read) model.notifications
            in
                ( { model
                    | popup = NotificationsListPopup
                    , notifications = Dict.map updateNotificationAsRead model.notifications
                }, markNotificationsAsRead { userId = model.user.uid, notificationIdList = notReadNotificationIdList } )

        HidePopup ->
            if model.popup == NoPopup
            then ( model, Cmd.none )
            else ( { model | popup = NoPopup }, Cmd.none )

        AddNotification notification ->
            ( { model
                | notifications = Dict.insert notification.id notification model.notifications }
            , Cmd.none )

        RemoveNotification notificationId ->
            ( { model
                | notifications = Dict.remove notificationId model.notifications }
            , Cmd.none )

            -- TODO not used
        --MarkNotificationsAsRead notificationIdList ->
        --    ( { model
        --        | notifications = Dict.map updateNotificationAsRead model.notifications }
        --    , markNotificationsAsRead { userId = model.user.uid, notificationIdList = notificationIdList } )

        RoomAdded room ->
            ( { model | rooms = room :: model.rooms }, Cmd.none )

        AddOnlineUser userId ->
            ( { model
                | usersOnline = Set.insert (Debug.log "userId came online" userId) model.usersOnline }
            , Cmd.none )

        RemoveOnlineUser userId ->
            ( { model
                | usersOnline = Set.remove (Debug.log "userId went offline" userId) model.usersOnline }
            , Cmd.none )

        ShowRoom room ->
            ( { model | activeRoom = room }, Cmd.none )

        MessageAdded chatMessage ->
            let
                room = model.activeRoom
                newActiveRoom =
                    if room.id == chatMessage.chatId 
                    then { room | messages = chatMessage.im :: room.messages }
                    else room
            in
                ( { model | activeRoom = newActiveRoom }, Cmd.none )

        SendMessage chatMessage ->
            ( model, sendMessage chatMessage )



updateNotificationAsRead : String -> Notification -> Notification
updateNotificationAsRead notificationId notification =
    if not notification.read
    then { notification | read = True }
    else notification


replaceCard : Card -> Card -> Card
replaceCard newCard oldCard =
    if newCard.id == oldCard.id
    then newCard
    else oldCard
