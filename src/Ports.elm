port module Ports exposing (..)

import Types exposing (..)

-- OUTGOING PORTS

port login : String -> Cmd msg
port logout : String -> Cmd msg
port createCard : Card -> Cmd msg
port fetchCard : String -> Cmd msg
port fetchCardVolunteers : String -> Cmd msg
port fetchStreamCards : String -> Cmd msg
port fetchUserCards : String -> Cmd msg
port fetchUser : String -> Cmd msg
port fetchUserTakenCards : String -> Cmd msg
port updateKarma : {authorId : String, cardId : String, karma : Int} -> Cmd msg
port takeCard : { user : User, card : Card } -> Cmd msg
port removeCard : Card -> Cmd msg
port assignVolunteer : { card : Card, user : User, userName : String } -> Cmd msg
port persistCardText : String -> Cmd msg
port removeNotification : String -> Cmd msg
port markNotificationsAsRead : { userId : String, notificationIdList : List String } -> Cmd msg
port watchChat : String -> Cmd msg
port unwatchChat : String -> Cmd msg
port sendMessage : ChatMessage -> Cmd msg

-- SUBSCRIPTIONS

port authStateChanged : (User -> msg) -> Sub msg
port showCards : ((List Card) -> msg) -> Sub msg
port userCardsFetched : ((List Card) -> msg) -> Sub msg
port addCardToList : (Card -> msg) -> Sub msg
port updateCard : (Card -> msg) -> Sub msg
port cardFetched : (Card -> msg) -> Sub msg
port cardVolunteersFetched : ((List User) -> msg) -> Sub msg
port userFetched : (User -> msg) -> Sub msg
port userTakenCardsFetched : ((List Card) -> msg) -> Sub msg
port cardRemoved : (Card -> msg) -> Sub msg
port cardTextFetched : (String -> msg) -> Sub msg
port notificationAdded : (Notification -> msg) -> Sub msg
port notificationRemoved : (String -> msg) -> Sub msg
port roomAdded : (Room -> msg) -> Sub msg
port onlineUserAdded : (String -> msg) -> Sub msg
port onlineUserRemoved : (String -> msg) -> Sub msg
port messageAdded : (ChatMessage -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authStateChanged SetUser
        , showCards ShowCards
        , userCardsFetched ShowUserCards
        , addCardToList AddCardToList
        , updateCard UpdateCard
        , cardFetched ShowCard
        , cardVolunteersFetched ShowVolunteers
        , userFetched SetActiveUser
        , userTakenCardsFetched ShowUserTakenCards
        , cardRemoved HandleRemoveCard
        , cardTextFetched SetCardText
        , notificationAdded AddNotification
        , notificationRemoved RemoveNotification
        , roomAdded RoomAdded
        , onlineUserAdded AddOnlineUser
        , onlineUserRemoved RemoveOnlineUser
        , messageAdded MessageAdded
        ]
