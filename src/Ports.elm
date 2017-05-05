port module Ports exposing (..)

import Types exposing (..)

-- OUTGOING PORTS

port login : String -> Cmd msg
port logout : String -> Cmd msg
port createCard : Card -> Cmd msg
port fetchCard : String -> Cmd msg
port fetchCardVolunteers : String -> Cmd msg
port fetchStreamCards : String -> Cmd msg
port watchStreamCards : String -> Cmd msg
port fetchUserCards : String -> Cmd msg
-- purpose : "openUserPage" | "openChatPage"
port fetchUser : { id: String, purpose: String } -> Cmd msg
port fetchUserTakenCards : String -> Cmd msg
port updateKarma : { authorId : String, cardId : String, karma : Int } -> Cmd msg
port takeCard : { user : User, card : Card } -> Cmd msg
port removeCard : Card -> Cmd msg
port assignVolunteer : { card : Card, user : User, userName : String } -> Cmd msg
port confirmHelp : Card -> Cmd msg
port persistCardText : String -> Cmd msg
port markNotificationsAsRead : { userId : String, notificationIdList : List String } -> Cmd msg
port fetchChatMessages : { chatId : String, lastMessageId : String } -> Cmd msg
--port watchChatMessages : String -> Cmd msg
--port unwatchChatMessages : String -> Cmd msg
port sendMessage : ChatMessage -> Cmd msg
port fetchRoomMetadata : String -> Cmd msg
-- id of DOMNode; count of children should be when scroll to fire
port scrollElementToEnd : { elementId : String, count : Int } -> Cmd msg
-- id of DOMNode
port enableCardStreamInfiniteScroll : { elementId : String, lastCardId : String } -> Cmd msg
port enableChatHistoryInfiniteScroll : { elementId : String, chatId : String, lastMessageId : String } -> Cmd msg

-- SUBSCRIPTIONS

port authStateChanged : (User -> msg) -> Sub msg
port showCards : ((List Card) -> msg) -> Sub msg
port userCardsFetched : ((List Card) -> msg) -> Sub msg
port addCardToList : (Card -> msg) -> Sub msg
port addCardsToList : ((List Card) -> msg) -> Sub msg
port updateCard : (Card -> msg) -> Sub msg
port cardFetched : (Card -> msg) -> Sub msg
port cardVolunteersFetched : ((List String) -> msg) -> Sub msg
port activeUserFetched : (User -> msg) -> Sub msg
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
port chatMessagesFetched : (ChatMessagePack -> msg) -> Sub msg
port roomMetadataFetched : (RoomMetadata -> msg) -> Sub msg
port windowResized : ({ width : Int, height : Int } -> msg) -> Sub msg
port clickedSomewhere : (String -> msg) -> Sub msg
port escPressed : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authStateChanged SetUser
        , showCards ShowCards
        , userCardsFetched ShowUserCards
        , addCardToList AddCardToList
        , addCardsToList AddCardsToList
        , updateCard UpdateCard
        , cardFetched ShowCard
        , cardVolunteersFetched ShowVolunteers
        , activeUserFetched SetActiveUser
        , userFetched UserFetched
        , userTakenCardsFetched ShowUserTakenCards
        , cardRemoved HandleRemoveCard
        , cardTextFetched SetCardText
        , notificationAdded AddNotification
        , roomAdded RoomAdded
        , onlineUserAdded AddOnlineUser
        , onlineUserRemoved RemoveOnlineUser
        , messageAdded MessageAdded
        , chatMessagesFetched MessagePackAdded
        , roomMetadataFetched RoomMetadataFetched
        , windowResized WindowResized
        , clickedSomewhere (PortWithNoArgs (HidePopup NoOp))
        , escPressed (PortWithNoArgs (HidePopup NoOp))
        ]
