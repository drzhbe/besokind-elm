module Types exposing (..)
import Time exposing (Time)
import Set
import Dict
import Array
import Navigation

type Page
    = PageHome
    | PageNotFound
    | PageCards
    | PageCard String
    | PagePrizes
    | PagePrize String
    | PageUser String
    | PageChatList
    | PageChat String


type alias Model =
    { time : Time
    , page : Page
    , loggedIn : Bool
    , title : String
    , cardText : String
    , cardInputFocus : Bool
    , messageText : String
    , messageInputFocus : Bool
    , place : String
    , user : User
    , cards : (List Card)
    , userCards : (List Card)
    , activeCard : Card
    , activeRoomId : String
    , activeCardVolunteers : (List User)
    , activeUser : User
    , userTakenCards : (List Card)
    , karma : (String, Int)
    , popup : Popup
    , notifications : (Dict.Dict String Notification)
    , usersOnline : (Set.Set String)
    , users : (Dict.Dict String User)
    , rooms : (Dict.Dict String Room)
    }


type alias User =
    { uid : String
    , name : String
    , email : String
    , photoURL : String
    , karma: Int
    , moderator : Bool
    }

emptyUser : User
emptyUser =
    User "" "" "" "" 0 False


type alias Card =
    { id : String
    , authorId : String
    , authorName : String
    , authorPhotoURL : String
    , creationTime : Float
    , creationTimeFriendly : String
    , karma : Int
    , place : String
    , title : String
    , body : String
    , assignedTo : String
    }


type alias IM =
    { id : String
    , userId : String
    , text : String
    , date : Int
    }


type alias ChatMessage =
    { chatId : String
    , im : IM
    }


type alias Room =
    { id : String
    , users : (List String)
    , messages : (Array.Array IM)
    }


type alias RoomMetadata =
    { id : String
    , users : (List String)
    }


type alias Notification =
    { id : String
    , name : String
    , read : Bool
    , userId : String
    , cardId : String
    , cardAuthorId: String
    , userName : String
    }


type Popup
    = NoPopup
    | ProfileMenuPopup
    | NotificationsListPopup


type Msg
    = NoOp
    | NewTime Time
    | HandleUrlChange Navigation.Location
    | SetPage Page
    | SetCardText String
    | CardInputFocus Bool
    | SetMessageText String
    | MessageInputFocus Bool
    | Login
    | Logout
    | SetUser User
    | CreateCard
    | ShowCards (List Card)
    | ShowUserCards (List Card)
    | AddCardToList Card
    | UpdateCard Card
    | ShowCard Card
    | SetActiveUser User
    | UserFetched User
    | ShowUserTakenCards (List Card)
    | UpdateKarma String String String
    | TakeCard User Card
    | RemoveCard Card
    | ShowVolunteers (List User)
    | HandleRemoveCard Card
    | AssignVolunteer Card User String
    | ShowProfileMenuPopup
    | ShowNotificationsPopup
    | HidePopup
    | AddNotification Notification
    | RemoveNotification String
    --| MarkNotificationsAsRead (List String)
    | RoomAdded Room
    | AddOnlineUser String
    | RemoveOnlineUser String
    | ShowRoom Room
    | MessageAdded ChatMessage
    | SendMessage ChatMessage
    | RoomMetadataFetched RoomMetadata
