module Types exposing (..)
import Set
import Dict
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
    { page : Page
    , loggedIn : Bool
    , title : String
    , cardText : String
    , cardInputFocus : Bool
    , place : String
    , user : User
    , cards : (List Card)
    , userCards : (List Card)
    , activeCard : Card
    , activeRoom : Room
    , activeCardVolunteers : (List User)
    , activeUser : User
    , userTakenCards : (List Card)
    , karma : (String, Int)
    , popup : Popup
    , notifications : (Dict.Dict String Notification)
    , rooms : (List Room)
    , usersOnline : (Set.Set String)
    }


type alias User =
    { uid : String
    , name : String
    , email : String
    , photoURL : String
    , karma: Int
    , moderator : Bool
    }


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
    , messages : (List IM)
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
    | HandleUrlChange Navigation.Location
    | SetPage Page
    | SetCardText String
    | CardInputFocus Bool
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
