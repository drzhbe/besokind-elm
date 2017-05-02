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
    , appHeight : Int
    , loggedIn : Bool
    , title : String
    , cardText : String
    , cardInputFocus : Bool
    , messageText : String
    , messageInputFocus : Bool
    , messageInputHeight : Int
    , place : String
    , user : User
    , cards : (List Card)
    , userCards : (List Card)
    , activeRoomId : String
    , activeCard : Card
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


emptyCard : Card
emptyCard =
    Card "" "" "" "" 0 "" 0 "" "" "" ""


type alias IM =
    { id : String
    , userId : String
    , text : String
    , date : Int
    }


emptyIM : IM
emptyIM =
    IM "" "" "" 0


type alias ChatMessage =
    { chatId : String
    , im : IM
    }


type alias ChatMessagePack =
    { chatId : String
    , messages : List IM
    }


type alias Room =
    { id : String
    , users : (List String)
    -- why Array, but not List?
    -- we use "prev" – get by index-1 and push to an end of array
    -- in List we can check "member" to deduplicate, in Array we can't
    -- but it is better to know some index and to check our last index in model
    , messages : (Array.Array IM)
    }


emptyRoom : Room
emptyRoom =
    Room "" [] Array.empty


type alias RoomMetadata =
    { id : String
    , users : (List String)
    }

type alias RoomMessages =
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
    | AddCardsToList (List Card)
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
    | SendMessage ChatMessage
    | MessageAdded ChatMessage
    | MessagePackAdded ChatMessagePack
    | RoomMetadataFetched RoomMetadata
    | WindowResized Int
