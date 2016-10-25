port module Main exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick, onInput, keyCode, on)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=), at, string)
import Navigation
import UrlParser exposing (Parser, (</>), format, int, oneOf, s, string)
import String


main =
    Navigation.program (Navigation.makeParser hashParser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


-- URL PARSERS


type Page
    = PageHome
    | PageCards
    | PageCard String
    | PagePrizes
    | PagePrize String
    | PageUser String


toHash : Page -> String
toHash page =
    case page of
        PageHome ->
            "#/home"

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


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 2 location.hash)


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format PageHome (UrlParser.s "home")
        , format PageCards (UrlParser.s "cards")
        , format PageCard (UrlParser.s "card" </> UrlParser.string)
        , format PagePrizes (UrlParser.s "prizes")
        , format PagePrize (UrlParser.s "prize" </> UrlParser.string)
        , format PageUser (UrlParser.s "user" </> UrlParser.string)
        ]


-- MODEL


type alias Model =
    { page : Page
    , loggedIn : Bool
    , title : String
    , cardText : String
    , place : String
    , user : User
    , cards : (List Card)
    , activeCard : Card
    , activeCardVolunteers : (List User)
    , activeUser : User
    , userTakenCards : (List Card)
    , karma : (String, Int)
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
    , karma : Int
    , place : String
    , title : String
    , body : String
    , assignedTo : String
    }


init : Result String Page -> (Model, Cmd Msg)
init result =
    urlUpdate result
        { page = PageHome
        , loggedIn = False
        , title = ""
        , cardText = ""
        , place = ""
        , user = (User "" "" "" "" 0 False)
        , cards = []
        , activeCard = (Card "" "" "" "" 0 0 "" "" "" "")
        , activeCardVolunteers = []
        , activeUser = (User "" "" "" "" 0 False)
        , userTakenCards = []
        , karma = ("", 0)
        }


-- UPDATE

type Msg
    = NoOp
    | CardText String
    | Login
    | Logout
    | SetUser User
    | CreateCard
    | ShowCards (List Card)
    | AddCardToList Card
    | ShowCard Card
    | SetActiveUser User
    | ShowUserTakenCards (List Card)
    | UpdateKarma String String String
    | TakeCard User Card
    | RemoveCard Card
    | ShowVolunteers (List User)
    | HandleRemoveCard Card
    | AssignVolunteer Card User


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CardText text ->
            ( { model | cardText = text }, Cmd.none )

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
            ( { model | cardText = "" }, createCard 
                { id = ""
                , authorId = model.user.uid
                , authorName = model.user.name
                , authorPhotoURL = model.user.photoURL
                , creationTime = 0
                , karma = 0
                , place = model.place
                , title = model.title
                , body = model.cardText
                , assignedTo = ""
                } )

        ShowCards cards ->
            ( { model | cards = (List.reverse cards) }, Cmd.none )

        AddCardToList card ->
            case List.head model.cards of
                Just c ->
                    if card.creationTime > c.creationTime
                        then ( { model | cards = card :: model.cards }, Cmd.none )
                        else ( model, Cmd.none )
                Nothing ->
                    ( { model | cards = card :: model.cards }, Cmd.none )

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

        AssignVolunteer card user ->
            ( model, assignVolunteer { card = card, user = user } )




urlUpdate : Result String Page -> Model -> (Model, Cmd Msg)
urlUpdate result model =
    case Debug.log "result" result of
        Err _ ->
            ( model, Navigation.modifyUrl (toHash model.page) )

        Ok page ->
            ( { model | page = page }, fetchData page )


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
port assignVolunteer : { card: Card, user: User } -> Cmd msg


-- SUBSCRIPTIONS

port authStateChanged : (User -> msg) -> Sub msg
port showCards : ((List Card) -> msg) -> Sub msg
port addCardToList : (Card -> msg) -> Sub msg
port cardFetched : (Card -> msg) -> Sub msg
port cardVolunteersFetched : ((List User) -> msg) -> Sub msg
port userFetched : (User -> msg) -> Sub msg
port userTakenCardsFetched : ((List Card) -> msg) -> Sub msg
port cardRemoved : (Card -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ authStateChanged SetUser
        , showCards ShowCards
        , addCardToList AddCardToList
        , cardFetched ShowCard
        , cardVolunteersFetched ShowVolunteers
        , userFetched SetActiveUser
        , userTakenCardsFetched ShowUserTakenCards
        , cardRemoved HandleRemoveCard
        ]


fetchData : Page -> Cmd Msg
fetchData page =
    case page of
        PageHome ->
            fetchStreamCards "TODO port should recieve at least 1 arg :/"

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
                [ fetchUser id
                , fetchUserCards id
                , fetchUserTakenCards id
                ]
            


-- VIEW

view : Model -> Html Msg
view model =
    div []
    [ div [ id "topbar" ]
        [ a [ href (toHash PageHome) ]
            [ img [ class "topbar__logo", src "besokind.jpg", width 48, height 48 ] [] ]
        , a [ href (toHash (PageUser model.user.uid)) ]
            [ img [ class "topbar__user-photo", src model.user.photoURL, width 48, height 48 ] [] ]
        , if model.loggedIn
            then button [ class "topbar__logout-btn", onClick Logout ] [ text "Выйти" ]
            else button [ class "topbar__login-btn", onClick Login ] [ text "Войти" ]
        ]
    , div [ id "container" ]
        [ if model.loggedIn
            then viewCreateCard model
            else div [] []
        , viewPage model
        ]
    ]


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        PageHome ->
            viewCards model model.cards

        PageCards ->
            viewCards model model.cards

        PageCard id ->
            viewCardFull model model.activeCard

        PagePrizes ->
            viewCards model model.cards

        PagePrize id ->
            viewCards model model.cards

        PageUser id ->
            div []
            [ viewProfile model.activeUser
            , div [] [ text "Дела пользователя:" ]
            , viewCards model model.cards
            , div [] [ text "Волонтер в делах:" ]
            , viewCards model model.userTakenCards
            ]


viewCreateCard : Model -> Html Msg
viewCreateCard model =
    div []
    [ textarea
        [ placeholder "Какая помощь вам требуется?"
        , onInput CardText
        , value model.cardText ]
        []
    , button [ onClick CreateCard ] [ text "Создать дело" ]
    ]


viewCards : Model -> (List Card) -> Html Msg
viewCards model cards =
    ul [] (List.map (viewCard model) cards)


viewCard : Model -> Card -> Html Msg
viewCard model card =
    li [ class (if not (String.isEmpty card.assignedTo) then "_assigned" else "") ]
    [ div [ class "list-card-header" ]
        [ img [ src card.authorPhotoURL, width 48, height 48 ] []
        , viewLink (PageCard card.id) (toString card.creationTime)
        , viewLink (PageUser card.authorId) card.authorName
        ]
    , div [ class "list-card-title"] [ text card.title ]
    , div [ class "list-card-body"] [ text card.body ]
    , viewCardKarmaPrice "list" model card
    ]


viewCardKarmaPrice : String -> Model -> Card -> Html Msg
viewCardKarmaPrice loc model card =
    div [ class (loc ++ "-card-karma") ]
        [ span [] [ text "Карма:" ]
        , span
            [ contenteditable model.user.moderator
            , on "blur" (Json.map (UpdateKarma card.authorId card.id) textContentDecoder)
            ]
            [ text (toString card.karma) ]
        ]


viewCardFull : Model -> Card -> Html Msg
viewCardFull model card =
    div []
    [ div [ class "full-card-header" ]
        [ img [ src card.authorPhotoURL, width 48, height 48 ] []
        , viewLink (PageCard card.id) (toString card.creationTime)
        , span [] [ text card.authorName ]
        ]
    , div [ class "full-card-title"] [ text card.title ]
    , div [ class "full-card-body"] [ text card.body ]
    , viewCardKarmaPrice "full" model card
    , div []
        [ span [] [ text "Желающие помочь:" ]
        , ul [] (List.map (viewVolunteer card model.user) model.activeCardVolunteers)
        ]
    , if not (String.isEmpty model.user.uid) && model.user.uid /= card.authorId
        then button [ onClick (TakeCard model.user card) ] [ text "Помочь" ]
        else span [] []
    , if not (String.isEmpty model.user.uid) && model.user.uid == card.authorId
        then button [ onClick (RemoveCard card) ] [ text "Удалить" ]
        else span [] []
    ]


viewVolunteer : Card -> User -> User -> Html Msg
viewVolunteer card currentUser volunteer =
    li []
        [ img [ src volunteer.photoURL, width 48, height 48 ] []
        , viewLink (PageUser volunteer.uid) volunteer.name
        , if (String.isEmpty card.assignedTo)
            && not (String.isEmpty currentUser.uid)
            && currentUser.uid == card.authorId
            then button [ onClick (AssignVolunteer card volunteer) ] [ text "Принять помощь" ]
            else span [] []
        ]


viewProfile : User -> Html Msg
viewProfile user =
    div []
    [ img [ src user.photoURL ] []
    , span [] [ text user.name ]
    , span [] [ text (toString user.karma) ]
    ]


viewLink : Page -> String -> Html msg
viewLink page description =
  a [ style [ ("padding", "0 20px") ], href (toHash page) ] [ text description ]


textContentDecoder : Json.Decoder String
textContentDecoder =
    at ["target", "textContent"] Json.string
