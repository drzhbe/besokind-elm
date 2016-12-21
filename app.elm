port module Main exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, keyCode, on, onWithOptions, Options)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (at, string, value, succeed)
import Navigation
import UrlParser as Url exposing (Parser, (</>), int, oneOf, s, string, map)
import String


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- URL PARSERS


type Page
    = PageHome
    | PageNotFound
    | PageCards
    | PageCard String
    | PagePrizes
    | PagePrize String
    | PageUser String


ensurePage : Maybe Page -> Page
ensurePage page =
    case page of
        Nothing ->
            PageNotFound
        Just page ->
            page

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


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ Url.map PageHome (Url.s "home")
        , Url.map PageCards (Url.s "cards")
        , Url.map PageCard (Url.s "card" </> Url.string)
        , Url.map PagePrizes (Url.s "prizes")
        , Url.map PagePrize (Url.s "prize" </> Url.string)
        , Url.map PageUser (Url.s "user" </> Url.string)
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
    , popups : (List Popup)
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


type Popup
    = ProfileMenu


init : Navigation.Location -> (Model, Cmd Msg)
init location =
    let
        page = defaultPage (ensurePage (Url.parseHash pageParser location))
    in
        (
            { page = page
            , loggedIn = False
            , title = ""
            , cardText = ""
            , place = ""
            , user = (User "" "" "" "" 0 False)
            , cards = []
            , activeCard = (Card "" "" "" "" 0 "" 0 "" "" "" "")
            , activeCardVolunteers = []
            , activeUser = (User "" "" "" "" 0 False)
            , userTakenCards = []
            , karma = ("", 0)
            , popups = []
            }
        , fetchData page
        )


defaultPage : Page -> Page
defaultPage page =
    if page == PageNotFound
    then PageHome
    else page


-- UPDATE

type Msg
    = NoOp
    | UrlChange Navigation.Location
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
    | ShowProfileMenuPopup
    | HideAllPopups


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChange location ->
            let
                page = ensurePage (Url.parseHash pageParser location)
            in
                ( { model | page = page }
                , fetchData page
                )

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
                , creationTimeFriendly = ""
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

        ShowProfileMenuPopup ->
            ( { model | popups = Debug.log "show profile" ProfileMenu :: model.popups }, Cmd.none )

        HideAllPopups ->
            if List.isEmpty model.popups
            then ( model, Cmd.none )
            else ( { model | popups = [] }, Cmd.none )


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

        PageNotFound ->
            Cmd.none

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

-- coral
brandColor : String
brandColor = "#f2836b"
-- almbost black
darkestColor : String
darkestColor = "#333"
-- gray
secondaryColor : String
secondaryColor = "#555"
-- lightest gray
lightestColor : String
lightestColor = "#999"
-- blue
linkColor : String
linkColor = "#1da1f2"


viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        PageHome ->
            viewCards model model.cards

        PageNotFound ->
            -- TODO: сделать 404
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
            [ viewProfile model.loggedIn model.activeUser
            , div [] [ text "Дела пользователя:" ]
            , viewCards model model.cards
            , div [] [ text "Волонтер в делах:" ]
            , viewCards model model.userTakenCards
            ]


view : Model -> Html Msg
view model =
    div [ onClick HideAllPopups ]
    [ div
        [ id "topbar"
        , style
            [ ("position", "fixed")
            , ("top", "0")
            , ("left", "0")
            , ("right", "0")
            , ("height", "46px")
            , ("background", brandColor)
            , ("border-radius", "0 0 8px 8px")
            , ("visible", "false")
            ]
        ]
        [ div
            [ id "topbar-content", style [ ("width", "720px"), ("margin", "0 auto") ] ]
            [ if model.loggedIn
                then
                    div
                    [ style
                        [ ("position", "relative")
                        , ("float", "right")
                        , ("width", "46px")
                        ]
                    ]
                    [ img
                        [ onWithOptions
                            "click"
                            (Options True True)
                            (Json.succeed
                                (if (List.length model.popups) > 0
                                    then HideAllPopups
                                    else ShowProfileMenuPopup))
                        , class "topbar__user-photo"
                        , src model.user.photoURL
                        , width 38
                        , height 38
                        , style
                            [ ("border-radius", "100%")
                            , ("margin", "4px")
                            ]
                        ]
                        []
                    , if (List.member ProfileMenu model.popups)
                        then viewProfileMenu model
                        else span [] []
                    ]
                else
                    div
                        [ class "topbar__login-btn"
                        , onClick Login
                        , style
                            [ ("margin", "4px 10px 4px 4px")
                            , ("color", "white")
                            , ("font-weight", "500")
                            , ("height", "38px")
                            , ("line-height", "38px")
                            , ("text-align", "center")
                            ]
                        ]
                        [ text "Войти" ]
            ]
        ]
    , div
        [ id "page-container"
        , style [ ("width", "720px"), ("margin", "0 auto") ] ]
        [ div
            [ id "nav"
            , width 120
            , style
                [ ("position", "fixed")
                , ("width", "120px")
                , ("margin", "0 20px") -- margin top = topbar height + 10px
                , ("float", "left")
                ]
            ]
            [ a [ href (toHash PageHome) ] [ text "Дела" ]
            , div [] [ text "Призы" ]
            , div [] [ text "Рейтинг" ]
            ]
        , div
            [ id "content-cointainer"
            , style
                [ ("margin", "56px 0 0 160px") -- nav's width + left + right margin
                ]
            ]
            [ if model.loggedIn
                then viewCreateCard model
                else div [] []
            , viewPage model
            ]
        ]
    ]


viewCreateCard : Model -> Html Msg
viewCreateCard model =
    div []
    [ textarea
        [ placeholder "Какая помощь вам требуется?"
        , onInput CardText
        , Html.Attributes.value model.cardText ]
        []
    , button [ onClick CreateCard ] [ text "Создать дело" ]
    ]


viewCards : Model -> (List Card) -> Html Msg
viewCards model cards =
    ul [] (List.map (viewCard model) cards)


viewCard : Model -> Card -> Html Msg
viewCard model card =
    li
        [ class (if not (String.isEmpty card.assignedTo) then "_assigned" else "")
        , style [ ("margin-top", "15px") ]
        ]
        [ viewCardHeader card
        , div [ class "list-card-title" ] [ text card.title ]
        , div [ class "list-card-body", style [ ("margin-top", "10px") ] ] [ text card.body ]
        , viewCardKarmaPrice "list" model card
        ]


viewCardFull : Model -> Card -> Html Msg
viewCardFull model card =
    div []
    [ viewCardHeader card
    , div [ class "full-card-title"] [ text card.title ]
    , div [ class "full-card-body"] [ text card.body ]
    , viewCardKarmaPrice "full" model card
    , div []
        [ span [] [ text "Желающие помочь:" ]
        , ul [] (List.map (viewVolunteer card model.user) model.activeCardVolunteers)
        ]
    , if not (String.isEmpty model.user.uid)
        && model.user.uid /= card.authorId
        && not (List.member model.user model.activeCardVolunteers)
        then button [ onClick (TakeCard model.user card) ] [ text "Помочь" ]
        else span [] []
    , if not (String.isEmpty model.user.uid) && model.user.uid == card.authorId
        then button [ onClick (RemoveCard card) ] [ text "Удалить" ]
        else span [] []
    ]


viewCardHeader : Card -> Html Msg
viewCardHeader card =
    div [ class "list-card-header" ]
        [ a [ href (toHash (PageUser card.authorId)) ]
            [ img
                [ src card.authorPhotoURL
                , width 48, height 48
                , style [ ("float", "left"), ("border-radius", "4px") ]
                ] []
            ]
        , div
            [ style
                [ ("height", "40px")
                , ("padding-top", "8px")
                , ("margin-left", "58px")
                ]
            ]
            [ ul []
                [ li
                    [ style [ ("color", darkestColor) , ("font-weight", "500") ] ]
                    [ viewLink (PageUser card.authorId) card.authorName ]
                , li
                    [ style [ ("color", lightestColor) ] ]
                    [ viewLink (PageCard card.id) card.creationTimeFriendly ]
                ]
            ]
        ]

viewCardKarmaPrice : String -> Model -> Card -> Html Msg
viewCardKarmaPrice loc model card =
    div [ class (loc ++ "-card-karma"), style [ ("margin-top", "4px") ] ]
        [ span [ style [ ("color", lightestColor) ] ] [ text "Карма: " ]
        , span
            [ contenteditable model.user.moderator
            , on "blur" (Json.map (UpdateKarma card.authorId card.id) textContentDecoder)
            ]
            [ text (toString card.karma) ]
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


viewProfile : Bool -> User -> Html Msg
viewProfile loggedIn user =
    div []
    [ img [ src user.photoURL, width 200, height 200 ] []
    , span [] [ text user.name ]
    , span [] [ text (toString user.karma) ]
    , if loggedIn
        then button [ class "topbar__logout-btn", onClick Logout ] [ text "Выйти" ]
        else span [] []
    ]


viewProfileMenu : Model -> Html Msg
viewProfileMenu model =
    ul
        [ style
            [ ("position", "absolute")
            , ("right", "0")
            , ("top", "54px") -- 46 topbar + 8 margin
            , ("width", "auto")
            , ("background", "white")
            , ("margin", "0")
            , ("padding", "10px")
            , ("border-radius", "5px")
            , ("border", "1px solid #ddd")
            , ("white-space", "nowrap")
            ]
        ]
        [ li [] [ a [ href (toHash (PageUser model.user.uid)), class "light-btn" ] [ text "Открыть профиль" ] ]
        , li [ onClick Logout, class "light-btn" ] [ text "Выйти" ]
        ]


viewLink : Page -> String -> Html msg
viewLink page description =
  a [ href (toHash page) ] [ text description ]


textContentDecoder : Json.Decoder String
textContentDecoder =
    at ["target", "textContent"] Json.string
