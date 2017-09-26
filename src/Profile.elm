module Profile exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (..)
import Dict

import Types exposing (..)
import Style exposing (..)
import Pages exposing (toHash)


viewProfile : Bool -> User -> Html Msg
viewProfile loggedIn user =
    div [ style [ ("position", "relative"), ("margin-top", "10px") ] ]
    [ img [ src user.photoURL, width 200, height 200 ] []
    , ul
        [ style
            [ ("position", "absolute")
            , ("margin-left", "10px")
            , ("display", "inline-block")
            ]
        ]
        [ li [] [ text user.name ]
        , li []
            [ span [ style [ ("color", grayColor) ] ] [ text "Карма: " ]
            , span [] [ text (toString user.karma) ]
            ]
        ]
    ]


viewProfileMenuPopup : Model -> Html Msg
viewProfileMenuPopup model =
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