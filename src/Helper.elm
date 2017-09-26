module Helper exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, targetValue, keyCode, on, onWithOptions, Options)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (at, string, value, succeed)

import Types exposing (..)
import Pages exposing (toHash)


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault clickHandler =
    onWithOptions
        "click"
        (Options True True)
        (Json.succeed
            clickHandler)


onClickShowPopup : Model -> Msg -> Attribute Msg
onClickShowPopup model msg =
    onClickPreventDefault
        (if model.popup /= NoPopup
            then (HidePopup NoOp)
            else msg)


viewLink : Page -> String -> Html Msg
viewLink page description =
  a [ href (toHash page)
    , onClickPreventDefault (SetPage page)
    ]
    [ text description ]


innerHTMLDecoder : Json.Decoder String
innerHTMLDecoder =
    Json.at ["target", "innerHTML"] Json.string


textContentDecoder : Json.Decoder String
textContentDecoder =
    Json.at ["target", "textContent"] Json.string
