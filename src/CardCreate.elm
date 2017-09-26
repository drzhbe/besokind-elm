module CardCreate exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Html.Attributes exposing (..)
import Dict

import Helper as H
import Types exposing (..)
import Style exposing (..)
import Pages exposing (toHash)
import CityList


viewCreateCard : Model -> Html Msg
viewCreateCard model =
    let
        emptyText = String.isEmpty model.cardText
        expandedTextArea = not emptyText || model.cardInputFocus
        createButtonActivityColors =
            if emptyText
            then [ ("background", brandLighterColor), ("color", grayLightestColor) ]
            else [ ("background", brandColor), ("color", "white") ]
        sendAction =
            if model.loggedIn
            then CreateCard
            else Login
        city =
            if not (String.isEmpty model.cardCity)
            then case Dict.get model.cardCity model.cities.engToRus of
                Just c -> c
                Nothing -> model.cardCity
            else
                if String.isEmpty model.user.city
                then "Город"
                else case Dict.get model.user.city model.cities.engToRus of
                    Just c -> c
                    Nothing -> model.user.city
    in
        div [ style
                [ ("min-height", "50px")
                , ("padding", "10px")
                , ("background", brandLightestColor)
                , ("border-radius", "4px")
                ]
            ]
            [ a [ href (toHash (PageUser model.user.uid)) ]
                [ img
                    [ src model.user.photoURL
                    , width 48, height 48
                    , style [ ("float", "left"), ("border-radius", "4px") ]
                    ] []
                ]
            , div [ style [ ("padding", "0 8px 0 56px") ] ]
                [ textarea
                    [ class "card-input"
                    , placeholder "Какая помощь вам требуется?"
                    , Html.Attributes.value model.cardText
                    , rows (if expandedTextArea then 4 else 2)
                    , onInput SetCardText
                    , Html.Events.onFocus (CardInputFocus True)
                    --, Html.Events.onBlur (CardInputFocus False)
                    , style
                        [ ("outline", "none")
                        , ("border", "0")
                        , ("resize", "none")
                        , ("font-size", "14px")
                        , ("width", "100%")
                        , ("padding", "8px 0 4px 8px")
                        , ("border", "1px solid " ++ brandLighterColor)
                        ]
                    ] []
                ]
            , div
                [ style
                    [ ("display", (if expandedTextArea then "block" else "none"))
                    , ("height", "40px")
                    ]
                ]
                [ div
                    [ style
                        [ ("position", "relative")
                        , ("display", "inline-block")
                        , ("margin", "10px 0 0 66px")
                        , ("z-index", "2")
                        ]
                    ]
                    [ span
                        [ H.onClickShowPopup model (ShowCityListPopup "card-input")
                        ]
                        [ text city ]
                    , case model.popup of
                        CityListPopup target -> if target == "card-input"
                            then CityList.viewCityListPopup target model
                            else text ""
                        _ -> text ""
                    ]
                , div
                    [ onClick sendAction
                    , style (
                        [ ("float", "right")
                        , ("margin", "4px 6px 0 0")
                        , ("padding", "10px 20px")
                        , ("border-radius", "4px")
                        , ("max-width", "100px")
                        , ("cursor", (if emptyText then "default" else "pointer"))
                        ] ++ createButtonActivityColors)
                    ]
                    [ text "Создать дело" ]
                ]
            ]