module CityList exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Html.Attributes exposing (..)
import Dict

import Helper as H
import Types exposing (..)
import Style exposing (..)
--import Pages exposing (toHash)


viewCityListPopup : String -> Model -> Html Msg
viewCityListPopup target model =
    let
        hasError = List.length model.cities.list == 0
            || not (String.isEmpty model.filterCityListQuery)
                && List.length model.filteredCityList == 0

        errorText =
            if List.length model.cities.list == 0
            then div []
                [ span [] [ text "Список городов не загрузился. Видимо, сейчас во всем мире столько добра, что в нашем сайте нет необходимости! Позвоните близким, скажите, что любите их. А также можете попробовать обновить страницу." ]
                ]
            else
                if not (String.isEmpty model.filterCityListQuery)
                && List.length model.filteredCityList == 0
                then div []
                    [ span [] [ text "По запросу " ]
                    , span [ style [ ("color", grayColor) , ("font-weight", "bold") ] ]
                        [ text model.filterCityListQuery ]
                    , span [] [ text " не нашлось города. Напишите нам, если хотите чтобы " ]
                    , span [ style [ ("color", darkColor) , ("font-weight", "bold") ] ]
                        [ text "Будь Добр" ]
                    , span [] [ text " был в вашем славном городе" ]
                    ]
                else text ""

        positionLeft =
            if target == "topbar"
            then "-200px"
            else "-10px"

        positionTop =
            if target == "topbar"
            then "0"
            else "-10px"

        cityListFilterId = case target of
            "topbar" -> "topbar__citylist"
            "card-input" -> "card-input__citylist"
            _ -> ""
    in
        div [ style
                [ ("position", "absolute")
                , ("width", "300px")
                , ("left", positionLeft)
                , ("top", positionTop)
                , ("background", "white")
                , ("border", "1px solid #ddd")
                ]
            ]
            [ input
                [ id cityListFilterId
                , H.onClickPreventDefault NoOp
                , onInput SetFilterCityListText
                , placeholder "Введите название города"
                , autofocus True
                , width 100
                --, Html.Events.onFocus (FilterCityListInputFocus True)
                --, Html.Events.onBlur (FilterCityListInputFocus False)
                , style
                    [ ("margin", "10px 10px 5px 10px")
                    , ("width", "260px")
                    , ("border", "0")
                    , ("outline", "0")
                    , ("font-size", "14px")
                    , ("line-height", "20px")
                    ]
                ]
                []
            , div
                [ style
                    [ ("display", if hasError then "block" else "none")
                    , ("padding", "10px")
                    ]
                ]
                [ errorText ]
            , ul
                [ style
                    [ ("max-height", toString (model.appHeight - 150) ++ "px" )
                    , ("overflow-y", "scroll")
                    , ("-webkit-overflow-scrolling", "touch")
                    ]
                ]
                (List.map (viewCityListItem (cityListItemAction target) model.filterCityListQuery) model.filteredCityList)
            ]


viewCityListItem : (String -> Msg) -> String -> String -> Html Msg
viewCityListItem action query city =
    -- TODO highlight with query
    li 
        [ onClick (action city)
        , class "city-list-item"
        , style [ ("padding", "5px 10px 5px 10px") ]
        ]
        [ text city ]


cityListItemAction : String -> String -> Msg
cityListItemAction target city =
    case target of
        "topbar" -> SetCity city
        "card-input" -> SetCardCity city
        _ -> NoOp