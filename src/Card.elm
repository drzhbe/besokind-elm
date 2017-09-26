module Card exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput, on)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing (at, string, value, succeed)
import Dict
import Set

import Helper as H
import Types exposing (..)
import Style exposing (..)
import Pages exposing (toHash)


viewCards : Model -> (List Card) -> Html Msg
viewCards model cards =
    div []
        [ if List.length cards == 0
            then div []
                [ text "В вашем городе пока никто не просит о помощи. Все добрые дела сделаны!"
                ]
            else ul
                [ id "card-stream"
                , style
                    [ ("overflow", "scroll")
                    , ("-webkit-overflow-scrolling", "touch")
                    -- TODO store topbar height, input height in model
                    -- 56 topbar
                    -- 72 input
                    , ("height", toString (model.appHeight - 56 - 82) ++ "px")
                    ]
                ]
                (List.map (viewCard model) cards)
        ]


viewCard : Model -> Card -> Html Msg
viewCard model card =
    li
        [ classList
            [ ("card", True)
            , ("_assigned", not (String.isEmpty card.assignedTo))
            ]
        , style [ ("padding", "10px") ]
        , onClick (SetPage (PageCard card.id))
        ]
        [ viewCardHeader card (Set.member card.authorId model.usersOnline)
        , div [ class "card-title" ] [ text card.title ]
        , div
            [ class "card-body"
            , style [ ("margin-top", "10px"), ("word-wrap", "break-word") ]
            ]
            [ text card.body ]
        , div [ style [ ("margin-top", "8px"), ("position", "relative") ] ]
            [ viewCardKarmaPrice model.user.moderator card ]
        ]


viewCardFull : Model -> Card -> Html Msg
viewCardFull model card =
    let
        background =
            case card.status of
                0 -> grayLightestColor
                1 -> "#fff"
                2 -> "#C0DF85"
                _ -> "#fff"

        deleteButton =
            if
                not (String.isEmpty model.user.uid)
                && model.user.uid == card.authorId
                && card.status < 2
            then
                div
                [ onClick (RemoveCard card)
                , style (buttonStyle ++ deleteButtonStyle)
                ]
                [ text "Удалить" ]
            else
                text ""

        helpButton =
            if
                not (String.isEmpty model.user.uid)
                && model.user.uid /= card.authorId
                && not (List.member model.user.uid model.activeCardVolunteers)
            then
                div
                [ onClick (TakeCard model.user card)
                , style (buttonStyle ++ takeButtonStyle)
                ]
                [ text "Помочь" ]
            else
                text ""

        volunteerList =
            if
                not (List.isEmpty model.activeCardVolunteers)
            then
                div []
                    [ h3 [] [ text "Желающие помочь:" ]
                    , ul [] (List.map (viewVolunteer model card model.user) model.activeCardVolunteers)
                    ]
            else
                text ""
    in
        div [ style
                [ ("padding", "10px")
                , ("background", background)
                ]
            ]
            [ viewCardHeader card (Set.member card.authorId model.usersOnline)
            , div [ class "card-title"] [ text card.title ]
            , div
                [ class "card-body"
                , style [ ("margin-top", "10px"), ("word-wrap", "break-word") ]
                ]
                [ text card.body ]
            , div [ style [ ("margin-top", "8px"), ("position", "relative") ] ]
                [ viewCardKarmaPrice model.user.moderator card
                , deleteButton
                , helpButton
                ]
            , volunteerList
            ]


viewCardHeader : Card -> Bool -> Html Msg
viewCardHeader card authorOnline =
    div [ class "card-header" ]
        [ a [ href (toHash (PageUser card.authorId))
            , H.onClickPreventDefault (SetPage (PageUser card.authorId))
            , classList
                [ ("card-header__author-photo", True)
                , ("online", authorOnline)
                ]
            , style
                [ ("position", "relative")
                , ("display", "block")
                , ("float", "left")
                ]
            ]
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
                    [ H.viewLink (PageUser card.authorId) card.authorName ]
                , li
                    [ style [ ("color", grayColor) ] ]
                    [ H.viewLink (PageCard card.id) card.creationTimeFriendly ]
                ]
            ]
        ]


viewCardKarmaPrice : Bool -> Card -> Html Msg
viewCardKarmaPrice userIsModerator card =
    span []
        [ span [ style [ ("color", grayColor) ] ] [ text "Карма: " ]
        , span
            [ contenteditable userIsModerator
            , on "blur" (Json.map (UpdateKarma card.authorId card.id) H.textContentDecoder)
            ]
            [ text (toString card.karma) ]
        ]


viewVolunteer : Model -> Card -> User -> String -> Html Msg
viewVolunteer model card currentUser volunteerId =
    let
        volunteer =
            case Dict.get volunteerId model.users of
                Nothing -> emptyUser
                Just user -> user

        assignVolunteerToCardButton =
            if
                (String.isEmpty card.assignedTo)
                && not (String.isEmpty currentUser.uid)
                && currentUser.uid == card.authorId
            then
                div [ onClick (AssignVolunteer card volunteer currentUser.name)
                    , style (buttonStyle ++ takeButtonStyle)
                    ]
                    [ text "Принять помощь" ]
            else
                text ""

        confirmHelpButton =
            if
                currentUser.uid == card.authorId
                && card.assignedTo == volunteer.uid
                && card.status == 1
            then
                div [ onClick (ConfirmHelp card)
                    , style (buttonStyle ++ assignedToLabelStyle)
                    ]
                    [ text "Спасибо" ]
            else
                text ""

        helpingLabel =
            if
                card.assignedTo == volunteer.uid
                && card.status == 1
            then
                span [ style italicLabelStyle ]
                    [ text "помогает" ]
            else
                text ""

        successfulHelperLabel =
            if
                card.assignedTo == volunteer.uid
                && card.status == 2
            then
                span [ style italicLabelStyle ]
                    [ text "помог" ]
            else
                text ""
    in
    li []
        [ a [ href (toHash (PageUser volunteer.uid)) ]
            [ img
                [ src volunteer.photoURL
                , width 25, height 25
                , style [ ("float", "left"), ("border-radius", "4px") ]
                ] []
            ]
        , span
            [ style [ ("line-height", "25px"), ("margin-left", "4px") ] ]
            [ H.viewLink (PageUser volunteer.uid) volunteer.name ]
        , helpingLabel
        , successfulHelperLabel
        , assignVolunteerToCardButton
        , confirmHelpButton
        ]
