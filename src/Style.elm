module Style exposing (..)

-- coral
brandColor : String
brandColor = "#f2836b"
brandLighterColor : String
brandLighterColor = "#f2c3ab"
brandLightestColor : String
brandLightestColor = "#ffe3cb"
-- almbost black
darkestColor : String
darkestColor = "#333"
darkColor : String
darkColor = "#555"
-- lightest gray
grayColor : String
grayColor = "#999"
-- lightest
grayLightestColor : String
grayLightestColor = "#f9f9f9"
-- blue
linkColor : String
linkColor = "#1da1f2"

buttonStyle : List (String, String)
buttonStyle =
    [ ("cursor", "pointer")
    , ("padding", "4px 8px")
    , ("color", "white")
    , ("border-radius", "4px")
    ]
deleteButtonStyle : List (String, String)
deleteButtonStyle =
    [ ("background", darkestColor)
    , ("position", "absolute")
    , ("right", "0")
    , ("top", "-4px")
    ]
takeButtonStyle : List (String, String)
takeButtonStyle =
    [ ("background", brandColor)
    , ("display", "inline")
    , ("margin-left", "8px")
    ]
assignedToLabelStyle : List (String, String)
assignedToLabelStyle =
    [ ("background", "limegreen")
    , ("display", "inline")
    , ("margin-left", "8px")
    ]
successfulHelperLabelStyle : List (String, String)
successfulHelperLabelStyle =
    [ ("background", darkestColor)
    , ("display", "inline")
    , ("margin-left", "8px")
    ]