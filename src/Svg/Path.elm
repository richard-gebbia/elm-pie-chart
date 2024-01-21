module Svg.Path exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs


type alias Command =
    String


toSvg : List (Svg.Attribute msg) -> List Command -> Svg msg
toSvg attrs commands =
    let
        commands_ =
            commands ++ [ end ]
    in
    Svg.path ((String.join " " commands_ |> SvgAttrs.d) :: attrs) []


moveTo : Float -> Float -> Command
moveTo x y =
    "M " ++ String.fromFloat x ++ " " ++ String.fromFloat y


moveBy : Float -> Float -> Command
moveBy dx dy =
    "m " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy


lineTo : Float -> Float -> Command
lineTo x y =
    "L " ++ String.fromFloat x ++ " " ++ String.fromFloat y


lineBy : Float -> Float -> Command
lineBy dx dy =
    "l " ++ String.fromFloat dx ++ " " ++ String.fromFloat dy


horizontalLineTo : Float -> Command
horizontalLineTo y =
    "H " ++ String.fromFloat y


horizontalLineBy : Float -> Command
horizontalLineBy dy =
    "h " ++ String.fromFloat dy


verticalLineTo : Float -> Command
verticalLineTo x =
    "V " ++ String.fromFloat x


verticalLineBy : Float -> Command
verticalLineBy dx =
    "v " ++ String.fromFloat dx


type ArcSize
    = ArcSmall
    | ArcLarge


type ArcDirection
    = ArcPositive
    | ArcNegative


arcTo : Float -> Float -> Float -> ArcSize -> ArcDirection -> Float -> Float -> Command
arcTo rx ry xAxisRotation arcSize arcDirection endX endY =
    String.join " "
        [ "A"
        , String.fromFloat rx
        , String.fromFloat ry
        , String.fromFloat xAxisRotation
        , if arcSize == ArcSmall then
            "0"

          else
            "1"
        , if arcDirection == ArcPositive then
            "0"

          else
            "1"
        , String.fromFloat endX
        , String.fromFloat endY
        ]


arcBy : Float -> Float -> Float -> ArcSize -> ArcDirection -> Float -> Float -> Command
arcBy rx ry xAxisRotation arcSize arcDirection endDx endDy =
    String.join " "
        [ "a"
        , String.fromFloat rx
        , String.fromFloat ry
        , String.fromFloat xAxisRotation
        , if arcSize == ArcSmall then
            "0"

          else
            "1"
        , if arcDirection == ArcPositive then
            "0"

          else
            "1"
        , String.fromFloat endDx
        , String.fromFloat endDy
        ]


end : Command
end =
    "Z"
