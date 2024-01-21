module PieChart exposing (..)

import Color exposing (Color)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import Svg.Path as SvgPath


pieChart : Float -> List Color -> List Float -> Svg msg
pieChart radius colors data =
    let
        total : Float
        total =
            List.sum data

        pieSlice : Color -> Float -> Float -> ( Svg msg, Float )
        pieSlice color startAngle value =
            let
                fraction : Float
                fraction =
                    value / total

                arcSize : SvgPath.ArcSize
                arcSize =
                    if fraction < 0.5 then
                        SvgPath.ArcSmall

                    else
                        SvgPath.ArcLarge

                endAngle : Float
                endAngle =
                    startAngle + (fraction * 2 * pi)
            in
            ( SvgPath.toSvg [ color |> Color.toCssString |> SvgAttrs.fill, SvgAttrs.stroke "white" ]
                [ SvgPath.moveTo radius radius
                , SvgPath.lineTo (radius * cos startAngle + radius) (radius * sin startAngle + radius)
                , SvgPath.arcTo radius radius 0 arcSize SvgPath.ArcNegative (radius * cos endAngle + radius) (radius * sin endAngle + radius)
                ]
            , endAngle
            )

        coloredData : List ( Color, Float )
        coloredData =
            List.map2 Tuple.pair colors data

        walk : ( Color, Float ) -> ( List (Svg msg), Float ) -> ( List (Svg msg), Float )
        walk ( color, datum ) ( slices, startAngle ) =
            let
                ( slice, endAngle ) =
                    pieSlice color startAngle datum
            in
            ( slice :: slices, endAngle )
    in
    List.foldr walk ( [], 0 ) coloredData
        |> Tuple.first
        |> Svg.g []


main =
    Svg.svg
        [ SvgAttrs.width "300"
        , SvgAttrs.height "300"
        , SvgAttrs.viewBox "0 0 300 300"
        ]
        [ pieChart 100 [ Color.red, Color.blue, Color.green ] [ 1, 2, 3 ]
            |> List.singleton
            |> Svg.g [ SvgAttrs.transform "translate(50,50)" ]
        ]
