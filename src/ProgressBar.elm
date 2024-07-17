module ProgressBar exposing (..)

{--
The progress bar has one item for each question in the "mastery window".
If the user needs to get 4 (threshold) of the last 6 (window) right, then 
the progress bar has 6 items.

Each item in the progress bar can have three possible states:
* The user got the question right (Just RightAnswer)
* The user got the question wrong (Just WrongAnswer)
* The question hasn't been asked yet (Nothing)
--}

import Element
import Element.Background
import Element.Border


type alias ProgressBar =
    List (Maybe Bool)


emptyProgressBar : Int -> ProgressBar
emptyProgressBar masteryWindow =
    List.repeat masteryWindow Nothing


addToProgressBar : Int -> ProgressBar -> Bool -> ProgressBar
addToProgressBar progressBarLength progressBar progress =
    -- Add the latest progress (right or wrong) to the front of...
    Just progress
        -- the current progress bar with the last item removed
        :: List.take (progressBarLength - 1) progressBar


viewProgressBar : ProgressBar -> Element.Element msg
viewProgressBar progressBar =
    let
        -- Creates an empty element with a border (a box) for each item in progress list
        drawProgressBox p =
            let
                fillColor =
                    case p of
                        Just True ->
                            Element.rgb255 0 255 0

                        Just False ->
                            Element.rgb255 255 0 0

                        Nothing ->
                            Element.rgb 255 255 255
            in
            Element.el
                [ Element.Background.color fillColor
                , Element.padding 10
                , Element.Border.rounded 6
                , Element.Border.width 3
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.height Element.fill
                , Element.width (Element.fillPortion 1)
                ]
                Element.none
    in
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.fillPortion 1)
        , Element.padding 20
        ]
        (List.map drawProgressBox progressBar)


masteryThresholdReached : Int -> ProgressBar -> Bool
masteryThresholdReached threshold progressBar =
    let
        ( rightAnswers, _ ) =
            -- split the progress bar into two lists: the right answers and everything else (wrong and not yet)
            List.partition (\rOrW -> rOrW == Just True) progressBar
    in
    -- has the number of right answers reached the threshold?
    List.length rightAnswers >= threshold
