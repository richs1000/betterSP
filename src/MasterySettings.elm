module MasterySettings exposing (..)

{--
Mastery Settings get passed in through a JS port from Torus. The settings determine how many questions the user
must get right (the threshold) out of how many of the last questions asked (the window)
--}

import Element


type alias MasterySettings =
    { threshold : Int -- User needs to get <threshold> questions right...
    , window : Int -- out of the last <window> questions
    }


defaultMasterySettings : MasterySettings
defaultMasterySettings =
    { threshold = 4
    , window = 6
    }


viewInstructionsPanel : MasterySettings -> Element.Element msg
viewInstructionsPanel masterySettings =
    let
        instructions =
            "You need to answer "
                ++ String.fromInt masterySettings.threshold
                ++ " out of your last "
                ++ String.fromInt masterySettings.window
                ++ " questions correctly in order to advance."
    in
    Element.column
        [ Element.width Element.fill
        , Element.height (Element.fillPortion 1)
        , Element.padding 20
        ]
        [ Element.paragraph [] [ Element.text instructions ]
        ]
