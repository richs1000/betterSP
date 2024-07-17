{--
elm init
elm install jxxcarlson/elm-stat
elm install mdgriffith/elm-ui
elm install terezka/elm-charts
elm install elm/svg
elm install elm/random
elm install elm-community/random-extra

elm make src/Main.elm --output=app.js --debug

echo "# betterSP" >> README.md
git init
git add .
git commit -m "first commit"
git branch -M main
git remote add github https://github.com/richs1000/betterSP.git
git push -u github main

--}


port module Main exposing (..)

import Browser
import Browser.Events
import Element
import Html
import MasterySettings
import ProgressBar exposing (masteryThresholdReached)
import QuestionResponse
import Random
import ScatterPlotQuestion
import WindowDimensions



{-
   The ports get data in from Torus to initialize the exercise and then send
   data back to Torus to let it know that we're done.

   I have to use a port instead of flags because Torus doesn't get all the initialization
   data to my code quickly enough.

   I use subscriptions to receive data from the ports and to window events.
-}


port getFromTorus : (MasterySettings.MasterySettings -> msg) -> Sub msg


port sendToTorus : Bool -> Cmd msg


main : Program WindowDimensions.WindowDimensions Model Msg
main =
    Browser.element
        { init = initModel
        , view = viewModel
        , update = updateModel
        , subscriptions = mySubscriptions
        }


type alias Model =
    { currentQuestion : ScatterPlotQuestion.ScatterPlotQuestion
    , userResponse : Maybe QuestionResponse.QuestionResponse
    , masterySettings : MasterySettings.MasterySettings
    , windowDimensions : WindowDimensions.WindowDimensions
    , progressBar : ProgressBar.ProgressBar
    }


createNewModel : WindowDimensions.WindowDimensions -> MasterySettings.MasterySettings -> Model
createNewModel newWindowDimensions newMasterySettings =
    { currentQuestion = ScatterPlotQuestion.emptyScatterPlotQuestion
    , userResponse = Nothing
    , masterySettings = newMasterySettings
    , windowDimensions = newWindowDimensions
    , progressBar = ProgressBar.emptyProgressBar newMasterySettings.window
    }


initModel : WindowDimensions.WindowDimensions -> ( Model, Cmd Msg )
initModel windowDimensions =
    ( createNewModel windowDimensions MasterySettings.defaultMasterySettings
    , Random.generate MsgDisplayNewQuestion ScatterPlotQuestion.scatterPlotQuestionGenerator
    )


type Msg
    = MsgReturnToTorus -- The user reached the threshold, go back to Torus (send to JavaScript)
    | MsgUpdateMasterySettings MasterySettings.MasterySettings -- Settings for mastery questions coming in from Torus (get from JavaScript)
    | MsgWindowSizeChanged Int Int -- Window changed size - maybe the device was rotated, maybe a change in the window
    | MsgGetNewQuestion -- The user has read the feedback and has asked for the next question
    | MsgDisplayNewQuestion ScatterPlotQuestion.ScatterPlotQuestion -- I just created a new random question, display it
    | MsgUserResponded ScatterPlotQuestion.ScatterPlotQuestionMsg -- User pressed a button to choose an answer to the question


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        -- The user has demonstrated mastery, kick control back to Torus
        MsgReturnToTorus ->
            ( model, sendToTorus True )

        -- The user has demonstrated mastery, kick control back to Torus
        MsgUserResponded ScatterPlotQuestion.SPQMsgReturnToTorus ->
            ( model, sendToTorus True )

        -- Data to initialize the exercise has come in from Torus.
        MsgUpdateMasterySettings settings ->
            let
                newMasterySettings : MasterySettings.MasterySettings
                newMasterySettings =
                    { threshold = settings.threshold
                    , window = settings.window
                    }

                currentWindowDimensions : WindowDimensions.WindowDimensions
                currentWindowDimensions =
                    model.windowDimensions
            in
            ( createNewModel currentWindowDimensions newMasterySettings
            , Random.generate MsgDisplayNewQuestion ScatterPlotQuestion.scatterPlotQuestionGenerator
            )

        -- Something happened to change the window size, update the model to store the new size
        MsgWindowSizeChanged newWidth newHeight ->
            let
                newWindowDimensions : WindowDimensions.WindowDimensions
                newWindowDimensions =
                    { winWidth = newWidth
                    , winHeight = newHeight
                    }
            in
            ( { model | windowDimensions = newWindowDimensions }
            , Cmd.none
            )

        -- The user has read the feedback and asked for a new question
        MsgGetNewQuestion ->
            ( model, Random.generate MsgDisplayNewQuestion ScatterPlotQuestion.scatterPlotQuestionGenerator )

        MsgUserResponded ScatterPlotQuestion.SPQMsgGetNewQuestion ->
            ( model, Random.generate MsgDisplayNewQuestion ScatterPlotQuestion.scatterPlotQuestionGenerator )

        -- We just got a new random question, so display it and wait for the user to respond
        MsgDisplayNewQuestion newRandomQuestion ->
            ( { model
                | currentQuestion = newRandomQuestion
                , userResponse = Nothing
              }
            , Cmd.none
            )

        -- The user responded. We need to:
        -- 1. provide feedback,
        -- 2. update the progress bar
        MsgUserResponded (ScatterPlotQuestion.SPQMsgUserResponded newUserResponse) ->
            ( { model
                | userResponse = Just newUserResponse
                , progressBar = ProgressBar.addToProgressBar model.masterySettings.window model.progressBar newUserResponse.correctAnswer
              }
            , Cmd.none
            )


viewModel : Model -> Html.Html Msg
viewModel model =
    let
        masteryThresholdReached : Bool
        masteryThresholdReached =
            ProgressBar.masteryThresholdReached model.masterySettings.threshold model.progressBar
    in
    Element.layout
        -- [ Element.width Element.fill
        -- , Element.height Element.fill
        -- ]
        [ Element.width (Element.px 800)
        , Element.height (Element.px 800)
        ]
        (Element.column
            [ Element.width Element.fill ]
            [ MasterySettings.viewInstructionsPanel model.masterySettings
            , ProgressBar.viewProgressBar model.progressBar
            , Element.map MsgUserResponded (ScatterPlotQuestion.viewScatterPlotQuestion model.currentQuestion model.userResponse)
            , Element.map MsgUserResponded (ScatterPlotQuestion.viewFeedback model.userResponse masteryThresholdReached)
            ]
        )


mySubscriptions : Model -> Sub Msg
mySubscriptions _ =
    Sub.batch
        [ -- Received mastery settings (threshold and window) from Torus
          getFromTorus MsgUpdateMasterySettings

        -- Update the model when the browser window gets resized
        , Browser.Events.onResize MsgWindowSizeChanged
        ]
