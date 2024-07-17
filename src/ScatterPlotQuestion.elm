module ScatterPlotQuestion exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import FunctionOfX exposing (FunctionOfX)
import QuestionResponse
import Random
import Random.Extra
import Random.List
import ScatterPlot



{--
A multiple choice question has three parts:  
* the stem 
* the list of possible responses
* an image (optional)

A response has three parts:
* the text that is shown to the user
* the feedback to give the user if they choose this response
* a flag for whether this response is correct or not
--}


type ScatterPlotQuestionMsg
    = SPQMsgUserResponded QuestionResponse.QuestionResponse
    | SPQMsgReturnToTorus
    | SPQMsgGetNewQuestion


type ScatterPlotQuestionType
    = SPQWhatIsThePredictorVariable
    | SPQWhatIsTheResponseVariable
    | SPQLinearFunction


type alias ScatterPlotQuestion =
    { stem : String
    , scatterPlot : ScatterPlot.ScatterPlot
    , possibleResponses : List QuestionResponse.QuestionResponse
    }


emptyScatterPlotQuestion : ScatterPlotQuestion
emptyScatterPlotQuestion =
    { stem = "This is an empty question"
    , scatterPlot = ScatterPlot.emptyScatterPlot
    , possibleResponses = []
    }


viewScatterPlotQuestion : ScatterPlotQuestion -> Maybe QuestionResponse.QuestionResponse -> Element.Element ScatterPlotQuestionMsg
viewScatterPlotQuestion currentQuestion userResponse =
    let
        -- drawButton is used to add one button to the panel for each possible answer
        -- presented to the user
        -- The buttons are only active if the user has not selected a response
        -- Once the user chooses an answer, the buttons deactivate
        drawButton btn =
            let
                btnResponse =
                    if userResponse == Nothing then
                        -- If the user has not chosen a button, yet, then the buttons
                        -- are active
                        Just (SPQMsgUserResponded btn)

                    else
                        -- otherwise, buttons are all deactivated
                        Nothing

                btnBackgroundColor =
                    -- I want to set the button color based on whether or not it's the right answer
                    if userResponse == Nothing then
                        -- If the user hasn't chosen a button, then the button is white
                        Element.rgb255 255 255 255

                    else if btn.correctAnswer then
                        -- If this was the correct answer, color it green
                        Element.rgb 0 200 0

                    else
                        -- If this wasn't the correct answer, then color it red
                        Element.rgb255 200 0 0
            in
            Element.Input.button
                [ Element.padding 10
                , Element.Border.width 3
                , Element.Border.rounded 6
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.Font.variant Element.Font.smallCaps
                , Element.width (Element.fillPortion 1)
                , Element.Background.color btnBackgroundColor
                ]
                { onPress = btnResponse
                , label = Element.el [ Element.centerX ] (Element.text btn.textPart)
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 20

        -- , Element.explain Debug.todo
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.height Element.fill

            -- , Element.explain Debug.todo
            ]
            [ Element.paragraph
                [ Element.width (Element.fillPortion 1) ]
                [ Element.text currentQuestion.stem ]
            , Element.el
                [ Element.width (Element.fillPortion 1)
                , Element.height Element.fill
                , Element.padding 30
                ]
                (ScatterPlot.drawScatterPlot currentQuestion.scatterPlot)
            ]
        , Element.wrappedRow
            [ Element.width Element.fill ]
            (List.map drawButton currentQuestion.possibleResponses)
        ]


scatterPlotQuestionGenerator : Random.Generator ScatterPlotQuestion
scatterPlotQuestionGenerator =
    Random.Extra.andThen2
        -- Step 3: get the answers (takes scatter plot and question type as arguments)
        randomScatterPlotAnswers
        -- Step 1: get a random scatter plot (no arguments)
        ScatterPlot.scatterPlotGenerator
        -- Step 2: choose a random question type (no arguments)
        randomScatterPlotQuestionType


randomScatterPlotQuestionType : Random.Generator ScatterPlotQuestionType
randomScatterPlotQuestionType =
    -- Randomly choose from all possible types of questions
    -- "uniform" means each kind of question is equally likely
    Random.uniform SPQWhatIsThePredictorVariable [ SPQWhatIsTheResponseVariable, SPQLinearFunction ]


stringFromBool : Bool -> String
stringFromBool value =
    if value then
        "True"

    else
        "False"


randomScatterPlotAnswers : ScatterPlot.ScatterPlot -> ScatterPlotQuestionType -> Random.Generator ScatterPlotQuestion
randomScatterPlotAnswers sPlot qType =
    let
        isQuadratic =
            FunctionOfX.isQuadratic sPlot.fOfX

        -- Get the right answer
        rightAnswer =
            case qType of
                SPQWhatIsThePredictorVariable ->
                    QuestionResponse.QuestionResponse True "Correct!" sPlot.predictorVariable

                SPQWhatIsTheResponseVariable ->
                    QuestionResponse.QuestionResponse True "Correct!" sPlot.responseVariable

                SPQLinearFunction ->
                    QuestionResponse.QuestionResponse True "Correct!" (stringFromBool isQuadratic)

        -- Get the distractors
        distractorOne =
            case qType of
                SPQWhatIsThePredictorVariable ->
                    QuestionResponse.QuestionResponse False "The predictor variable is on the x axis" sPlot.responseVariable

                SPQWhatIsTheResponseVariable ->
                    QuestionResponse.QuestionResponse False "The predictor variable is on the x axis" sPlot.predictorVariable

                SPQLinearFunction ->
                    QuestionResponse.QuestionResponse False "Incorrect" (stringFromBool (not isQuadratic))

        -- Turn them into a list of QuestionResponse variables
        listOfDistractors =
            [ rightAnswer, distractorOne ]

        -- Question stem
        questionStem =
            case qType of
                SPQWhatIsThePredictorVariable ->
                    "Given this scatter plot, what is the predictor variable?"

                SPQWhatIsTheResponseVariable ->
                    "Given this scatter plot, what is the response variable?"

                SPQLinearFunction ->
                    "True or False: This scatter plot corresponds to a linear function"
    in
    Random.map
        (ScatterPlotQuestion questionStem sPlot)
        -- shuffle the order of possible answers
        (Random.List.shuffle listOfDistractors)


viewFeedback : Maybe QuestionResponse.QuestionResponse -> Bool -> Element.Element ScatterPlotQuestionMsg
viewFeedback userResponse masteryThresholdReached =
    case userResponse of
        Nothing ->
            -- If the user hasn't made a choice, then we don't give any feedback
            Element.none

        Just actualResponse ->
            -- otherwise, the user has made a choice and we give them the corresponding feedback
            let
                ( buttonMessage, buttonLabel ) =
                    if masteryThresholdReached then
                        -- if mastery threshold is reached then we're done
                        -- question button is the "let's go back to Torus" button
                        ( SPQMsgReturnToTorus
                        , "Return to Lesson"
                        )

                    else
                        -- if mastery threshold hasn't been reached then
                        -- question button is "give me the next question" button
                        ( SPQMsgGetNewQuestion
                        , "Next Question"
                        )

                nextBtn =
                    Element.Input.button
                        [ Element.padding 10
                        , Element.Border.width 3
                        , Element.Border.rounded 6
                        , Element.Border.color (Element.rgb255 0 0 0)
                        , Element.Font.variant Element.Font.smallCaps
                        , Element.width (Element.fillPortion 1)
                        ]
                        { onPress = Just buttonMessage
                        , label = Element.el [ Element.centerX ] (Element.text buttonLabel)
                        }
            in
            Element.column
                [ Element.width Element.fill

                -- , Element.height (Element.fillPortion 3)
                , Element.padding 20

                -- , Element.explain Debug.todo
                ]
                [ Element.row
                    [ Element.width Element.fill ]
                    [ Element.paragraph
                        [ Element.width Element.fill
                        , Element.padding 10
                        ]
                        [ Element.text actualResponse.feedback ]
                    ]
                , Element.row
                    [ Element.width Element.fill ]
                    [ nextBtn ]
                ]
