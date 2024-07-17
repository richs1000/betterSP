module ScatterPlot exposing (..)

import Chart
import Chart.Attributes
import Element
import Element.Region
import FunctionOfX
import Html.Attributes
import Random
import Random.Extra
import StatRandom
import Svg


type alias ScatterPlotPoint =
    { predictor : Float
    , response : Float
    }


defaultData : List ScatterPlotPoint
defaultData =
    [ ScatterPlotPoint 0.5 4
    , ScatterPlotPoint 0.8 5
    , ScatterPlotPoint 1.2 6
    , ScatterPlotPoint 1.4 6
    , ScatterPlotPoint 1.6 4
    , ScatterPlotPoint 3 8
    , ScatterPlotPoint 3 9
    , ScatterPlotPoint 3.2 10
    , ScatterPlotPoint 3.8 7
    , ScatterPlotPoint 6 12
    , ScatterPlotPoint 6.2 8
    , ScatterPlotPoint 6 10
    , ScatterPlotPoint 6 9
    , ScatterPlotPoint 9.1 8
    , ScatterPlotPoint 9.2 13
    , ScatterPlotPoint 9.8 10
    , ScatterPlotPoint 12 7
    , ScatterPlotPoint 12.5 5
    , ScatterPlotPoint 12.5 2
    ]


type alias ScatterPlot =
    { predictorVariable : String
    , responseVariable : String
    , fOfX : FunctionOfX.FunctionOfX
    , data : List ScatterPlotPoint
    }


emptyScatterPlot : ScatterPlot
emptyScatterPlot =
    { predictorVariable = "no predictor variable"
    , responseVariable = "no response variable"
    , fOfX = []
    , data = []
    }


pointsGenerator : List Float -> FunctionOfX.FunctionOfX -> Float -> Random.Generator (List ScatterPlotPoint)
pointsGenerator xs fOfX stdDev =
    let
        pointGenerator : Float -> Random.Generator ScatterPlotPoint
        -- Generates a point a random distance from an equation
        pointGenerator x =
            Random.map
                -- Construct a point with a guaranteed x value
                (ScatterPlotPoint x)
                -- and a randomized y-value
                (StatRandom.normal (FunctionOfX.calculate fOfX x) stdDev)

        listOfPointGenerators : List (Random.Generator ScatterPlotPoint)
        -- Create one pointGenerator for each x-value
        listOfPointGenerators =
            List.map pointGenerator xs
    in
    -- Use the list of pointGenerators to create a single generator for a list of random points
    Random.Extra.sequence listOfPointGenerators


scatterPlotGeneratorPart3 : String -> String -> FunctionOfX.FunctionOfX -> Float -> Random.Generator ScatterPlot
scatterPlotGeneratorPart3 predictorV responseV fOfX stdDev =
    let
        xs : List Float
        -- xs are not randomly chosen, they are the numbers 0 to 100
        xs =
            List.map (\xAsInt -> toFloat xAsInt)
                (List.range 1 100)

        adjustedStdDev : Float
        adjustedStdDev =
            if FunctionOfX.equationOrder fOfX == FunctionOfX.OrderQuadratic then
                stdDev * 10

            else
                stdDev
    in
    Random.map
        -- step 9: create a scatter plot!
        (ScatterPlot predictorV responseV fOfX)
        -- step 8: randomly generate points based on the equation
        (pointsGenerator xs fOfX adjustedStdDev)


scatterPlotGeneratorPart2 : String -> String -> Int -> Random.Generator ScatterPlot
scatterPlotGeneratorPart2 predictorV responseV eqnOrder =
    let
        randomlyChooseFunctionOfX : Random.Generator FunctionOfX.FunctionOfX
        -- generator for list of coefficients [b0, b1] for equation (y = b0 + b1 * x)
        randomlyChooseFunctionOfX =
            Random.list eqnOrder (Random.float 2 10)

        randomlyChooseStdDev : Random.Generator Float
        -- generator for standard deviation
        randomlyChooseStdDev =
            -- Random.float 80 100
            Random.float 20 40
    in
    Random.Extra.andThen2
        -- step 7: use the function and std dev to generate points
        (scatterPlotGeneratorPart3 predictorV responseV)
        -- step 5: randomly choose coefficients for the function
        randomlyChooseFunctionOfX
        -- step 6: randomly choose a standard deviation around the function
        randomlyChooseStdDev


scatterPlotGenerator : Random.Generator ScatterPlot
scatterPlotGenerator =
    let
        randomlyChoosePredictorVariable : Random.Generator String
        -- generator for randomly choosing one of the strings for a predictor variable
        randomlyChoosePredictorVariable =
            Random.uniform firstPredictorVariable remainingPredictorVariables

        randomlyChooseResponseVariable : Random.Generator String
        -- generator for randomly choosing one of the strings for a response variable
        randomlyChooseResponseVariable =
            Random.uniform firstResponseVariable remainingResponseVariables

        -- generator for order of equation
        -- 75% chance it's linear, 25% chance it's quadratic
        randomlyChooseEqnOrder =
            Random.weighted ( 75, 2 ) [ ( 25, 3 ) ]
    in
    Random.Extra.andThen3
        -- step 4: use the eqn order to get a function for the points in the scatter plot
        scatterPlotGeneratorPart2
        -- step 1: chose a predictor variable
        randomlyChoosePredictorVariable
        -- step 2: choose a response variable
        randomlyChooseResponseVariable
        -- step 3: choose an order (linear or quadratic) for the underlying equation
        randomlyChooseEqnOrder


generateAltText : ScatterPlot -> String
generateAltText sPlot =
    "A scatter plot with "
        ++ sPlot.predictorVariable
        ++ " on the x axis and "
        ++ sPlot.responseVariable
        ++ " on the y axis"


drawScatterPlot : ScatterPlot -> Element.Element msg
drawScatterPlot sPlot =
    Element.el
        -- I wrap this thing in an "el" to get the alt text
        [ Element.Region.description (generateAltText sPlot)
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.html
            -- sampleScatterPlot
            (Chart.chart
                [ Chart.Attributes.height 300
                , Chart.Attributes.width 300
                , Chart.Attributes.padding { top = 25, bottom = 30, left = 30, right = 10 }
                , Chart.Attributes.range [ Chart.Attributes.lowest 0 Chart.Attributes.exactly ]
                , Chart.Attributes.htmlAttrs [ Html.Attributes.alt "This is the alternate text" ]
                ]
                [ -- draw x-axis
                  Chart.xAxis []

                -- draw y-axis
                , Chart.yAxis []

                -- draw data
                , Chart.series .predictor
                    [ Chart.scatter .response [ Chart.Attributes.opacity 0, Chart.Attributes.borderWidth 1 ]
                    ]
                    sPlot.data

                -- label y-axis
                , Chart.labelAt .min
                    Chart.Attributes.middle
                    [ Chart.Attributes.rotate 90
                    , Chart.Attributes.fontSize 12
                    ]
                    [ Svg.text sPlot.responseVariable ]

                -- label x-axis
                , Chart.labelAt Chart.Attributes.middle
                    .min
                    [ Chart.Attributes.fontSize 12 ]
                    [ Svg.text sPlot.predictorVariable ]
                ]
            )
        )


firstPredictorVariable : String
firstPredictorVariable =
    "Chest diameter (in inches)"


remainingPredictorVariables : List String
remainingPredictorVariables =
    [ "Functional Independence Measure (from 18 to 126)"
    , "Berg Balance Scale score (ranges from 0 to 56)"
    , "Grip strength (in pounds)"
    , "Age (in years)"
    , "Number of ADLs a client can complete independently"
    , "Number of falls an individual has in a month"
    , "Mini-Mental State Exam score (from 0 to 30)"
    , "Beck Depression Inventory score (from 0 to 63)"
    ]


firstResponseVariable : String
firstResponseVariable =
    "Weight (in pounds)"


remainingResponseVariables : List String
remainingResponseVariables =
    [ "Temperature (in degrees Celsius)"
    , "Modified Ashworth Scale score (from 0 to 5)"
    , "Blood pressure"
    , "Height (in inches)"
    , "Heart rate (in beats per minute)"
    , "# of verbal outbursts during a single class"
    , "Score on Timed Up and Go test (in seconds)"
    ]
