module WindowDimensions exposing (..)

{--
Window dimensions are used to make the app responsive. I need to keep track of the width and height.
The initial values get passed in to the app as flags. Then I use a subscription to keep track of 
changes during use.
--}


type alias WindowDimensions =
    { winWidth : Int
    , winHeight : Int
    }


defaultWindowDimensions : WindowDimensions
defaultWindowDimensions =
    { winWidth = 800
    , winHeight = 600
    }
