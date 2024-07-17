module QuestionResponse exposing (..)


type alias QuestionResponse =
    { correctAnswer : Bool -- True when this is the right answer
    , feedback : String -- The feedback associated with this answer
    , textPart : String -- What gets displayed on the button for the user to choose
    }


correctResponse : QuestionResponse
correctResponse =
    { textPart = "This is the right answer"
    , feedback = "You chose the right answer"
    , correctAnswer = True
    }


oneDistractor : QuestionResponse
oneDistractor =
    { textPart = "This is the first distractor"
    , feedback = "You chose the first distractor"
    , correctAnswer = False
    }


anotherDistractor : QuestionResponse
anotherDistractor =
    { textPart = "This is the second distractor"
    , feedback = "You chose the second distractor"
    , correctAnswer = False
    }
