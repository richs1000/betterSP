module FunctionOfX exposing (..)


type alias FunctionOfX =
    -- f(x) = b_0 + b_1 . x + b_2 . x^2 + ...
    -- The list stores the coefficients in order
    -- c[0] = b_0, c[1] = b_1, ...
    List Float


calculate : FunctionOfX -> Float -> Float
calculate fOfX x =
    let
        -- exp raises a^p
        -- exp 2 3 = 2^3 = 8
        exp : Float -> Int -> Float
        exp a p =
            if p <= 0 then
                1

            else
                a * exp a (p - 1)

        fOfXHelper : FunctionOfX -> Int -> Float
        fOfXHelper coefficients power =
            case coefficients of
                c :: cs ->
                    (c * exp x power)
                        + fOfXHelper cs (power + 1)

                [] ->
                    0
    in
    fOfXHelper fOfX 0


type EquationOrder
    = OrderConstant
    | OrderLinear
    | OrderQuadratic


equationOrder : FunctionOfX -> EquationOrder
equationOrder fOfX =
    case fOfX of
        x :: [] ->
            OrderConstant

        x1 :: x2 :: [] ->
            OrderLinear

        _ ->
            OrderQuadratic


isQuadratic : FunctionOfX -> Bool
isQuadratic fOfX =
    List.length fOfX < 3
