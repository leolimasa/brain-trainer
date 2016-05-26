module Main exposing (..)

import Html exposing (text, div, h1, input)
import Html.App exposing (program)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json
import String

type Message = UpdateAnswer String | TextboxKeyPress Int

type Operation = Sum | Subtract | Multiply | Divide

type alias Question =
    { leftNum : Float 
    , rightNum : Float 
    , operation : Operation
    }

type alias Model =
    { curQuestion : Question
    , curAnswer : Float
    , questionsRemaining : List Question
    , wrongAnswers : List Question
    }

onKeyPress : (Int -> a) -> Html.Attribute a
onKeyPress tagger =
    on "keypress" (Json.map tagger keyCode)
                 
update : Message -> Model -> (Model, Cmd Message)
update msg model = 
    ( case msg of
          UpdateAnswer a ->  
              { model |
                curAnswer =
                    case String.toFloat a of
                        Ok v -> v
                        Err er -> 0.0
              }
          TextboxKeyPress key ->
              if key == 13
              then answerQuestion model model.curAnswer
              else model
    , Cmd.none
    )

subscriptions : Model -> Sub Message 
subscriptions m = Sub.none

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

permutations : Int -> Int -> List (Float, Float)
permutations start end =
    List.map (\(x,y) -> (toFloat x, toFloat y))
    <| cartesian [start..end] [start..end]

opToString : Operation -> String
opToString op =
    case op of
        Sum -> "+"
        Subtract -> "-"
        Multiply -> "x"
        Divide -> "/"

questionsFromList : Operation -> List (Float, Float) -> List Question
questionsFromList op = List.map
                       (\(x, y) ->
                           { leftNum = x
                           , rightNum = y
                           , operation = op
                           }
                       )
                       
nextQuestion : Model -> Model 
nextQuestion m =
    { m |
      curQuestion =
          case List.head m.questionsRemaining of
              Just x -> x
              Nothing -> m.curQuestion
    , questionsRemaining =
        case List.tail m.questionsRemaining of
            Just x -> x
            Nothing -> []
    }

wrongAnswer : Model -> Model
wrongAnswer m =
    { m |
      wrongAnswers = m.curQuestion :: m.wrongAnswers
    }

evalQuestion : Question -> Float 
evalQuestion q = evalOperation q.operation q.leftNum q.rightNum

evalOperation : Operation -> Float -> Float -> Float 
evalOperation op a b = case op of
                         Sum -> a + b
                         Subtract -> a - b
                         Multiply -> a * b
                         Divide -> a / b
  
answerQuestion : Model -> Float -> Model
answerQuestion m answer = if (evalQuestion m.curQuestion) == answer
                          then nextQuestion m
                          else nextQuestion <| wrongAnswer m

startingModel : (Model, Cmd Message)
startingModel =
    ( { curAnswer = 0.0,
        curQuestion =
            { leftNum = 0.0
            , rightNum = 0.0
            , operation = Sum
            }
      , questionsRemaining =
          questionsFromList Multiply <| permutations 3 9 
      , wrongAnswers = []
      }
    , Cmd.none
    )

display : Model -> Html.Html Message 
display model =
    div []
        [ div []
              [ text (toString model.curQuestion.leftNum),
                text <| opToString model.curQuestion.operation,
                text (toString model.curQuestion.rightNum),
                text "=",
                input [ onInput UpdateAnswer, onKeyPress TextboxKeyPress ] []
              ] 
        , div []
            [ text <|
                  "Wrong answers: "
                  ++ toString (List.length model.wrongAnswers)
            ]
        ]

main =
    program {
             init = startingModel,
             update = update,
             view = display,
             subscriptions = subscriptions
       }
