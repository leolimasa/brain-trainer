module Main exposing (..)

import Html exposing (text, div, h1, input)
import Html.App exposing (program)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json
import String
import Time exposing (second, Time)

type Message = UpdateAnswer String | TextboxKeyPress Int | Tick Time 

type Operation = Sum | Subtract | Multiply | Divide

type alias Question =
    { leftNum : Float 
    , rightNum : Float 
    , operation : Operation
    }

type alias Model =
    { timeleft : Int
    , curQuestion : Question
    , curAnswer : Float
    , correctAnswer : Maybe Float
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
          Tick t -> model
          {-    if model.timeleft == 0 && model.correctAnswer == Nothing
              then answerQuestion model model.curAnswer 
              else
                  if model.correctAnswer == Nothing
                  then { model | timeleft = model.timeleft - 1 }
                  else model
                  -}
    , Cmd.none
    )

subscriptions : Model -> Sub Message 
subscriptions m =
    if (List.length m.questionsRemaining > 0)
    then Time.every second Tick 
    else Sub.none

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
resetTime : Model -> Model
resetTime m = { m | timeleft = 5 }
                           
nextQuestion : Model -> Model
nextQuestion m =
    resetTime 
    <| if (List.length m.questionsRemaining) > 0
       then nextRegularQuestion m
       else nextWrongQuestion m
                           
nextRegularQuestion : Model -> Model 
nextRegularQuestion m =
    { m |
      timeleft = 5
    , curAnswer = 0
    , curQuestion =
          case List.head m.questionsRemaining of
              Just x -> x
              Nothing -> m.curQuestion
    , questionsRemaining =
        case List.tail m.questionsRemaining of
            Just x -> x
            Nothing -> []
    }

nextWrongQuestion m =
    { m |
      curQuestion =
        case List.head m.wrongAnswers of
            Just x -> x
            Nothing -> m.curQuestion
    , wrongAnswers =
        case List.tail m.wrongAnswers of
            Just x -> x
            Nothing -> []
    }

wrongAnswer : Model -> Model
wrongAnswer m =
    { m | wrongAnswers = m.curQuestion :: m.wrongAnswers
    , correctAnswer = Just <| evalQuestion m.curQuestion 
    }

evalQuestion : Question -> Float 
evalQuestion q = evalOperation q.operation q.leftNum q.rightNum

evalOperation : Operation -> Float -> Float -> Float 
evalOperation op a b =
    case op of
        Sum -> a + b
        Subtract -> a - b
        Multiply -> a * b
        Divide -> a / b
  
answerQuestion : Model -> Float -> Model
answerQuestion m answer =
    let
        evaled = (evalQuestion m.curQuestion)
    in
        if m.correctAnswer /= Nothing
        then nextQuestion { m | correctAnswer = Nothing }
        else if evaled == answer
             then nextQuestion { m | correctAnswer = Nothing } 
             else 
                 wrongAnswer m 
                   
                              
startingModel : (Model, Cmd Message)
startingModel =
    ( { timeleft = 5,
        curAnswer = 0.0,
        correctAnswer = Nothing,
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
        , div [] <| footerMessage model
        ]

footerMessage : Model -> List (Html.Html a)
footerMessage m =
    [
     text (case m.correctAnswer of
               Just a -> "Correct answer: " ++ toString a
               Nothing -> ""
          )
     , text <|
          "Wrong answers: "
          ++ toString (List.length m.wrongAnswers)
    ]
    ++ if (List.length m.questionsRemaining) == 0 && (List.length m.wrongAnswers) > 0
       then [text "Correction time"]
       else [] 
    ++ if (List.length m.questionsRemaining) == 0 && (List.length m.wrongAnswers) == 0
       then [text "Done!"]
       else []
         


main =
    program { init = startingModel
            , update = update
            , view = display
            , subscriptions = subscriptions
            }
