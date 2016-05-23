module Main exposing (..)

import Html exposing (text, div, h1)
import Html.App exposing (beginnerProgram)


type Message = GenerateMultiplication | SubmitAnswer

type Operation = Sum | Subtract | Multiply | Divide

type alias Question = {
      leftNum : Int
    , rightNum : Int 
    , operation : Operation 
  }

type alias Model = {
      curQuestion : Question
    , questionsRemaining : List Question
    , wrongAnswers : List Question
  }

update : Model -> Model -> Model 
update msg model = model

cartesian : List a -> List b -> List (a,b)
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> (x, y) ) ys )
    xs

permutations : Int -> Int -> List (Int, Int)
permutations start end = cartesian [start..end] [start..end]

opToString : Operation -> String
opToString op = case op of
                  Sum -> "+"
                  Subtract -> "-"
                  Multiply -> "x"
                  Divide -> "/"

questionsFromList : Operation -> List (Int, Int) -> List Question
questionsFromList op = List.map (\(x, y) -> {
                              leftNum = x
                              , rightNum = y
                              , operation = op
                            })
                       
nextQuestion : Model -> Model
nextQuestion m = { m |
                     curQuestion = List.head m.questionsRemaining
                     , questionsRemaining = List.tail m.questionsRemaining
                 }

startingModel : Model
startingModel = {
  curQuestion = {
      leftNum = 7
      , rightNum = 8
      , operation = Sum }
 , questionsRemaining = questionsFromList Multiply <| permutations 3 9 
 , wrongAnswers = [] }


display : Model -> Html.Html d
display model = div [] [
                 text (toString model.curQuestion.leftNum),
                 text <| opToString model.curQuestion.operation,
                 text (toString model.curQuestion.rightNum)
                ] 

main : Program Never
main = beginnerProgram {
             model = startingModel,
             update = update,
             view = display 
       }
