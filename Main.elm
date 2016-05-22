module Main exposing (..)

import Html exposing (text, div, h1)
import Html.App exposing (beginnerProgram)

update msg model = model

type alias Model = {
     leftNum : Float 
   , rightNum : Float
   , operation : String}

startingModel : Model
startingModel = {
   leftNum = 3
 , rightNum = 6
 , operation = "+"}

display model = div [] [
                 text (toString model.leftNum),
                 text model.operation,
                 text (toString model.rightNum)
                ] 

main = beginnerProgram {
             model = startingModel,
             update = update,
             view = display 
       }

       
