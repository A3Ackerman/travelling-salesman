module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, a, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Round
import List exposing (reverse, member, isEmpty, head, tail, concat, filter, sortBy)
import Maybe exposing (withDefault)
import List.Extra exposing (..)
import Tuple exposing (first, second, pair)

type alias Model = {
  depotX: Float,
  depotY: Float,
  stops: List (Float, Float), 
  route: List (Float, Float),
  inputX: String, 
  inputY: String
  }

type alias Edge = {p1: (Float, Float), p2: (Float, Float), d: Float}
type alias Vertex = {pt: (Float, Float), connected: List (Float, Float)}


init : Model
init = {depotX = 0, depotY=0, stops = [], route = [], inputX = "", inputY = ""}
main = Browser.sandbox { init = init, update = update, view = view }

type Msg = Add | Set | ChangeX String | ChangeY String

update msg model =
  case msg of
    Add ->
      let 
        newStops = ((Maybe.withDefault 0 (String.toFloat model.inputX)), (Maybe.withDefault 0 (String.toFloat model.inputY))) :: model.stops
        withDepot = (model.depotX, model.depotY) :: newStops
      in
        {model | stops = newStops,
                route = greedyRoute withDepot}
    Set ->
      {model | depotX = (Maybe.withDefault 0 (String.toFloat model.inputX)), depotY = (Maybe.withDefault 0 (String.toFloat model.inputY))}
    ChangeX newX ->
      {model | inputX = newX }
    ChangeY newY ->
      {model | inputY = newY}

view model =
  div [] [
      text "X = "
    , input [ placeholder "0", value model.inputX, onInput ChangeX] []
    , text "Y = "
    , input [ placeholder "0", value model.inputY, onInput ChangeY] []
    , button [ onClick Add ] [ text "Add Stop" ]
    , button [ onClick Set ] [ text "Set Depot"]
    , div [] [text ("Depot Location: " ++ String.dropRight 2 (printList (model.depotX, model.depotY)))]
    , div [] [text (String.dropRight 2 (String.concat ("Stops:  " :: (List.map printList model.stops))))]
    , div [] [text (String.dropRight 2 (String.concat ("Route:  " :: (List.map printList model.route))))]
    ]
  

printList: (Float, Float) -> String
printList location = 
  "(" ++ Round.round 2 (Tuple.first location) ++ ", " ++ Round.round 2 (Tuple.second location) ++ "), "

-- This is an implementation of a greedy algorithm for the Travelling Salesman Problem, based on Bentley's Multi Fragment algorithm
-- not a fan of having to implement functionally w/o any loops...
greedyRoute: List (Float, Float) -> List (Float, Float)
greedyRoute stops =
  let
      edges =  uniquePairs stops
      edgeRecords: List Edge
      edgeRecords = reformatHelper [] edges
      sortedEdgeRecords = sortBy .d edgeRecords

      minGraph = greedyRecursion sortedEdgeRecords (List.length stops + 1) []
  in
    -- starting at depot follow the edges in the path list to unroll the path. order is arbitrary so have to check both first and second elements of the pairs
      unRoll [(head stops)] [] minGraph
    

greedyRecursion: List Edge -> Int -> List Vertex -> List Vertex
greedyRecursion  edges numStops minGraph = 

  if List.length minGraph == numStops then
    minGraph
  else
    let
      nextEdge = head edges

      newGraph = addEdge nextEdge minGraph

      -- isCyclic [] minGraph

      p1Seen = member nextEdge.p1 (map .pt minGraph)
      p2Seen = member nextEdge.p2 (map .pt minGraph)

      add = (map .p1) || ()
      
    in
      if add then
        greedyRecursion (tail edges) numStops newGraph
      else
        greedyRecursion (tail edges) numStops minGraph


reformatHelper records edges = 
  case edges of
     [] -> records
     x :: xs -> reformatHelper ({p1 = (first x), p2 = (second x), d = (distance x)} :: records) xs

distance: Edge -> Float
distance pair = 
  let
    (a,b) = pair
  in 
    sqrt ((Tuple.first a - Tuple.first b)^2 + (Tuple.second a - Tuple.second b)^2)

isCyclic : List (Float, Float) -> List (Float, Float) -> List Vertex -> Bool
isCyclic seen todo currGraph =
  if isEmpty todo then
    True
  else if member (head todo) seen then
    False
  else
    isCyclic ((head todo) :: seen) (concat (.connected (filter (.pt == (head todo)))) (tail todo)) currGraph

addEdge nextEdge minGraph = 

  let
    insertEdge1 v = 
      if v.pt == nextEdge.p1 then
        {v | connected = nextEdge.p2 :: v.connected}
      else
        v
    insertEdge2 v = 
      if v.pt == nextEdge.p2 then
        {v | connected = nextEdge.p1 :: v.connected}
      else
        v
  in
    if (member nextEdge.p1 (map .pt minGraph)) then
      if (member nextEdge.p2 (map .pt minGraph)) then
        map insertEdge1 (map insertEdge2 minGraph)
      else
        {pt = nextEdge.p2, connected = [nextEdge.p1]} :: (map insertEdge1 minGraph)
    else if (member nextEdge.p2 (map .pt minGraph)) then
      {pt = nextEdge.p1, connected = [nextEdge.p2]} :: (map insertEdge1 minGraph)
    else
      {pt = nextEdge.p1, connected = [nextEdge.p2]} :: ({pt = nextEdge.p2, connected = [nextEdge.p1]} :: minGraph)


unRoll todo path graph =
  if isEmpty todo then
    path
  else
    unRoll (tail todo) (concat (.connected (filter (.pt == (head todo)))) (tail todo)) graph