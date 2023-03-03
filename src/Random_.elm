module Random_ exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Process
import Random exposing (Generator)
import Random.Extra
import Task



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Die =
  { face: Int
  , bounces: Int
  }

type alias Model =
  { count: Int
  , dice : List Die
  , buttonDisabled: Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 5 (List.repeat 5 { face = 1, bounces = 0 }) False
  , Cmd.none
  )



-- UPDATE


type Msg
  = Click
  | SetCount (Maybe Int)
  | SetBounces (List Int)
  | Roll
  | NewFaces (List Int)

roll: Die -> Generator Int
roll die =
  if die.bounces == 0 then
    Random.constant die.face
  else
    Random.int 1 6
    |> Random.andThen
      ( \value ->
        if value /= die.face then
          Random.constant value
        
        else
          roll die
      )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetCount Nothing -> (model, Cmd.none)

    SetCount (Just count) ->
      let
        c = Basics.max 1 count
      in
      ( { model | count = c, dice = List.repeat c { face = 1, bounces = 0 }}
      , Cmd.none
      )

    -- Generate the number of bounces to a random value
    Click ->
      ( {model | buttonDisabled = True}
      , Random.generate SetBounces (Random.list model.count (Random.int 5 50))
      )

    -- Set the number of bounces and roll for the first time
    SetBounces bounces ->
      { model | dice = bounces |> List.map2 (\die b -> {die | bounces = b}) model.dice }
      |> update Roll

    -- Roll the dice if there are any that have bounces left      
    Roll ->
      let
        totalBounces =
          model.dice
          |> List.map (\die -> die.bounces)
          |> List.sum
      in
      case totalBounces of
        0 ->
          ({ model | buttonDisabled = False }, Cmd.none)
        _ ->
          ( model
          , Cmd.batch
            [ Task.perform (\_ -> Roll) (Process.sleep 100)
            , Random.generate NewFaces (Random.Extra.sequence (model.dice |> List.map roll))
            ]
          )

    NewFaces newFaces ->
      ( { model | dice = newFaces |> List.map2 (\die f -> {die | face = f, bounces = (Basics.max 0 (die.bounces - 1)) }) model.dice }
        , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ style "padding" "1rem" ]
    [ div [ style "margin-bottom" "1rem" ]
        ( model.dice |> List.map (\die -> span
          [ style "padding" "0 1rem"
          , style "font-size" "5rem"
          , style "font-family" "monospace"
          , style "color" (if die.bounces == 0 then "black" else "lightgray")
          ]
          [ text (String.fromInt die.face) ]) )
    , input [ type_ "number", value (String.fromInt model.count), onInput (String.toInt >> SetCount) ] []
    , button [ onClick Click, disabled model.buttonDisabled ] [ text "Roll" ]
    ]

