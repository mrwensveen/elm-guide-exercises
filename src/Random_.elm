module Random_ exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Task
import Process
import Html.Attributes exposing (disabled)
import Random exposing (Generator)
import Html.Attributes exposing (style)



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


type alias Model =
  { dieFace : Int
  , buttonDisabled: Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 False
  , Cmd.none
  )



-- UPDATE


type Msg
  = Click
  | Roll Int
  | NewFace Int

roll: Int -> Generator Int
roll currentValue =
  Random.int 1 6
  |> Random.andThen
    ( \value ->
      if value /= currentValue then
        Random.constant value
      
      else
        roll currentValue
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click ->
      ( {model | buttonDisabled = True}
      , Random.generate Roll (Random.int 0 100))

    Roll 0 ->
      ( {model | buttonDisabled = False }
      , Cmd.none
      )

    Roll reroll ->
      ( model
      , Cmd.batch
        [ Task.perform (\_ -> Roll (reroll - 1)) (Process.sleep 100)
        , Random.generate NewFace (roll model.dieFace)
        ]
      )

    NewFace newFace ->
      ( { model | dieFace = newFace }
        , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ style "padding" "1em" ]
    [ h1 [] [ text (String.fromInt model.dieFace) ]
    , button [ onClick Click, disabled model.buttonDisabled ] [ text "Roll" ]
    ]
