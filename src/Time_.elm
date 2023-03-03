module Time_ exposing (..)

import Browser
import Html exposing (..)
import Task
import Time
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , paused: Bool
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) False
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | PauseToggle



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    PauseToggle ->
      ( { model | paused = (not model.paused) }
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then
    Sub.none

  else
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = String.fromInt (Time.toHour   model.zone model.time)
    minute = String.fromInt (Time.toMinute model.zone model.time)
    second = String.fromInt (Time.toSecond model.zone model.time)
  in
  div [ style "padding" "1rem", style "font-family" "monospace" ]
    [ p []
      [ span
        [ style "background-color" (if model.paused then "lightgrey" else "lightgreen")
        , style "font-size" "64px"
        , style "padding" "0 .5rem"
        ]
        [ text (pad hour ++ ":" ++ pad minute ++ ":" ++ pad second) ]
      ]
    , button [ onClick PauseToggle ] [ text "⏯️" ]
    ]



-- UTILITY FUNCTIONS


pad: String -> String
pad = String.pad 2 '0'
