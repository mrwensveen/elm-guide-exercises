module Http_ exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http
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


type Model
  = Failure
  | Loading Int
  | Success String


cmd : Cmd Msg
cmd =
  Http.get
    { url = "https://elm-lang.org/assets/public-opinion.txt"
    , expect = Http.expectString GotText
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading 2
  , cmd
  )



-- UPDATE


type Msg
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          case model of
            Loading n ->
              if n == 0 then
                (Failure, Cmd.none)
              else
                (Loading (n - 1), cmd)

            _ -> (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading n ->
      text ("Loading..." ++ String.fromInt n)

    Success fullText ->
      pre [ style "padding" "1em" ] [ text fullText ]