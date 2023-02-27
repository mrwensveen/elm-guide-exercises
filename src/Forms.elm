module Forms exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Extra exposing (find)


-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.password /= model.passwordAgain then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else case validate model.password of
    Err err -> div [ style "color" "red" ] [ text err ]
    Ok _ -> div [ style "color" "green" ] [ text "OK" ]



-- VALIDATION

validations : List ((String -> Bool), String)
validations =
  [ ( \x -> String.length x < 8, "Password is too short!" )
  , ( \x -> not (containsUpper x), "Password should contain uppercase characters!" )
  , ( \x -> not (containsLower x), "Password should contain lowercase characters!" )
  , ( \x -> not (containsNumeric x), "Password should contain numeric characters!" )
  ]

validate : String -> Result String String
validate password =
  let validationResult = validations |> find (\(fn, _) -> fn password)
  in case validationResult of
    Nothing -> Ok password
    Just (_, s) -> Err s



-- UTILITY FUNCTIONS


anyMember : List a -> List a -> Bool
anyMember list1 list2 = list1
  |> List.any (\x -> List.member x list2)

containsTransformed : (String -> String) -> String -> Bool
containsTransformed fn a = a
  |> fn
  |> String.toList
  |> anyMember (String.toList a)

containsUpper : String -> Bool
containsUpper = containsTransformed String.toUpper

containsLower : String -> Bool
containsLower = containsTransformed String.toLower

containsNumeric : String -> Bool
containsNumeric = String.any Char.isDigit
