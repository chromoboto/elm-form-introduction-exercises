import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" "" ""



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

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
    , viewInput "text" "Age" model.age Age
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

notLongEnough : String -> Bool
notLongEnough password =
  if String.length password < 8 then
    True
  else
    False

containsDigitUppercaseLowercase : String -> Bool
containsDigitUppercaseLowercase password =
  if (True /= ((String.any Char.isDigit password)
    && (String.any Char.isUpper password)
    && (String.any Char.isLower password))) then
      True
  else
    False

viewValidation : Model -> Html msg
viewValidation model =
  if notLongEnough model.password then
    div [ style "color" "red" ] [ text "Password needs to be at least 8 characters long." ]
  else if containsDigitUppercaseLowercase model.password then
    div [ style "color" "red" ] [ text "The password must contain a digit, an uppercase letter, and a lowercase letter." ]
  else if model.password /= model.passwordAgain then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else if Nothing == String.toInt model.age then
    div [ style "color" "red" ] [ text "Please enter an integer for age."]
  else
    div [ style "color" "green" ] [ text "OK" ]
