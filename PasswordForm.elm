import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char exposing (isDigit, isLower, isUpper)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : Int
  }


model : Model
model =
  Model "" "" "" 0


-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    
    Password password ->
      { model | password = password }
    
    PasswordAgain password ->
      { model | passwordAgain = password }
    
    Age age ->
      let
        newAge = Result.withDefault 0 ( String.toInt age )
      in
        { model | age = newAge }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "number", placeholder "Age", onInput Age ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password /= model.passwordAgain then
        ("red", "Passwords do not match!")
      else if String.length model.password == 0 then
        ("green", "Please enter a password")
      else if String.length model.password < 8 then
        ("red", "Password must be at least eight characters")
      else if not (String.any isDigit model.password) then
        ("red", "Password must contain a number!") 
      else if not (String.any isLower model.password) then
        ("red", "Password must contain lowercase characters!")
      else if not (String.any isUpper model.password) then
        ("red", "Password must contain uppercase characters!")
      else
        ("green", "OK")
  in
    div [ style [("color", color)] ] [ text message ]
      

    
