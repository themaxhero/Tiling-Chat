module Main exposing (init, main)
import Browser
import Element exposing (text)
import Model exposing (Model, initialModel)

type alias Flags = {}

type alias UpdateOutput =
  (Model, Cmd Msg)

init : Flags -> (Model, Cmd Msg)
init _ =
  (initialModel, Cmd.none)

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view >> Element.layout attr
    , update = update
    , subscriptions = subscriptions
    }

update : Msg -> Model -> UpdateOutput
update msg model =
  (model, Cmd.none)

attr : List (Attribute Msg)
attr =
  []

view : Model -> Element Msg
view _ =
  text ""

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none