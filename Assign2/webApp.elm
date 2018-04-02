--This is a simpler timer
module TimerApp exposing (..)

import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg)
import Time exposing (Time, second)
import String exposing (padLeft)

--main--

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- model--

initialModel : Model
initialModel =
    { status = Initialised
    , timer = 0
    }


type alias Model =
    { status : Status
    , timer : Int
    }


type Status
    = Running
    | Finished
    | Initialised
    | Increased
    | Decreased
    | Paused

-- Msgs

type Msg
    = Start
    | Reset
    | Increase
    | Decrease
    | Pause
    | DecTime Time


--Updates
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | timer = model.timer, status = Running }, Cmd.none ) --keeps the timer running
        Reset ->
            ( { model | status = Initialised, timer = 0 }, Cmd.none ) --sets the timer to 0
        Increase ->
            ( { model | status = Increased, timer = model.timer + 1 }, Cmd.none ) --increase the time
        Decrease ->
            if model.timer > 0 then
                ( { model | status = Decreased, timer = model.timer - 1  }, Cmd.none ) --decrease the time
            else
                ( {model | status = Finished}, Cmd.none)
        Pause ->
            ( { model | status = Paused, timer = model.timer }, Cmd.none ) --pause the timer
        DecTime _ -> --the decrease in timer
            if model.status == Running then
                if model.timer > 0 then
                    ( { model | timer = model.timer - 1 }, Cmd.none ) --timer can not be 0
                else
                    ( { model | status = Finished }, Cmd.none )
            else
                ( model, Cmd.none )




-- VIEW


row : Html.Attribute Msg
row = style [("display", "inline-block"), ("padding", "20px")]

info : Html.Attribute Msg
info = style [("width", "500px"), ("text-align", "center")]

view : Model -> Html Msg
view model =
  div [ style[("font-family", "Courier"), ("margin", "10px"), ("background", "yellow"), ("height", "80%")] ]
    [ div [ row ] [ button [ onClick Decrease ] [ text "-" ] ]
    , div [ row , style [("text-align", "center"), ("font-size", "30px")] ] [ text (toString (model.timer//60) ++ ":" ++ toString (model.timer%60)) ]
    , div [ row ] [ button [ onClick Increase ] [ text "+" ] ]
    , div [ info , style [("text-align", "center")] ] [ text (toString model.status) ]
    , div [ row ] [ button [ onClick Start ] [ text "start" ] ]
    , div [ row ] [ button [ onClick Pause ] [ text "pause" ] ]
    , div [ row ] [ button [ onClick Reset ] [ text "reset" ] ]
    , div [ info ] [ p [] [ text "This is a simple timer, Increase the timer first and then click start" ] ]
    ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    if model.status == Running then
        Sub.batch
            [ Time.every second DecTime ]
    else
        Sub.none
