import Html exposing (..)
import Keyboard exposing (..)
import Char exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard.Extra exposing (..)
import Time exposing (..)
import List exposing (..)
import Physics exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- IMMUTABLE SETTINGS
type alias Settings =
    { height : Int
    , width : Int
    , horizontalMargin : Int
    , paddleHeight : Int
    , paddleWidth : Int
    , ballR : Int
    , jumpSize : Int
    }

settings : Settings
settings =
    { height = 300
    , width = 600
    , horizontalMargin = 20
    , paddleHeight = 100
    , paddleWidth = 20
    , ballR = 5
    , jumpSize = 10
    }

-- MODEL

type alias Model =
    { leftPaddleY : Int
    , rightPaddleY : Int
    , ballCX : Int
    , ballCY : Int
    , ballDX : Int
    , ballDY : Int
    , scoreLeft: Int
    , scoreRight: Int
    , pressedKeys : List Key
    }



model : Model
model =
    { leftPaddleY = 100
    , rightPaddleY = 100
    , ballCX = 300
    , ballCY = 150
    , ballDX = 0
    , ballDY = 0
    , scoreLeft = 0
    , scoreRight = 0
    , pressedKeys = []
    }

init : (Model, Cmd Msg)
init = (model, Cmd.none)

-- UPDATE

type Msg
    = KeyboardMsg Keyboard.Extra.Msg
    | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            ({ model | pressedKeys = Keyboard.Extra.update keyMsg model.pressedKeys}, Cmd.none)
        Tick time ->
            (applyTickUpdates model, Cmd.none)

applyTickUpdates : Model -> Model
applyTickUpdates model = updateModelByKeys model |> moveBall

giveLeftPlayerPoint : Model -> Model
giveLeftPlayerPoint model =
    { model | scoreLeft = model.scoreLeft + 1 }

giveRightPlayerPoint : Model -> Model
giveRightPlayerPoint model =
    { model | scoreRight = model.scoreRight + 1 }

moveBall : Model -> Model
moveBall model =
    -- just stop velocity if fully outside screen
    case hitStatus model of
        HitNothing ->
            tickBallPosition model
        HitPaddle ->
            flipBallDX model |> tickBallPosition
        HitTopOrBottom ->
            flipBallDY model |> tickBallPosition
        OffLeft ->
            resetBall model |> giveRightPlayerPoint
        OffRight ->
            resetBall model |> giveLeftPlayerPoint

tickBallPosition : Model -> Model
tickBallPosition model =
    { model | ballCX = model.ballCX + model.ballDX, ballCY = model.ballCY + model.ballDY }

resetBall : Model -> Model
resetBall model =
    { model | ballDX = 0, ballDY = 0, ballCX = 300, ballCY = 150}

flipBallDX : Model -> Model
flipBallDX model =
    { model | ballDX = (model.ballDX * -1) }

flipBallDY : Model -> Model
flipBallDY model =
    { model | ballDY = (model.ballDY * -1) }

hitStatus : Model -> HitStatus
hitStatus model =
    foldl foldlHitStatus HitNothing [hitPaddle model, hitTopOrBottom model, offLeft model, offRight model]

foldlHitStatus : HitStatus -> HitStatus -> HitStatus
foldlHitStatus hitA hitB =
    case hitA of
        HitNothing -> hitB
        _ -> hitA

type HitStatus = HitNothing | HitPaddle | HitTopOrBottom | OffLeft | OffRight

hitPaddle : Model -> HitStatus
hitPaddle model =
    if boxesCollide (ballBoundingBox model) (leftPaddleBoundingBox model) || boxesCollide (ballBoundingBox model) (rightPaddleBoundingBox model) then
        HitPaddle
    else
        HitNothing

hitTopOrBottom : Model -> HitStatus
hitTopOrBottom model =
    if (.y1 (ballBoundingBox model) >= settings.height || .y2 (ballBoundingBox model) >= settings.height) || (.y1 (ballBoundingBox model) <= 0 || .y2 (ballBoundingBox model) <= 0)  then
        HitTopOrBottom
    else
        HitNothing

-- only count as off if totally disappeared
offLeft : Model -> HitStatus
offLeft model =
    if .x1 (ballBoundingBox model) < 0 && .x2 (ballBoundingBox model) < 0 then
        OffLeft
    else
        HitNothing

-- only count as off if totally disappeared
offRight : Model -> HitStatus
offRight model =
    if .x1 (ballBoundingBox model) > settings.width && .x2 (ballBoundingBox model) > settings.width then
        OffRight
    else
        HitNothing

leftPaddleBoundingBox : Model -> Box
leftPaddleBoundingBox model =
    { x1 = settings.horizontalMargin
    , x2 = settings.horizontalMargin + settings.paddleWidth
    , y1 = model.leftPaddleY
    , y2 = model.leftPaddleY + settings.paddleHeight
    }

rightPaddleBoundingBox : Model -> Box
rightPaddleBoundingBox model =
    { x1 = (settings.width - settings.horizontalMargin - settings.paddleWidth)
    , x2 = (settings.width - settings.horizontalMargin)
    , y1 = model.rightPaddleY
    , y2 = model.rightPaddleY + settings.paddleHeight
    }

ballBoundingBox : Model -> Box
ballBoundingBox model =
    { x1 = model.ballCX - settings.ballR
    , x2 = model.ballCX + settings.ballR
    , y1 = model.ballCY - settings.ballR
    , y2 = model.ballCY + settings.ballR
    }

startMovementIfStopped : Model -> Model
startMovementIfStopped model =
    if model.ballDX == 0 && model.ballDY == 0 then
        { model | ballDX = 3, ballDY = 1 }
    else
        model

updateModelByKey : Key -> Model -> Model
updateModelByKey key model =
    let
        clampToEdges = clamp 0 (settings.height - settings.paddleHeight)
    in
        case key of
            CharW ->
                { model | leftPaddleY = clampToEdges (model.leftPaddleY - settings.jumpSize) }
            CharS ->
                { model | leftPaddleY = clampToEdges (model.leftPaddleY + settings.jumpSize) }
            CharK ->
                { model | rightPaddleY = clampToEdges (model.rightPaddleY - settings.jumpSize) }
            CharM ->
                { model | rightPaddleY = clampToEdges (model.rightPaddleY + settings.jumpSize) }
            Space ->
                startMovementIfStopped model
            _ ->
                model

updateModelByKeys : Model -> Model
updateModelByKeys model = foldl updateModelByKey model model.pressedKeys

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions,
        every (inMilliseconds 10) Tick
        ]

-- View

view : Model -> Html Msg
view model =
  div []
    [ h3 [] [ Html.text "Press spacebar to start, w/s to control the left paddle, and k/m to control the right paddle." ], gameBoard model
    ]

whiteColor : String
whiteColor = "rgb(255,255,255)"

gameBoard : Model -> Html.Html msg
gameBoard model =
    let
        w = (toString settings.width)
        h = (toString settings.height)
        leftY = (toString model.leftPaddleY)
        rightY = (toString model.rightPaddleY)
        pw = (toString settings.paddleWidth)
        ph = (toString settings.paddleHeight)
        topY = (toString 0)
        bottomY = (toString settings.height)
        leftX = (toString settings.horizontalMargin)
        rightX = (toString (settings.width - settings.horizontalMargin - settings.paddleWidth))
        ballCX = (toString model.ballCX)
        ballCY = (toString model.ballCY)
        ballR = (toString settings.ballR)
        centerX = (toString (settings.width // 2))
        leftScoreX = (toString ((settings.width // 2) - 45))
        rightScoreX = (toString ((settings.width // 2) + 25))
    in
        svg
          [ Svg.Attributes.style "margin-left: 20px", width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h) ]
          [ rect [ x "0", y "0", width w, height h ] []
          , line [ strokeDasharray "10, 10", x1 centerX, y1 topY, x2 centerX, y2 bottomY, Svg.Attributes.style ("stroke:" ++ whiteColor ++ ";stroke-width:5") ] []
          , circle [ cx ballCX, cy ballCY, r ballR, fill whiteColor ] []
          , rect [ x leftX, y leftY , width pw, height ph, fill whiteColor ] []
          , rect [ x rightX, y rightY , width pw, height ph, fill whiteColor ] []
          , Svg.text_ [ x leftScoreX, y "50", fill whiteColor, Svg.Attributes.style "font-size: 44px;" ] [ Svg.text (toString model.scoreLeft) ]
          , Svg.text_ [ x rightScoreX, y "50", fill whiteColor, Svg.Attributes.style "font-size: 44px;" ] [ Svg.text (toString model.scoreRight) ]
          ]
