module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact

width, height, offset :: Int
width = 300
height = 300
offset = 100
fps = 60

playerSpeed = 4.0

ballRadius = 10
paddleWidth = 13
paddleLength = 46
wallWidth = 10

paddleYMax = 150 - paddleLength - wallWidth

initialPosition = (0.0, 0.0)
initialVelocity = (60.0, 30.0)

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = play window background fps initialState draw handleKeys update

update :: Float -> PongGame -> PongGame
update seconds game = if (paused game)
                      then game
                      else ((moveBall seconds) . wallBounce . paddleBounce . movePlayers . handleBallOutOfBounds) game

type Radius = Float
type Position = (Float, Float)
data PlayerMovement = PlayerUp | PlayerStill | PlayerDown deriving Show
data Winner = Player1 | Player2 | NoOne

data PongGame = Game
  { ballLoc :: Position
  , ballVel :: (Float, Float)
  , player1 :: Position
  , player1Movement :: PlayerMovement
  , player1Score :: Int
  , player2 :: Position
  , player2Movement :: PlayerMovement
  , player2Score :: Int
  , paused :: Bool
  } deriving Show

initialState :: PongGame
initialState = Game initialPosition initialVelocity
  (-120.0, 20.0) PlayerStill 0
  (120.0, 100.0) PlayerStill 0
  False

draw :: PongGame -> Picture
draw game = pictures [ball, walls, mkPaddle rose (player1 game), mkPaddle orange (player2 game), scores]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 wallWidth

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Position -> Picture
    mkPaddle col pos = pictures
      [ translate x y $ color col $ rectangleSolid (2 * paddleWidth) (2 * paddleLength)
      , translate x y $ color paddleColor $ rectangleSolid ((2 * paddleWidth) - 4) ((2 * paddleLength) - 4)
      ]
      where (x, y) = pos

    paddleColor = light (light blue)

    scores = translate (-30) (115) $
      scale 0.25 0.25 $
      color white $
      text $
      (show (player1Score game)) ++ " " ++ (show (player2Score game))



moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    x' = x + vx * seconds
    y' = y + vy * seconds

wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    topCollision = y - ballRadius <= -fromIntegral width / 2
    bottomCollision = y + ballRadius >= fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballLoc game) then -vy else vy


leftPaddleCollision :: Position -> Position -> Bool
leftPaddleCollision ballPosition player = xCollision && yCollision
  where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = x - ballRadius == px + paddleWidth
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

rightPaddleCollision :: Position -> Position -> Bool
rightPaddleCollision ballPosition player = xCollision && yCollision
  where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = x + ballRadius == px - paddleWidth
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    (vx, vy) = ballVel game
    collision = leftPaddleCollision (ballLoc game) (player1 game)
      || rightPaddleCollision (ballLoc game) (player2 game)
    vx' = if collision then -vx else vx

movePlayer :: Position -> PlayerMovement -> Position
movePlayer (px, py) PlayerUp = (px, min (py + playerSpeed) paddleYMax)
movePlayer (px, py) PlayerStill = (px, py)
movePlayer (px, py) PlayerDown = (px, max (py - playerSpeed) (-paddleYMax))

movePlayers :: PongGame -> PongGame
movePlayers game = game { player1 = movePlayer (player1 game) (player1Movement game)
                        , player2 = movePlayer (player2 game) (player2Movement game)
                        }

winner :: Position -> Winner
winner (bx, _) | bx >= 150 = Player1
winner (bx, _) | bx <= (-150) = Player2
winner _ = NoOne

handleBallOutOfBounds :: PongGame -> PongGame
handleBallOutOfBounds game = case winner (ballLoc game) of
                             NoOne -> game
                             Player1 -> game { ballLoc = initialPosition, player1Score = (player1Score game) + 1}
                             Player2 -> game { ballLoc = initialPosition, player2Score = (player2Score game) + 1}


handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 't') Down _ _) game = game { ballLoc = initialPosition }
handleKeys (EventKey (Char 'p') Down _ _) game = game { paused = not (paused game) }

handleKeys (EventKey (Char 'w') Down _ _) game = game { player1Movement = PlayerUp }
handleKeys (EventKey (Char 'w') Up _ _) game = game { player1Movement = PlayerStill }
handleKeys (EventKey (Char 'r') Down _ _) game = game { player1Movement = PlayerDown }
handleKeys (EventKey (Char 'r') Up _ _) game = game { player1Movement = PlayerStill }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { player2Movement = PlayerUp }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { player2Movement = PlayerStill }
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { player2Movement = PlayerDown }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game { player2Movement = PlayerStill }

handleKeys _ game = game
