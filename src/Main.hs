module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100
fps = 60

ballRadius = 10
paddleLength = 13

initialPosition = (0.0, 0.0)
initialVelocity = (60.0, 30.0)

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

main :: IO ()
main = simulate window background fps initialState draw update

update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = (moveBall seconds) . (wallBounce seconds) . (paddleBounce seconds)

type Radius = Float
type Position = (Float, Float)

data PongGame = Game
  { ballLoc :: Position
  , ballVel :: (Float, Float)
  , player1 :: Position
  , player2 :: Position
  } deriving Show

initialState :: PongGame
initialState = Game initialPosition initialVelocity (-120.0, 20.0) (120.0, 100.0)


draw :: PongGame -> Picture
draw game = pictures [ball, walls, mkPaddle rose (player1 game), mkPaddle orange (player2 game)]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Position -> Picture
    mkPaddle col pos = pictures
      [ translate x y $ color col $ rectangleSolid (2 * paddleLength) 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]
      where (x, y) = pos

    paddleColor = light (light blue)



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

wallBounce :: Float -> PongGame -> PongGame
wallBounce _ game = game { ballVel = (vx, vy') }
  where
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballLoc game) then -vy else vy


leftPaddleCollision :: Position -> Position -> Bool
leftPaddleCollision ballPosition player = xCollision && yCollision
  where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = x - ballRadius <= px
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)


rightPaddleCollision :: Position -> Position -> Bool
rightPaddleCollision ballPosition player = xCollision && yCollision
  where
    (x, y) = ballPosition
    (px, py) = player
    xCollision = x + ballRadius >= px
    yCollision = (y >= py - paddleLength) && (y <= py + paddleLength)

paddleBounce :: Float -> PongGame -> PongGame
paddleBounce _ game = game { ballVel = (vx', vy) }
  where
    (vx, vy) = ballVel game
    collision = leftPaddleCollision (ballLoc game) (player1 game)
      || rightPaddleCollision (ballLoc game) (player2 game)
    vx' = if collision then -vx else vx
