module Game.Model
    (
      -- * Full game state
      Pong (..)

      -- * Model data
    , Paddle (..)
    , Ball (..)
    , Scores (..)
    , PlayState (..)

      -- * Model information
    , gameWidth
    , gameHeight

      -- * Initial state
    , initialGame
    ) where


------------------------------------------------------------------------------
-- | The entire state of the whole game.
data Pong = Pong
    { playState   :: PlayState
    , scores      :: Scores
    , leftPaddle  :: Paddle
    , rightPaddle :: Paddle
    , ball        :: Ball
    }

------------------------------------------------------------------------------
-- | Pong paddle, with accompanying y-position.
data Paddle = Paddle Float
    deriving (Show)


------------------------------------------------------------------------------
-- | Pong ball, with its current position and velocity.
data Ball = Ball (Float, Float) (Float, Float)
    deriving (Show)


------------------------------------------------------------------------------
-- | The score of both sides.
data Scores = Scores Int Int
    deriving (Show)


------------------------------------------------------------------------------
-- | The play state, either active or waiting (between rounds).
data PlayState = Play | Wait
    deriving (Show)


------------------------------------------------------------------------------
-- | Game width.
gameWidth :: Float
gameWidth = 600

-- | Game height.
gameHeight :: Float
gameHeight = 400


------------------------------------------------------------------------------
-- | Initial model.
initialGame :: Pong
initialGame = Pong Wait (Scores 0 0) (Paddle halfHeight)
    (Paddle halfHeight) (Ball (halfWidth, halfHeight) (150, 150))
  where
    halfWidth  = gameWidth / 2
    halfHeight = gameHeight / 2
