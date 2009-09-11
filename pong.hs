-- Pongell
-- A crappy ``pong'' clone written by kamatsu in haskell (in 127 lines!)
-- I honestly don't care what you do with this, so public domain
-- proof of concept demonstrating that yes, you can do games in haskell, and yes, it's nice
-- don't forget, your miso is boiling
--
import Prelude
import Graphics.UI.SDL as SDL
import Data.Maybe
import GHC.Word
import Control.Applicative 

class GameObject β where
   tick :: GameState -> β -> β
   draw :: β -> Surface -> IO ()

data Player = AI | Human deriving (Eq, Show)

data Direction = Up | Down | None deriving (Eq, Show)

data Paddle = Paddle { paddleX :: Int
                     , paddleY :: Int                                 
                     , paddleP :: Player
                     } deriving (Show)

data Ball = Ball { ballX  :: Int
                 , ballY  :: Int
                 , ballDX :: Int
                 , ballDY :: Int
                 , framesSinceSpeedup :: Int
                 } deriving (Show)

data GameState = GS { firstPaddle        :: Paddle 
                    , secondPaddle       :: Paddle
                    , ball               :: Ball
                    , humanReq           :: Direction
                    } deriving (Show)

createColor :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
createColor screen r g b = mapRGB (surfaceGetPixelFormat screen) r g b
 
isWithin :: Int -> (Int,Int) -> Bool
isWithin n (b,e) = n > b && n < e

ticks   = 1000 `div` 60
swidth  = 640
sheight = 480
pwidth  = 10
pheight = 50
bsize   = 10

instance GameObject Ball where
   tick gs (Ball x y dx dy fss) | game_over                   = Ball (swidth `div` 2) (sheight `div` 2) (-1) (-1) 0
                                | collides                    = Ball (x-dx) (y+dy) (-dx) dy (fss+1)
                                | y <= 0 || y >= sheight - bsize = Ball (x+dx) (y-dy) dx (-dy) (fss+1)
                                | fss > 1000                  = Ball (x+dx) (y+dy) (dx + (dx `div` (abs dx))) (dy + (dy `div` (abs dy))) 0
                                | otherwise                   = Ball (x+dx) (y+dy) dx dy (fss+1)
                   where 
                      game_over   = x > swidth || x < -bsize 
                      collides    = collides_p1 || collides_p2
                      collides_p1 = x < pwidth                  && y `isWithin` (p1y, p1y+pheight) && dx < 0
                      collides_p2 = x > swidth - pwidth - bsize && y `isWithin` (p2y, p2y+pheight) && dx > 0
                      p1y = paddleY $ firstPaddle $ gs
                      p2y = paddleY $ secondPaddle $ gs
   draw (Ball x y _ _ _) screen = do
              white  <- createColor screen 255 255 255
              fillRect screen (Just (Rect x y bsize bsize)) white  
              return ()      
                                           
instance GameObject Paddle where
   tick gs (Paddle x y p) | p == Human = tickFor $ humanReq gs 
                          | p == AI    = tickFor $ aiReq
                          where
                             tickFor d  = Paddle x (new_y y d) p
                             new_y y Up   = if y > 0                 then y - 3 else y
                             new_y y Down = if y < sheight - pheight then y + 3 else y
                             new_y y None = y
                             aiReq        = if ball_y > y + (pheight `div` 2) then
                                               Down
                                            else 
                                               if ball_y < y + (pheight `div` 2) then
                                                  Up
                                               else
                                                  None
                             ball_y        = ballY $ ball $ gs

   draw (Paddle x y _) screen = do
              white  <- createColor screen 255 255 255
              fillRect screen (Just (Rect x y pwidth pheight)) white  
              return ()
 
instance GameObject GameState where
   tick   gs@(GS p1 p2 b h) _  = GS (tick gs p1) (tick gs p2) (tick gs b) h
   draw   gs@(GS p1 p2 b h) s  = do
       bgColor  <- createColor s 0 0 0
       clipRect <- Just <$> getClipRect s
       fillRect s clipRect bgColor
       draw p1 s; draw p2 s; draw b s
       SDL.flip s
       return ()

eventHandler :: GameState -> Word32 -> IO ()
eventHandler gs last_frame = do
   e <- pollEvent
   s <- getVideoSurface
   t <- getTicks   
   let time = if t >= last_frame + ticks 
              then t
              else last_frame     
   case e of
        Quit                    -> quit
        NoEvent                 -> if t == time 
                                       then let newstate = (tick gs gs) 
                                            in draw newstate s >> eventHandler newstate time
                                       else eventHandler gs time

        KeyDown (Keysym SDLK_UP _ _)   -> eventHandler (gs { humanReq = Up   }) time
        KeyDown (Keysym SDLK_DOWN _ _) -> eventHandler (gs { humanReq = Down }) time

        KeyUp _                 -> eventHandler (gs { humanReq = None }) time
        otherwise               -> eventHandler gs time
        
main :: IO ()
main = do
   SDL.init [InitEverything]
   setVideoMode swidth sheight 32 [DoubleBuf, HWSurface]
   eventHandler (GS (Paddle 0 (sheight `div` 2) Human) (Paddle (swidth - pwidth) (sheight `div` 2) AI) (Ball (swidth `div` 2) (sheight `div` 2) (-1) (-1) 0) None) 0
