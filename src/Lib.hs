module Lib (run) where

import Codec.Picture.Jpg
import Codec.Picture.Types
import Codec.Picture.Extra
import Codec.Picture
import Data.Array
import UI.NCurses
import System.Environment
import qualified Data.ByteString as ByteString

width :: Int
width = 120

height :: Int
height = 40

type Position = (Integer, Integer)
data Type = Empty | Sand | OOB deriving (Eq, Show)
type Particle = (Position, Type)
type World = [[Particle]]

getType :: Particle -> Type
getType (_, t) = t

imageToWorld :: DynamicImage -> World
imageToWorld image = do
  let converted = convertRGB8 image
  let scaledImage = scaleBilinear width height converted
  [[
    ((toInteger x, toInteger y), pixelToType (pixelAt scaledImage x y))
    | x <- [0..width - 1]]
    | y <- [0..height - 1]]

pixelToType  :: PixelRGB8 -> Type
pixelToType (PixelRGB8 r g b) =
  if (r + g + b) < 200
  then Sand
  else Empty

initWorld :: String -> IO World
initWorld file = do
  imageBytes <- ByteString.readFile file
  case decodeJpeg imageBytes of
    Left str -> return [[((0, 0), Empty)]]
    Right image -> return (imageToWorld image)

drawParticle :: (Particle) -> Update ()
drawParticle p = do
  let ((x, y), t) = p
  moveCursor y x
  case t of
    Sand -> drawString "s"
    Empty -> drawString " "
    OOB -> drawString " "

run :: IO ()
run = do
  args <- getArgs
  world <- initWorld (args !! 0)
  runCurses $ do
    setEcho False
    window <- defaultWindow
    tick world window 0

tick :: World -> Window -> Int -> Curses ()
tick world window i = do
  updateWindow window $ do
    mapM drawParticle [
     worldAt world x y
      | x <- [0..width - 1],
        y <- [0..height - 1]]
  render

  if i > 10
  then tick (tickWorld world) window (i + 1)
  else tick world window (i + 1)

worldAt :: World -> Int -> Int -> Particle
worldAt world x y = do
  if x >= width || y >= height || x < 0 || y < 0
  then ((toInteger x, toInteger y), OOB)
  else (world !! y) !! x

tickWorld :: World -> World
tickWorld world = do
  [[
    ((toInteger x, toInteger y), tickTile world x y)
    | x <- [0..width - 1]]
    | y <- [0..height - 1]]

tickTile :: World -> Int -> Int -> Type
tickTile world x y = do
  let above = getType $ worldAt world x (y - 1)
  let current = getType $ worldAt world x y
  let below = getType $ worldAt world x (y + 1)
  if above == Sand && current == Empty
  then Sand
  else if current == Sand && below == Empty
  then Empty
  else current
