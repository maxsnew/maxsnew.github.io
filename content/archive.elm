--
-- title: Blog
--

import Color (..)
import Graphics.Collage (..)
import Mouse
import Window

mkShape : (Int, Int) -> (Int, Int) -> Int -> Float -> Int -> Int -> Form
mkShape (x, y) (w, h) sides size angle col = 
  let (dx,dy) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
      toDeg n = degrees << toFloat <| n % 360 
  in ngon sides size
      |> filled (hsl (toDeg col) (degrees << toFloat <| 45) (degrees << toFloat <| 45))
      |> rotate (toDeg angle)
      |> move (dx, dy)

bimap : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimap f g (x, y) = (f x, g y)

size : Int -> Float
size = (\n -> n * 500) << sin << (\n -> n / 500) << toFloat

-- silly : (Int, Int) -> (Int, Int) -> Element
silly (x,y) (w,h) perSec perHalfSec =
  collage w h
    [ mkShape (x,y)
              (w, h)
              ((+) 3 << flip (%) 12 << floor <| perSec)
              (size y)
              x
              (floor perHalfSec)
    ]

text = [markdown|
No posts yet, but here's a silly demo:
|]

main = above text <~ (silly <~ Mouse.position ~ Window.dimensions ~ every (1 * second) ~ every (0.5 * second))
