--
-- title: Blog
--

import open Graphics.Collage
import Mouse

mkShape : Int -> Float -> Int -> Int -> Form
mkShape sides size angle col = 
  let toDeg n = degrees . toFloat <| n `mod` 360 in
  ngon sides size
  |> filled (hsv (toDeg col) (degrees . toFloat <| 45) (degrees . toFloat <| 45))
  |> rotate (toDeg angle)

size : Int -> Float
size = (\n -> n * 500) . sin . (\n -> n / 500) . toFloat

silly : Signal Element
silly = collage 1000 500 <~ combine
        [ mkShape <~ ((\n -> n + 3) . flip mod 12 . floor <~ every (1 * second))
                   ~ (size . snd <~ Mouse.position)
                   ~ (fst <~ Mouse.position)
                   ~ (floor <~ every (0.5 * second))
        ]

text = [markdown|
No posts yet, but here's a silly demo:
|]

main = above text <~ silly
