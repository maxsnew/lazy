{- Adapted from the elm-lang.org example: http://elm-lang.org/edit/examples/Intermediate/SlideShow.elm -}

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import List
import List (..)
import Mouse
import Signal
import String
import Time (..)
import Window

import Lazy.Stream (Stream)
import Lazy.Stream as S

main : Signal Element
main =
  Signal.map2 slideShow Window.dimensions currentImage


slideShow : (Int, Int) -> String -> Element
slideShow (w,h) src =
  image 472 315 src
    |> container w h middle
    |> color black

images : Stream String
images = S.cycle "etc/book.jpg"
                 (List.map (String.append "etc/") [ "shells.jpg", "stack.jpg", "car.jpg", "pipe.jpg" ])

currentImage : Signal String
currentImage = S.sampleOn (every (2 * second)) images
