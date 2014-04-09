{-# LANGUAGE BangPatterns #-}

import Data.Complex
import Codec.Picture
import Codec.Picture.Types
import Data.Array
import Control.Parallel.Strategies

minx = -2
maxx = 1
miny = -1
maxy = 1

width :: RealFloat a => a
width = 1920

height :: RealFloat a => a
height = 1080

escape = 2000

threshhold x = fromIntegral $ (x*255) `div` 500

getmandle x y = mandlebrot ! (x,y)

mandlebrot = array ((0,0),(round width, round height)) ls
  where poly c = iterate (\z -> z^2 + c) 0
        cutoff = length . takeWhile ((<2) . magnitude) . take escape
        xs = [minx, (minx)+((maxx-minx)/(width-1)) .. maxx]
        ys = [miny, (miny)+((maxy-miny)/(height-1)) .. maxy]
        points = xs `seq` ys `seq` [((x,y),threshhold . cutoff . poly $ r:+i) | (!x,!r) <- zip [0..] xs, (!y,!i) <- zip [0..] ys ]
        ls = points `using` parListChunk ((truncate height) `div` 8) rdeepseq

main = savePngImage "out.png" (ImageY8 (generateImage getmandle (truncate width) (truncate height)))
