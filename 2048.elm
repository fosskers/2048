import ListHelp (..)

import Graphics.Element as G
import Window as W
import Keyboard as K
import Random as R

---

data Block = Block Int

type Row  = [Block]
type Grid = [Row]

type Direction  = { x:Int, y:Int }
type Dimensions = (Int,Int)

dim : Int
dim = 4

-- | Starting board
board : Grid
board = repeat dim <| repeat dim (Block 0)

-- | The number of `Block 0` in a Grid.
blanks : Grid -> Int
blanks g = case g of
             [] -> 0
             [] :: rs -> blanks rs
             (Block 0 :: bs) :: rs -> 1 + blanks (bs :: rs)
             (_ :: bs) :: rs -> blanks (bs :: rs)

-- | Adds a new `Block 1` to the `nth` empty space (Block 0).
addNew : Int -> Grid -> Grid
addNew n g =
    let f n bs =
        case bs of
          Block 0 :: bs' -> if n == 0 then Block 1 :: bs' else Block 0 :: f (n-1) bs'
          b :: bs'       -> b :: f n bs'
    in groupsOf dim . f n . concat <| g

next : Block -> Block
next (Block n) = Block <| n + 1

randNthPos : Signal Int
randNthPos = R.range 1 (dim ^ 2) K.arrows

-- | ROW REDUCTION
reduce : Row -> Row
reduce row =
    let f b r =
          case r of
            []            -> [b]
            Block 0 :: xs -> b :: xs
            x :: xs       -> if b == x then next b :: xs else b :: x :: xs
        rd = foldr f [] row
        zs = repeat (dim - length rd) <| Block 0  -- Padding. Ensures length `dim`.
    in zs ++ rd

right : Grid -> Grid
right = map reduce

left : Grid -> Grid
left = map (reverse . reduce . reverse)

up : Grid -> Grid
up = transpose . left . transpose

down : Grid -> Grid
down = transpose . right . transpose

-- | RENDERING
-- By setting the color of the container, you can create borders.
render : Dimensions -> Grid -> Element
render (w,h) g =
    let f  = flow G.right . map (asSquare (w,h))
        s  = (min w h `div` 10) * dim * 2
    in center (w,h) . container s s middle . flow G.down . map f <| g

shift : Direction -> Grid -> Grid
shift d g =
  maybe g (\f' -> let g' = f' g
                  in if blanks g' == 0 then g' else addNew 0 g') <| shiftBy d

shiftBy : Direction -> Maybe (Grid -> Grid)
shiftBy {x,y} = if | x == 1  && y == 0  -> Just right
                   | x == -1 && y == 0  -> Just left
                   | x == 0  && y == 1  -> Just up
                   | x == 0  && y == -1 -> Just down
                   | otherwise          -> Nothing

asSquare : Dimensions -> Block -> Element
asSquare (w,h) b = let co = colour b
                       si = min w h `div` 10
                       sh = circle <| toFloat si
                   in collage (si * 2) (si * 2) [filled co sh]

-- | Yields a colouring function based Block rank.
colour : Block -> Color
colour (Block n) =
    let cs = [ lightGray, lightRed, lightOrange, lightYellow, lightGreen
             , lightBlue, lightPurple, red, orange, yellow
             , green, blue, purple, darkRed, darkOrange, darkYellow
             , darkGreen, darkBlue, darkPurple ]
    in maybe gray id <| cs !! (n `mod` length cs)

-- | Centers an Element based on given Window size.
center : Dimensions -> Element -> Element
center (w,h) e = container w h middle e

main : Signal Element
main = render <~ W.dimensions ~ (foldp shift board K.arrows)
