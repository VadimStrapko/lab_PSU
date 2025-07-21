import Data.List (find, minimum, maximum, unzip4)
import Data.Maybe (listToMaybe) 

type Point = (Double, Double)
type Vector = (Double, Double)

data Font = Courier | Lucida | Fixedsys deriving (Eq, Show)

data Figure
  = Circle Point Double          -- круг 
  | Rectangle Point Point        -- прямоугольник   
  | Triangle Point Point Point   -- треугольник 
  | TextBox Point Font String    
  deriving (Show, Eq)

getFontDimensions :: Font -> (Double, Double)
getFontDimensions Courier  = (12.0, 7.0) -- шв
getFontDimensions Lucida   = (10.0, 6.0)
getFontDimensions Fixedsys = (14.0, 8.0)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

area :: Figure -> Double
area (Circle _ r) = pi * r * r
area (Rectangle (x1, y1) (x2, y2)) = abs $ (x2 - x1) * (y2 - y1)
area (Triangle p1 p2 p3) = -- Формула Герона
  let a = dist p1 p2
      b = dist p2 p3
      c = dist p3 p1
      s = (a + b + c) / 2.0
  in if s <= a || s <= b || s <= c 
     then 0.0
     else sqrt $ s * (s - a) * (s - b) * (s - c) 
area (TextBox _ font text) =
  let (h, w) = getFontDimensions font
      len = fromIntegral $ length text
  in len * w * h

getRectangles :: [Figure] -> [Figure]
getRectangles figs = filter isRectangle figs
   where
      isRectangle (Rectangle _ _) = True
      isRectangle _               = False

-- коорд пр
normalizeRect :: Point -> Point -> (Point, Point)
normalizeRect (x1, y1) (x2, y2) =
    let minX = min x1 x2
        maxX = max x1 x2
        minY = min y1 y2
        maxY = max y1 y2
    in ((minX, maxY), (maxX, minY)) 

-- огранич пр
getBound :: Figure -> Figure
getBound (Circle (cx, cy) r) = Rectangle (cx - r, cy + r) (cx + r, cy - r) 
getBound (Rectangle p1 p2) =
    let ((minX, maxY), (maxX, minY)) = normalizeRect p1 p2
    in Rectangle (minX, maxY) (maxX, minY)
getBound (Triangle (x1, y1) (x2, y2) (x3, y3)) =
    let minX = minimum [x1, x2, x3]
        maxX = maximum [x1, x2, x3]
        minY = minimum [y1, y2, y3]
        maxY = maximum [y1, y2, y3]
    in Rectangle (minX, maxY) (maxX, minY) 
getBound (TextBox (x, y) font text) =
    let (h, w) = getFontDimensions font
        len = fromIntegral $ length text
        -- Bottom-left is (x, y)
        -- Top-left is (x, y + h)
        -- Bottom-right is (x + len * w, y)
        -- Top-right is (x + len * w, y + h)
        topLeft = (x, y + h)
        bottomRight = (x + len * w, y)
    in Rectangle topLeft bottomRight 

-- пр
getRectCoords :: Figure -> (Double, Double, Double, Double)
getRectCoords (Rectangle (tlx, tly) (brx, bry)) = (tlx, tly, brx, bry)
getRectCoords fig = error $ "getRectCoords called on non-rectangle figure: " ++ show fig 

-- огранич пр
getBounds :: [Figure] -> Figure 
getBounds [] = Rectangle (0, 0) (0, 0)
getBounds (fig:figs) =
  let startCoords@(startX, startY_max, endX, startY_min) = getRectCoords (getBound fig)
      (finalMinX, finalMaxY, finalMaxX, finalMinY) =
         foldl (\(accMinX, accMaxY, accMaxX, accMinY) currentFig ->
                   let (currMinX, currMaxY, currMaxX, currMinY) = getRectCoords (getBound currentFig)
                   in (min accMinX currMinX, max accMaxY currMaxY, max accMaxX currMaxX, min accMinY currMinY)
               ) startCoords figs
  in Rectangle (finalMinX, finalMaxY) (finalMaxX, finalMinY) 

-- пр
isPointInBounds :: Point -> Figure -> Bool
isPointInBounds (px, py) (Rectangle (tlx, tly) (brx, bry)) =
    px >= tlx && px <= brx && 
    py <= tly && py >= bry    
isPointInBounds _ _ = False 

getFigure :: [Figure] -> Point -> Maybe Figure
getFigure figs p = find (\fig -> isPointInBounds p (getBound fig)) figs

movePoint :: Point -> Vector -> Point
movePoint (x, y) (dx, dy) = (x + dx, y + dy)

move :: Figure -> Vector -> Figure
move (Circle center r) v = Circle (movePoint center v) r
move (Rectangle p1 p2) v = Rectangle (movePoint p1 v) (movePoint p2 v)
move (Triangle p1 p2 p3) v = Triangle (movePoint p1 v) (movePoint p2 v) (movePoint p3 v)
move (TextBox pos font text) v = TextBox (movePoint pos v) font text