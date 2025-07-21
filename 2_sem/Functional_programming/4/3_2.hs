import Data.List (find, minimum, maximum, unzip4, foldl)
import Data.Maybe (listToMaybe)

type Point = (Double, Double)
type Vector = (Double, Double)

data Font = Courier | Lucida | Fixedsys deriving (Eq, Show)

data Figure
  = Circle Point Double
  | Rectangle Point Point
  | Triangle Point Point Point
  | TextBox Point Font String
  deriving (Show, Eq)

getFontDimensions :: Font -> (Double, Double)
getFontDimensions Courier  = (12.0, 7.0)
getFontDimensions Lucida   = (10.0, 6.0)
getFontDimensions Fixedsys = (14.0, 8.0)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

area :: Figure -> Double
area (Circle _ r) = pi * r * r
area (Rectangle (x1, y1) (x2, y2)) = abs $ (x2 - x1) * (y2 - y1)
area (Triangle p1 p2 p3) =
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
getRectangles = filter isRectangle
   where
      isRectangle :: Figure -> Bool
      isRectangle (Rectangle _ _) = True
      isRectangle _               = False

normalizeRect :: Point -> Point -> (Point, Point)
normalizeRect (x1, y1) (x2, y2) =
    let minX = min x1 x2
        maxX = max x1 x2
        minY = min y1 y2
        maxY = max y1 y2
    in ((minX, maxY), (maxX, minY))

getBound :: Figure -> Figure
getBound (Circle (cx, cy) r) =
    Rectangle (cx - r, cy + r) (cx + r, cy - r)
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
        topLeft = (x, y + h)
        bottomRight = (x + len * w, y)
    in Rectangle topLeft bottomRight

getRectCoords :: Figure -> (Double, Double, Double, Double)
getRectCoords (Rectangle (tlx, tly) (brx, bry)) = (tlx, tly, brx, bry)
getRectCoords fig = error $ "getRectCoords called on non-rectangle figure: " ++ show fig

getBounds :: [Figure] -> Figure
getBounds [] = Rectangle (0, 0) (0, 0)
getBounds figs =
  let bounds = map getBound figs
      coordsList = map getRectCoords bounds
      (minXs, maxYs, maxXs, minYs) = unzip4 coordsList
      finalMinX = minimum minXs
      finalMaxY = maximum maxYs
      finalMaxX = maximum maxXs
      finalMinY = minimum minYs
  in Rectangle (finalMinX, finalMaxY) (finalMaxX, finalMinY)

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

main :: IO ()
main = do
    let p1 = (0.0, 0.0)
        p2 = (4.0, 4.0)
        p3 = (0.0, 4.0)
        p4 = (4.0, 0.0)
        p5 = (2.0, 6.0)
        center = (5.0, 5.0)
        radius = 3.0

        circle1 = Circle center radius
        rect1 = Rectangle p3 p4
        triangle1 = Triangle p1 p2 p5
        textbox1 = TextBox (10.0, 1.0) Courier "Hello"

        figures = [circle1, rect1, triangle1, textbox1]

        vec = (1.0, -1.0)

    putStrLn $ "Original Figures: " ++ show figures

    putStrLn "\nAreas:"
    mapM_ (print . area) figures

    putStrLn $ "\nRectangles Only: " ++ show (getRectangles figures)

    putStrLn "\nBounds:"
    mapM_ (print . getBound) figures

    putStrLn $ "\nOverall Bound: " ++ show (getBounds figures)

    let testPoint1 = (1.0, 1.0)
    let testPoint2 = (5.0, 5.0)
    let testPoint3 = (2.0, 5.0)
    let testPoint4 = (11.0, 1.5)
    let testPoint5 = (100.0, 100.0)

    putStrLn "\nFigure Checks:"
    let checkPoint p = putStrLn $ "Figure at " ++ show p ++ ": " ++ show (getFigure figures p)
    checkPoint testPoint1
    checkPoint testPoint2
    checkPoint testPoint3
    checkPoint testPoint4
    checkPoint testPoint5

    putStrLn $ "\nMoved Figures by " ++ show vec ++ ":"
    let movedFigures = map (`move` vec) figures
    putStrLn $ show movedFigures
    putStrLn $ "\nOverall Bound of Moved Figures: " ++ show (getBounds movedFigures)
    
    
