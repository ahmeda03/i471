module Lab7Sol (
  fact,
  poly2,
  thrd,
  Shape(..), area, perim,
  boundedShapes
               )
where

---------------------------------- fact ---------------------------------

-- fact n = n! for n >= 0
fact 0 = 1
fact n = n * fact (n - 1)

---------------------------------- poly2 --------------------------------

-- poly2 x a b: Given a second degree polynomial with roots
-- a and b, return its value at x; i.e. return (x - a) * (x - b)
poly2 x a b = (x - a) * (x - b)

---------------------------------- thrd3 --------------------------------

-- thrd3 triple: given a 3-tuple triple (_, _, x) return the third
-- element x.
thrd (_, _, x) = x

--------------------------------- shapes --------------------------------

-- a Len is an alias for an Int (a machine integer)
type Len = Int

-- sum type: Shape is a (Rect width height) or (Square side)
data Shape =
  Rect Len Len |  -- two Len fields are width and height of rect
  Square Len      -- Len field is side of square
  deriving (Show, Eq)

-- area Shape: return area of Shape
area (Rect width height) = width * height
area (Square side) = side * side


-- perim Shape: return perimeter of Shape
perim (Rect width height) = (width * 2) + (height * 2)
perim (Square side) = side * 4


------------------------------ boundedShapes ----------------------------

-- boundedShapes shapes bound: given a list shapes of Shape and some integer
-- bound, return those shapes having area < bound.
-- *Must* use a list comprehension.
boundedShapes shapes bound = [ shape | shape <- shapes, area shape < bound ]

  
