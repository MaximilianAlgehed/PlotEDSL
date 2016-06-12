-- | A representation of indecies
data Index = Direct Int | Length Array | P Index Index | M Index Index | T Index Index deriving (Eq, Show)

-- | Numerical 
instance Num Index where
    (+) = P
    (-) = M
    (*) = T
    fromInteger = Direct . fromInteger

-- | An array representation
data Array = Argument | IndexOf Array Index | Range Index Index Array deriving (Eq, Show)

-- | Some combinators for describing arrays
arg = Argument
(!) = IndexOf
range (a, b) arr = Range a b arr

-- | Models data layouts
data Layout a = An a | Above (Layout a) (Layout a) | Besides (Layout a) (Layout a) deriving (Eq, Show)

-- | Some nice combinators
put = An
($$) = Above
(<>) = Besides

-- | Models plots
data Plot = Line Array | Scatter Array Array | Bar Array | Table [String] Array | Text String deriving (Eq, Show)

-- | Compile the representations to strings that can be used on the web and in matplotlib
compileMatplotlib, compileWeb :: Layout Plot -> String
compileMatplotlib = undefined
compileWeb = undefined

-- | Compute the granularity of the grid required by a layout
gridSize :: Layout a -> (Int, Int)
gridSize (An a)       = (1, 1)
gridSize (Above l l') = (x+x', max y y')
    where
        (x, y) = gridSize l
        (x', y') = gridSize l'
gridSize (Besides l l') = (max x x', y+y')
    where
        (x, y) = gridSize l
        (x', y') = gridSize l'

-- | Compute the column and row spans of the layout elements
spans :: Layout a -> Layout (a, (Int, Int))
spans layout = spansHelper (gridSize layout) layout
    where
        spansHelper (x, y) (An a)       = An (a, (x, y))
        spansHelper (x, y) (Above l l') = Above (spansHelper (x, y `div` 2) l) (spansHelper (x, y `div` 2) l')
        spansHelper (x, y) (Besides l l') = Besides (spansHelper (x`div`2, y) l) (spansHelper (x`div`2, y) l')
