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
len = Length
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

-- | Compile a plot
compileMatplotlibPlot :: Plot -> [String]
compileMatplotlibPlot (Line arr) = ["plt.plot("++(compileMatplotlibArray arr)++")"]
compileMatplotlibPlot (Scatter arr arrr) = ["plt.scatter("++(compileMatplotlibArray arr)++","++(compileMatplotlibArray arrr)++")"]
compileMatplotlibPlot (Bar arr) = ["plt.bar(range(len("++array++")),"++array++"0.5)",
                                   "plt.xticks(np.array(range(len("++array++")))+0.25, np.array(range(len("++array++"))))"]
    where
        array = compileMatplotlibArray arr
compileMatplotlibPlot (Table _ _) = undefined
compileMatplotlibPlot (Text s) = ["plt.text(0, 1, "++(show s)++")"]

-- | Compile an index into a matplotlib string
compileMatplotlibIndex :: Index -> String
compileMatplotlibIndex (Direct i)     = show i
compileMatplotlibIndex (Length arr)   = "len("++(compileMatplotlibArray arr)++")"
compileMatplotlibIndex (P indx indxx) = (compileMatplotlibIndex indx) ++ "+" ++ (compileMatplotlibIndex indxx)
compileMatplotlibIndex (M indx indxx) = (compileMatplotlibIndex indx) ++ "-" ++ (compileMatplotlibIndex indxx)
compileMatplotlibIndex (T indx indxx) = (compileMatplotlibIndex indx) ++ "*" ++ (compileMatplotlibIndex indxx)

-- | Compile an array into a matplotlib string
compileMatplotlibArray :: Array -> String
compileMatplotlibArray Argument               = "arg"
compileMatplotlibArray (IndexOf arr indx)     = (compileMatplotlibArray arr) ++ "["++(compileMatplotlibIndex indx)++"]"
compileMatplotlibArray (Range indx indxx arr) =
    (compileMatplotlibArray arr) ++ "["++(compileMatplotlibIndex indx)++":"++(compileMatplotlibIndex indxx)++"]"

-- | Compute the granularity of the grid required by a layout
gridSize :: Layout a -> (Int, Int)
gridSize (An a)         = (1, 1)
gridSize (Above l l')   = (x+x', max y y')
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
        spansHelper p (An a)       = An (a, p)
        spansHelper (x, y) (Above l l') = Above (spansHelper (x, y `div` 2) l) (spansHelper (x, y `div` 2) l')
        spansHelper (x, y) (Besides l l') = Besides (spansHelper (x`div`2, y) l) (spansHelper (x`div`2, y) l')

-- | Compute the position of a layout element
positions :: Layout a -> Layout (a, (Int, Int))
positions layout = positionsHelper (0, 0) (spans layout)
    where
        height (An (a, (_, y))) = y
        height (Besides l l') = max (height l) (height l')
        height (Above l l') = (height l) + (height l')

        width (An (a, (x, _))) = x
        width (Besides l l') = (width l) + (width l')
        width (Above l l') = max (width l) (width l')

        positionsHelper p (An (a, _)) = An (a, p)
        positionsHelper (x, y) (Above l l') = Above (positionsHelper (x, y) l) (positionsHelper (x, y+(height l)) l')
        positionsHelper (x, y) (Besides l l') = Besides (positionsHelper (x, y) l) (positionsHelper (x+(width l), y) l')
