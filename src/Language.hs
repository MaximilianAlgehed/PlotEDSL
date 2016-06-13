{-# LANGUAGE DeriveFoldable #-}
import Data.Foldable

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
data Layout a = An a | Above (Layout a) (Layout a) | Besides (Layout a) (Layout a) deriving (Eq, Show, Foldable)

-- | Some nice combinators
plot = An
empty = plot (Text "")
($$) = Above
(<>) = Besides

-- | Models plots
data Plot = Line Array | Scatter Array Array | Bar Array | Table [String] Array | Text String deriving (Eq, Show)

-- | Compile the representations to strings that can be used on the web and in matplotlib
compileMatplotlib :: Layout Plot -> String
compileMatplotlib layout = unlines $
                           ["import matplotlib.pyplot as plt",
                            "import numpy as np",
                            "from matplotlib.backends.backend_pdf import PdfPages",
                            "def plot(arg, pdf):"]
                            ++ (map ("    "++) $
                            ["plt.figure()"]++(compileLayout (gridSize layout) (toList (positionsAndSpans layout)))
                            ++ ["pdf.savefig()", "plt.close()"])
    where
        compileLayout _ [] = []
        compileLayout grid ((plt, pos, spans):xs) =
            ["plt.subplot2grid("++(show grid)++","++(show pos)++",rowspan="++(show (fst spans))++",colspan="++(show (snd spans))++")"]
            ++
            (compileMatplotlibPlot plt)
            ++
            (compileLayout grid xs)

-- | Compile a layout to a string representing it's javascript and a string representing it's html table
compileWeb :: Layout Plot -> (String, String)
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
gridSize (Above l l')   = (y+y', max x x')
    where
        (y, x) = gridSize l
        (y', x') = gridSize l'
gridSize (Besides l l') = (max y y', x+x')
    where
        (y, x) = gridSize l
        (y', x') = gridSize l'

-- | Compute the column and row spans of the layout elements
spans :: Layout a -> Layout (a, (Int, Int))
spans layout = spansHelper (gridSize layout) layout
    where
        spansHelper p (An a)       = An (a, p)
        spansHelper (y, x) (Above l l') = Above (spansHelper (y `div` 2, x) l) (spansHelper (y `div` 2, x) l')
        spansHelper (y, x) (Besides l l') = Besides (spansHelper (y, x`div`2) l) (spansHelper (y, x`div`2) l')

-- | Compute the position of a layout element
positionsAndSpans :: Layout a -> Layout (a, (Int, Int), (Int, Int))
positionsAndSpans layout = positionsHelper (0, 0) (spans layout)
    where
        height (An (a, (y, _))) = y
        height (Besides l l') = max (height l) (height l')
        height (Above l l') = (height l) + (height l')

        width (An (a, (_, x))) = x
        width (Besides l l') = (width l) + (width l')
        width (Above l l') = max (width l) (width l')

        positionsHelper p (An (a, ps)) = An (a, p, ps)
        positionsHelper (y, x) (Above l l') = Above (positionsHelper (y, x) l) (positionsHelper (y+(height l), x) l')
        positionsHelper (y, x) (Besides l l') = Besides (positionsHelper (y, x) l) (positionsHelper (y, x+(width l)) l')
