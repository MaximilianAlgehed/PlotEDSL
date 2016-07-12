{-# LANGUAGE DeriveFoldable #-}
module Plot.Language where
import Data.Foldable
import Control.Monad.State

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
data Layout a = An a String | Above (Layout a) (Layout a) | Besides (Layout a) (Layout a) deriving (Eq, Show)

toList' (An a s) = [(a, s)]
toList' (Above x y) = toList' x ++ toList' y
toList' (Besides x y) = toList' x ++ toList' y

-- | Some nice combinators
plot = An
empty = plot (Text "")
($$) = Above
(<>) = Besides

-- | Models plots
data Plot = Line Array | Scatter Array Array | Bar Array | Table Array | Text String deriving (Eq, Show)

-- | Compile the representations to strings that can be used on the web and in matplotlib
compileMatplotlib :: Layout Plot -> String
compileMatplotlib layout = unlines $
                           ["import matplotlib.pyplot as plt",
                            "import numpy as np",
                            "from matplotlib.backends.backend_pdf import PdfPages",
                            "def plot(arg, pdf):"]
                            ++ (map ("    "++) $
                            ["plt.figure()"]++(compileLayout (gridSize layout) (toList' (positionsAndSpans layout)))
                            ++ ["pdf.savefig()", "plt.close()"])
    where
        compileLayout _ [] = []
        compileLayout grid (((plt, pos, spans),s):xs) =
            ["plt.subplot2grid("++(show grid)++","++(show pos)++",rowspan="++(show (fst spans))++",colspan="++(show (snd spans))++")"]
            ++
            ["plt.title('"++s++"')"]
            ++
            (compileMatplotlibPlot plt)
            ++
            (compileLayout grid xs)

-- | Compile a layout to a string representing it's javascript and a string representing it's html table
-- | wrapped in a state monad to make sure every 
compileWeb :: Layout Plot -> String -> State Int (String, String)
compileWeb p s = compileWebHelper (toList' (positionsAndSpans p))
    where
        compileWebHelper :: [((Plot, (Int, Int), (Int, Int)), String)] -> State Int (String, String)
        compileWebHelper ps = do
            i <- get -- The first reference point
            put (i+length ps) -- The last reference point+1
            let htmlTable = unlines $ zipWith htmlhelper ps [i..]
            let js = unlines $ zipWith jshelper ps [i..] 
            return (js, htmlPreamble++htmlTable++htmlPostamble)

        htmlPreamble = ""
        htmlPostamble = ""
        htmlhelper ((Text txt, _, _), _) _ = "<p>"++txt++"</p>"
        htmlhelper _ i = "<div id='"++s++(show i)++"' style='width 900px; height=500px'><img src='resources/gears.gif'></img></div>"

        jshelper ((plt, _, _), t) i = compileWebPlot plt i s t

-- | Compile a plot
compileWebPlot :: Plot -> Int -> String -> String -> String
compileWebPlot (Line arr) i n t = "plotLine("++(compileWebArray arr)++",document.getElementById('"++n++(show i)++"'),'"++t++"')"
compileWebPlot (Scatter arr arrr) i n t = "plotScatter("++(compileWebArray arr)++","++(compileWebArray arrr)++",document.getElementById('"++n++(show i)++"'),'"++t++"')"
compileWebPlot (Bar arr) i n t = "plotBar("++(compileWebArray arr)++",document.getElementById('"++n++(show i)++"'),'"++t++"')"
compileWebPlot (Text _) _ _ _ = ""
compileWebPlot (Table a) i n t = "plotTable("++(compileWebArray a)++",document.getElementById('"++n++(show i)++"'),'"++t++"')"

-- | Compile an index
compileWebIndex :: Index -> String
compileWebIndex (Direct i) = show i
compileWebIndex (Length arr) = (compileWebArray arr)++".length"
compileWebIndex (P indx indxx) = "("++(compileWebIndex indx) ++ "+" ++ (compileWebIndex indxx)++")"
compileWebIndex (M indx indxx) = "("++(compileWebIndex indx) ++ "-" ++ (compileWebIndex indxx)++")"
compileWebIndex (T indx indxx) = "("++(compileWebIndex indx) ++ "-" ++ (compileWebIndex indxx)++")"

-- | Compile an array
compileWebArray :: Array -> String
compileWebArray Argument = "data"
compileWebArray (IndexOf arr indx) = (compileWebArray arr) ++ "["++(compileWebIndex indx)++"]"
compileWebArray (Range indx indxx arr) = (compileWebArray arr) ++ ".slice("++(compileWebIndex indx)++","++(compileWebIndex indxx)++")"

-- | Compile a plot
compileMatplotlibPlot :: Plot -> [String]
compileMatplotlibPlot (Line arr) = ["plt.plot("++(compileMatplotlibArray arr)++")"]
compileMatplotlibPlot (Scatter arr arrr) = ["plt.scatter("++(compileMatplotlibArray arr)++","++(compileMatplotlibArray arrr)++")"]
compileMatplotlibPlot (Bar arr) = ["plt.bar(range(len("++array++")),"++array++",0.5)",
                                   "plt.xticks(np.array(range(len("++array++")))+0.25, np.array(range(len("++array++"))))"]
    where
        array = compileMatplotlibArray arr
compileMatplotlibPlot (Table _) = []
compileMatplotlibPlot (Text s) = ["plt.axis('off')", "plt.text(0, 1, "++(show s)++")"]

-- | Compile an index into a matplotlib string
compileMatplotlibIndex :: Index -> String
compileMatplotlibIndex (Direct i)     = show i
compileMatplotlibIndex (Length arr)   = "len("++(compileMatplotlibArray arr)++")"
compileMatplotlibIndex (P indx indxx) = "("++(compileMatplotlibIndex indx) ++ "+" ++ (compileMatplotlibIndex indxx)++")"
compileMatplotlibIndex (M indx indxx) = "("++(compileMatplotlibIndex indx) ++ "-" ++ (compileMatplotlibIndex indxx)++")"
compileMatplotlibIndex (T indx indxx) = "("++(compileMatplotlibIndex indx) ++ "*" ++ (compileMatplotlibIndex indxx)++")"

-- | Compile an array into a matplotlib string
compileMatplotlibArray :: Array -> String
compileMatplotlibArray Argument               = "arg"
compileMatplotlibArray (IndexOf arr indx)     = (compileMatplotlibArray arr) ++ "["++(compileMatplotlibIndex indx)++"]"
compileMatplotlibArray (Range indx indxx arr) =
    (compileMatplotlibArray arr) ++ "["++(compileMatplotlibIndex indx)++":"++(compileMatplotlibIndex indxx)++"]"

-- | Compute the granularity of the grid required by a layout
gridSize :: Layout a -> (Int, Int)
gridSize (An _ _)         = (1, 1)
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
        spansHelper p (An a s)       = An (a, p) s
        spansHelper (y, x) (Above l l') = Above (spansHelper (y `div` 2, x) l) (spansHelper (y `div` 2, x) l')
        spansHelper (y, x) (Besides l l') = Besides (spansHelper (y, x`div`2) l) (spansHelper (y, x`div`2) l')

-- | Compute the position of a layout element
positionsAndSpans :: Layout a -> Layout (a, (Int, Int), (Int, Int))
positionsAndSpans layout = positionsHelper (0, 0) (spans layout)
    where
        height (An (a, (y, _))_ ) = y
        height (Besides l l') = max (height l) (height l')
        height (Above l l') = (height l) + (height l')

        width (An (a, (_, x)) _) = x
        width (Besides l l') = (width l) + (width l')
        width (Above l l') = max (width l) (width l')

        positionsHelper p (An (a, ps) s) = An (a, p, ps) s
        positionsHelper (y, x) (Above l l') = Above (positionsHelper (y, x) l) (positionsHelper (y+(height l), x) l')
        positionsHelper (y, x) (Besides l l') = Besides (positionsHelper (y, x) l) (positionsHelper (y, x+(width l)) l')

compileAndSave :: String -> Layout Plot -> IO ()
compileAndSave name plt = do
                       let (js, html) = fst $ runState (compileWeb plt name) 0
                       let py = compileMatplotlib plt
                       writeFile (name++".js") js
                       writeFile (name++".html") html
                       writeFile (name++".py") py
