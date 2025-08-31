{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Prelude hiding (value)
import Diagrams.Backend.SVG (B, renderSVG)
import Options.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.List as L

data CLIOptions = CLIOptions
  { optThickness :: Double
  , optRows      :: Maybe Int
  , optCols      :: Maybe Int
  , optCellW     :: Maybe Double
  , optCellH     :: Maybe Double
  , optDepth     :: Double
  , optAreaW     :: Maybe Double
  , optAreaH     :: Maybe Double
  , optOutput    :: FilePath
  }

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions
  <$> option auto (long "thickness" <> short 't' <> value 3 <> help "Thickness of wood panels")
  <*> optional (option auto (long "rows" <> short 'r' <> help "Number of rows"))
  <*> optional (option auto (long "cols" <> short 'c' <> help "Number of columns"))
  <*> optional (option auto (long "cell-width"  <> short 'w' <> help "Width of a cell"))
  <*> optional (option auto (long "cell-height" <> short 'h' <> help "Height of a cell"))
  <*> option auto (long "depth" <> short 'd' <> value 200 <> help "Depth of the shelf")
  <*> optional (option auto (long "area-width"  <> help "Cutting area width"))
  <*> optional (option auto (long "area-height" <> help "Cutting area height"))
  <*> strOption (long "output" <> short 'o' <> value "shelf.svg" <> help "Output SVG filename")

optsInfo :: ParserInfo CLIOptions
optsInfo = info (optionsParser <**> helper)
  (fullDesc <> progDesc "Generate shelf cutting plans" <> header "mini-bookshelf-scad")

-- | Concrete parameters after filling defaults and guesses
data Params = Params
  { pThickness :: Double
  , pRows      :: Int
  , pCols      :: Int
  , pCellW     :: Double
  , pCellH     :: Double
  , pDepth     :: Double
  , pAreaW     :: Maybe Double
  , pAreaH     :: Maybe Double
  , pOutput    :: FilePath
  }

fillDefaults :: CLIOptions -> Params
fillDefaults o = Params
  { pThickness = optThickness o
  , pRows      = fromMaybe 2 (optRows o)
  , pCols      = fromMaybe 3 (optCols o)
  , pCellW     = fromMaybe 100 (optCellW o)
  , pCellH     = fromMaybe 100 (optCellH o)
  , pDepth     = optDepth o
  , pAreaW     = optAreaW o
  , pAreaH     = optAreaH o
  , pOutput    = optOutput o
  }

-- | Individual panel with name, width and height
computePanels :: Params -> [(String, Double, Double)]
computePanels p = concat
  [ replicate 1 ("Back", totalW, totalH)
  , replicate 1 ("Top", totalW, pDepth p)
  , replicate 1 ("Bottom", totalW, pDepth p)
  , replicate 2 ("Side", pDepth p, totalH)
  , replicate (pRows p - 1) ("Shelf", totalW - 2 * t, pDepth p)
  , replicate (pCols p - 1) ("Divider", pDepth p, totalH - 2 * t)
  ]
  where
    t = pThickness p
    totalW = fromIntegral (pCols p) * pCellW p + fromIntegral (pCols p + 1) * t
    totalH = fromIntegral (pRows p) * pCellH p + fromIntegral (pRows p + 1) * t

-- | Layout panels into cutting areas
layout :: Maybe Double -> Maybe Double -> Double -> [(String, Double, Double)] -> [Diagram B]
layout mAw mAh gap panels = map (makeSheet aw ah) (pack aw ah gap panels)
  where
    aw = fromMaybe (1/0) mAw
    ah = fromMaybe (1/0) mAh

pack :: Double -> Double -> Double -> [(String, Double, Double)]
     -> [[(String, Double, Double, Double, Double)]]
pack aw ah gap = go 0 0 0 [] []
  where
    go _ _ _ current acc [] = reverse (reverse current : acc)
    go x y rowH current acc ((n,w,h):ps)
      | w > aw || h > ah = error ("Panel " ++ n ++ " exceeds cutting area")
      | x + w <= aw =
          let item = (n,w,h,x,y)
          in go (x + w + gap) y (max rowH h) (item:current) acc ps
      | y + rowH + h <= ah =
          go 0 (y + rowH + gap) 0 current acc ((n,w,h):ps)
      | otherwise =
          go 0 0 0 [] (reverse current : acc) ((n,w,h):ps)

makeSheet :: Double -> Double -> [(String, Double, Double, Double, Double)] -> Diagram B
makeSheet aw ah items = border <> mconcat (map draw items)
  where
    border
      | isInfinite aw || isInfinite ah = mempty
      | otherwise = rect aw ah # lwO 0.3 # lc red
    draw (name,w,h,x,y) =
      let r = rect w h # lwO 0.5 # lc black
          label = text name # fontSizeL (min w h / 5)
      in translate (r2 (x + w / 2, -(y + h / 2))) (r <> label)

main :: IO ()
main = do
  opts <- execParser optsInfo
  let params = fillDefaults opts
      panels = computePanels params
      sheets = layout (pAreaW params) (pAreaH params) (pThickness params) panels
      finalDiagram = vsep (pThickness params) sheets
  renderSVG (pOutput params) (mkWidth 1000) finalDiagram
