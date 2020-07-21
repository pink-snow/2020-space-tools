#! /usr/bin/env nix-shell
-- Usage: ./annotate-pf.hs in-msg.png out-annotated.svg out-decoded.txt
#! nix-shell -i runhaskell -p
#! nix-shell "haskellPackages.ghcWithPackages (pkgs: with pkgs; [JuicyPixels JuicyPixels-util errors extra groupBy])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/5cb5ccb54229efd9a4cd1fccc0f43e0bbed81c5d.tar.gz

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Error.Safe (justZ)
import Control.Monad (forM, forM_, guard, when)
import Control.Monad.Extra (allM, (||^))
import Control.Monad.ST (runST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (foldl', intercalate, sortOn, find)
import Data.List.GroupBy (groupBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.STRef
import Data.Word (Word16)
import System.Environment (getArgs)
import qualified Codec.Picture as P
import qualified Codec.Picture.RGBA8 as P8
import qualified Data.Vector as Vi
import qualified Data.Vector.Mutable as V

--------------------------------------------------------------------------------
-- Types

type Scale = Int
type Coord = (Int, Int)
type Size = (Int, Int)

data Img = Img
  { imgVec :: Vi.Vector Bool
  , imgSize :: Size
  }

data Symbol
  = SymNumber Integer
  | SymOperator Integer
  | SymVariable Integer
  | SymSomething Integer
  | SymUnknown

--------------------------------------------------------------------------------
-- Misc functions

range2d :: Int -> Int -> Int -> Int -> [(Int, Int)]
range2d x0 y0 x1 y1 = [(x', y') | y' <- [y0 .. y1], x' <- [x0 .. x1]]

none :: (a -> Bool) -> [a] -> Bool
none = (not .) . any

bitsToInteger :: [Bool] -> Integer
bitsToInteger = fst . foldl' f (0, 1)
  where
    f (sum, bit) True = (sum + bit, bit*2)
    f (sum, bit) False = (sum, bit*2)

groupAcc :: (a -> s) -> (s -> a -> Maybe s) -> [a] -> [(s, [a])]
groupAcc init f = groupAcc1'
  where
    groupAcc1' [] = []
    groupAcc1' (x:xs) = takeGroup (init x) [x] xs

    takeGroup state group [] = [(state, reverse group)]
    takeGroup state group (y:ys) = case f state y of
      Nothing -> (state, reverse group) : groupAcc1' (y:ys)
      Just state' -> takeGroup state' (y : group) ys

--------------------------------------------------------------------------------
-- Img

imgLoad :: FilePath -> IO Img
imgLoad path = do
  img <- P8.readImageRGBA8 path
  let size@(w, h) = (P.imageWidth img, P.imageHeight img)
  let vec = Vi.generate (w*h) (\i -> imgPixelUnscaled img (mod i w) (div i w))
  return $ Img vec size

imgWidth :: Img -> Int
imgWidth (Img _ (w, _)) = w

imgHeight :: Img -> Int
imgHeight (Img _ (_, h)) = h

imgPixel :: Img -> Coord -> Bool
imgPixel (Img vec (w, h)) (x, y)
  | x < 0 || y < 0 || x >= w || y >= h = False
  | otherwise = vec Vi.! (x + y*w)

imgPixelUnscaled :: (P.Image P.PixelRGBA8) -> Int -> Int -> Bool
imgPixelUnscaled img x y = True
  && x >= 0 && y >= 0
  && x < P.imageWidth img && y < P.imageHeight img
  && fromIntegral r + fromIntegral g + fromIntegral b > (0::Word16)
  where P.PixelRGBA8 r g b _ = P.pixelAt img x y

imgShow :: Img -> [Int] -> [Int] -> String
imgShow img xs ys = unlines $ map showLine ys
  where showLine y = map (\x -> if imgPixel img (x, y) then '#' else '.') xs

imgShowFull :: Img -> String
imgShowFull img = imgShow img [0 .. imgWidth img - 1] [0 .. imgHeight img - 1]

instance Show Img where
  show = imgShowFull

imgAllPixels :: Img -> [Coord]
imgAllPixels img = range2d 0 0 (imgWidth img - 1) (imgHeight img - 1)

--------------------------------------------------------------------------------
-- Img manipulation

imgRemoveGarbage :: Img -> Img
imgRemoveGarbage img =
  foldl imgFloodFill img (imgAllPixels img)

imgFloodFill :: Img -> Coord -> Img
imgFloodFill img (sx, sy)
  | not $ imgPixel img (sx, sy) = img
  | otherwise = runST $ do
    vec <- Vi.thaw (imgVec img)
    bounds <- newSTRef (sx, sy, sx, sy)
    let
      put x y
        | x < 0 || y < 0 || x >= imgWidth img || y >= imgHeight img = return ()
        | otherwise = do
          v <- V.read vec (x + y * (imgWidth img))
          when v $ do
            V.write vec (x + y * (imgWidth img)) False
            modifySTRef' bounds $ \(x0, y0, x1, y1) ->
              (min x0 x, min y0 y, max x1 x, max y1 y)
            put (x-1) (y-1); put (x  ) (y-1); put (x+1) (y-1)
            put (x-1) (y  );                  put (x+1) (y  )
            put (x-1) (y+1); put (x  ) (y+1); put (x+1) (y+1)
    put sx sy
    (x0, y0, x1, y1) <- readSTRef bounds

    if max (x1 - x0 + 1) (y1 - y0 + 1) >= 5 || (x0 == x1 && y0 == y1)
    then do
      vec' <- Vi.freeze vec
      return $ img { imgVec = vec' }
    else
      return $ img

imgExtractElectrons :: Img -> (Img, [Coord])
imgExtractElectrons img = runST $ do
  vec <- Vi.thaw (imgVec img)
  let idx x y
       | x < 0 || y < 0 || x >= imgWidth img || y >= imgHeight img = Nothing
       | otherwise = Just (x + y * (imgWidth img))
  let px x y = case idx x y of
         Nothing -> return False
         Just idx -> V.read vec idx
  let pxw x y = case idx x y of
        Nothing -> return ()
        Just idx -> V.write vec idx False
  let
    extractPattern (x, y) pattern = do
      p <- allM (\((x', y'), val) -> (==val) <$> px (x + x') (y + y')) pattern
      when p $ mapM_ (\((x', y'), _) -> pxw (x+x') (y+y')) pattern
      return p
  a <- fmap catMaybes $ forM (imgAllPixels img) $ \coord -> do
    res <- extractPattern coord majorElectron ||^ extractPattern coord minorElectron
    return $ if res then Just coord else Nothing
  vec' <- Vi.freeze vec
  return $ (img { imgVec = vec' }, a)

majorElectron = parsePattern
  [ "... "
  , ".#.."
  , "..#."
  , " ..."
  ]

minorElectron = parsePattern
  [ " ..."
  , "..#."
  , ".#.."
  , "... "
  ]

parsePattern :: [String] -> [(Coord, Bool)]
parsePattern = catMaybes . concat . zipWith line [0..]
  where
    chr y x '#' = Just ((x, y), True)
    chr y x '.' = Just ((x, y), False)
    chr _ _ _ = Nothing
    line y = zipWith (chr y) [0..]
    lines = catMaybes . concat . zipWith line [0..]

--------------------------------------------------------------------------------
-- Symbol decoder

symDecode :: Img -> Coord -> Size -> Symbol
symDecode img (x, y) (w, h)
  | isNonNegativeNumber = SymNumber value
  | isNegativeNumber = SymNumber (-value)
  | isSomething = SymSomething somethingValue
  | isVariable = SymVariable varValue
  | isOperator = SymOperator value
  | otherwise = SymUnknown
  where
    size = w

    px (x', y') = imgPixel img (x + x', y + y')

    isNonNegativeNumber = True
      && w == h
      && not (px (0, 0))

    isNegativeNumber = True
      && w + 1 == h
      && not (px (0, 0))
      && px (0, size)
      && none px [(x', size) | x' <- [1 .. size-1]] -- bottom + 1 is empty

    isOperator = True
      && w == h
      && px (0, 0)

    isVariable = True
      && w == h
      && px (1, 1)
      && all px [(x',     size-1) | x' <- [0 .. size-1]] -- bottom is full
      && all px [(size-1, y')     | y' <- [0 .. size-1]] -- right is full
      && none px [(x', 1) | x' <- [2 .. size-2]] -- top + 1 is empty
      && none px [(1, y') | y' <- [2 .. size-2]] -- left + 1 is empty

    isSomething = True
      && w == h
      && px (0, 0)
      && not (px (1, 0))

    value = bitsToInteger $ map px $ range2d 1 1 (size-1) (size-1)

    varValue = bitsToInteger $ map (not . px) $ range2d 2 2 (size-2) (size-2)

    somethingValue = bitsToInteger $ map px $ range2d 0 0 (size-1) (size-1)

symDetectAll :: Img -> [(Coord, Size)]
symDetectAll img = runST $ do
  vec <- V.replicate (width * height) False
  fmap catMaybes $ forM validRange $ \(x, y) -> runMaybeT $ do
    guard =<< not <$> V.read vec (idx (x, y))
    (w, h) <- justZ $ symDetectSingle img (x, y)
    lift $ forM_ (range2d x y (x+w-1) (y+h-1)) $ write vec
    return ((x, y), (w, h))
  where
    (width, height) = (imgWidth &&& imgHeight) img
    validRange = range2d 2 2 (width - 3) (height - 3)
    idx (x, y) = x + y * width
    write vec (x, y)
      | x >= width || y >= height = return ()
      | otherwise = V.write vec (idx (x, y)) True >> return ()

symDetectSingle :: Img -> Coord -> Maybe Size
symDetectSingle img (x, y)
  | ok1 = Just (width, height)
  | ok2 = Just (ok2Size, ok2Size)
  | otherwise = Nothing
  where
    px x' y' = imgPixel img (x + x', y + y')
    width = (+1) $ length $ takeWhile (flip px 0) [1 ..]
    height = (+1) $ length $ takeWhile (px 0) [1 ..]

    noBorders w h = True
      && none (\y -> px w y || px (-1) y) [0..h-1]
      && none (\x -> px x h || px x (-1)) [0..w-1]

    ok1 = px 1 0 && px 0 1 && noBorders width height
      && (width > 2 || px 1 1)

    ok2Have i = any (px i) [0..i] || any (flip px i) [0..i]
    ok2Size = length $ takeWhile ok2Have [0..]
    ok2 = px 0 0 && px 1 1 && not (px 0 1) && not (px 1 0)
      && ok2Size <= 4
      && ok2Size > 2
      && x + ok2Size < imgWidth img
      && y + ok2Size < imgHeight img
      && none (px (-1)) [0..ok2Size-1]
      && none (flip px (-1)) [0..ok2Size-1]

symEllipsis :: [[Bool]]
symEllipsis = map (map (=='#'))
  [ "........."
  , ".#.#.#.#."
  , "........."
  ]

checkLine :: Img -> [Bool] -> Coord -> Bool
checkLine _ [] _ = True
checkLine img (h:t) (x, y) = (imgPixel img (x, y) == h) && checkLine img t (x+1, y)

checkSymbol :: Img -> [[Bool]] -> Coord -> Bool
checkSymbol _ [] _ = True
checkSymbol img (h:t) (x, y) = checkLine img h (x, y) && checkSymbol img t (x, y+1)

--------------------------------------------------------------------------------
-- svg

svg :: Img -> [(Coord, Size, String, String)] -> String
svg img annotations =
  concat (
    svgHead img ++
    svgImgPoints img ++
    concatMap (\(coord, size, text, color) -> svgAnnotation coord size text color) annotations ++
    ["</svg>"]
  )

svgHead :: Img -> [String]
svgHead img = [
    "<svg xmlns='http://www.w3.org/2000/svg' version='1.1' width='",
    show $ 1 + imgWidth img * 8,
    "' height='",
    show $ 1 + imgHeight img * 8,
    "'>\n",
    "<rect width='100%' height='100%' style='fill:black'/>\n"
  ]

svgPoint :: Coord -> Bool -> [String]
svgPoint (x, y) value = [
    "<rect x='", show (1 + x*8),
    "' y='", show (1 + y*8),
    "' width='7' height='7' style='fill:",
    if value then "white" else "#333333",
    "'/>\n"
  ]

svgImgPoints :: Img -> [String]
svgImgPoints img =
  concatMap (\coord -> svgPoint coord (imgPixel img coord)) $ imgAllPixels img

svgAnnotation :: Coord -> Size -> String -> String -> [String]
svgAnnotation (x, y) (w, h) text color =
  case words text of
    [text1] -> svgAnnotation2 (x, y) (w, h) text1 "" color
    text1:rest -> svgAnnotation2 (x, y) (w, h) text1 (unwords rest) color


svgAnnotation2 :: Coord -> Size -> String -> String -> String -> [String]
svgAnnotation2 (x, y) (w, h) text1 text2 color = [
    "<rect x='", show (1 + x*8 - 2),
    "' y='", show (1 + y*8 - 2),
    "' width='", show (w*8 + 3),
    "' height='", show (h*8 + 3),
    "' style='fill:", color, ";opacity:0.5'/>\n"]
    ++ if text2 == ""
         then putText text1 0
         else putText text1 (-4) ++ putText text2 8
  where
    putText t yShift = [
      "<text x='", show (1 + x*8 + w*4),
      "' y='", show (1 + y*8 + h*4 + yShift),
      "' dominant-baseline='middle' text-anchor='middle' fill='white' style='",
      "paint-order: stroke; fill: white; stroke: black; stroke-width: 2px; font:",
      show $ (w * 15) `div` (length t)
      ,"px bold sans;",
      "'>", t, "</text>\n" ]

--------------------------------------------------------------------------------
-- Main

annotateImg :: Img -> String
annotateImg img = id
  $ svg img
  $ map (symRepr' img)
  $ symDetectAll img'
  where img' = (fst . imgExtractElectrons) $ imgRemoveGarbage img

decodeImg :: Img -> String
decodeImg img = id
    $ unlines
    $ map (intercalate "   ") -- join groups
    $ map (map (intercalate " ")) -- join items inside each group
    $ map (map (map (\(_, _, text, _) -> text)))
    $ map (groupBy (\a b -> xRight a >= xLeft b - 2)) -- split by horisontal groups
    $ splitByLines
    $ map (symRepr' img)
    $ symDetectAll img'
  where
    xLeft ((x, _), _, _, _) = x
    xRight ((x, _), (w, _), _, _) = x + w
    img' = (fst . imgExtractElectrons) $ imgRemoveGarbage img

splitByLines :: [(Coord, Size, a, b)] -> [[(Coord, Size, a, b)]]
splitByLines = id
  . map (sortOn (\((x, _), _, _, _) -> x))
  . map concat
  . map (map snd . snd) -- drop accumulators from both groupAcc's
  . groupAcc fst (\s x -> addRanges s (fst x))
  . groupAcc yRange (\s x -> addRanges s (yRange x))
  where
    yRange ((_, y), (_, h), _, _) = (y, y+h)
    addRanges (a0, a1) (b0, b1)
      | b0 <= a0 && a0 <= b1 = Just (b0, max a1 b1)
      | a0 <= b0 && b0 <= a1 = Just (a0, max a1 b1)
      | otherwise = Nothing

symRepr :: Symbol -> (String, String)
symRepr SymUnknown = ("?", "gray")
symRepr (SymNumber val) = (text, "green")
  where
    text = fromMaybe (show val) $ lookup val elementIdentifiers
symRepr (SymOperator val) = fromMaybe (':' : show val, "yellow") $ lookup val $
  map (\(a,b,c) -> (a, (b ++ " " ++ c, "yellow"))) moleculeIdentifiers
  ++ map (\(a, b) -> (a, (b, "orange"))) aminoAcidIdentifiers

symRepr (SymVariable val) = ('x' : show val, "blue")

symRepr (SymSomething 341) = ("H-Bond", "red")
symRepr (SymSomething 7077) = ("Amino acid", "red")

symRepr (SymSomething 49953) = ("²H Deuterium", "red")
symRepr (SymSomething 50145) = ("³H Tritium", "red")

symRepr (SymSomething 20193) = ("A", "red")
symRepr (SymSomething 20461) = ("A0", "red")
symRepr (SymSomething 17641) = ("A1", "red")
symRepr (SymSomething 58593) = ("A2", "red")

symRepr (SymSomething 44769) = ("B", "red")
symRepr (SymSomething 45037) = ("B0", "red")
symRepr (SymSomething 43753) = ("B1", "red")
symRepr (SymSomething 60129) = ("B2", "red")

symRepr (SymSomething 61153) = ("○ Planet", "red")

symRepr (SymSomething val) = (show val, "red")

symRepr' :: Img -> (Coord, Size) -> (Coord, Size, String, String)
symRepr' img (coord, size) =
  (coord, size, text, color)
  where (text, color) = symRepr $ symDecode img coord size

main :: IO ()
main = do
  [fnameIn, fnameSvg, fnameTxt] <- getArgs
  img <- imgLoad fnameIn
  writeFile fnameSvg $ annotateImg img
  writeFile fnameTxt $ decodeImg img


tr :: Eq a => [a] -> [a] -> [a] -> [a]
tr from to = map tr'
  where
    table = zip from to
    tr' s = fromMaybe s (snd <$> find (\(f, _) -> s == f) table)

elementIdentifiers =
  [ ( 1, "H")
  , ( 2, "He")
  , ( 3, "Li")
  , ( 4, "Be")
  , ( 5, "B")
  , ( 6, "C")
  , ( 7, "N")
  , ( 8, "O")
  , (11, "Na")
  , (12, "Mg")
  , (13, "Al")
  , (14, "Si")
  , (17, "Cl")
  , (26, "Fe")
  ]

moleculeIdentifiers =
  map (\(a, b, c) -> (a, tr ['0'..'9'] ['₀'..'₉'] b, c))
  [ {-05:01-} ( 16, "CH4",     "methane")
  , {-05:02-} ( 17, "NH3",     "ammonia")
  , {-05:03-} ( 18, "H2O",     "water")
  , {-05:04-} ( 28, "N2",      "triple bond")
  , {-05:05-} ( 32, "O2",      "double bond")
  , {-05:06-} ( 46, "SiO2",    "silicon dioxide")
  , {-05:07-} ( 48, "O3",      "ozone")
  , {-05:08-} (328, "FeO",     "ferrous oxide")
  , {-05:09-} ( 74, "Al2O3",   "")
  , {-05:10-} ( 85, "NaNO3",   "sodium nitrate")
  , {-05:11-} ( 95, "MgCl2",   "magnesium dichloride")
  , {-05:12-} (104, "Fe2O3",   "ferric oxide")
  ]

aminoAcidIdentifiers =
  [ {-06:01-} ( 56, "a01")  -- C2H2NO   glycyl radical residue.
  , {-06:02-} ( 20, "a02")  -- C2H6N    1-aminoethyl.
  , {-06:03-} ( 29, "a03")  -- CH2-CH3  ethyl.
  , {-06:04-} ( 30, "a04")  -- CH2-NH2  1-amine-.
  , {-06:05-} ( 31, "SER")  -- CH2-OH   hydroxymethyl.
  , {-06:06-} ( 44, "THR")  -- C2H5O    1-hydroxyethyl.
  , {-06:07-} ( 45, "a07")  -- C2H5O    2-hydroxyethyl.
  , {-06:08-} ( 58, "ASN")  -- C2H4NO   2-amino-2-oxoethyl.

  , {-07:01-} ( 59, "ASP")  -- C2H3O2   carboxymethyl.
  , {-07:02-} ( 72, "GLN")  -- C3H6NO   3-amino-3-oxopropyl.
  , {-07:03-} (  9, "GLU")  -- C3H5O2   2-carboxyethyl.
  , {-07:04-} ( 84, "LEU")  -- C4H9     isobutyl.
  , {-07:05-} ( 86, "a15")  -- C3H7N3   2-carbamimidamidoethyl.
  , {-07:06-} ( 87, "a16")  -- C4H7O2   butanoic acid.
  , {-07:07-} (100, "ARG")  -- C4H9N3   3-carbamimidamidopropyl.
  , {-07:08-} (118, "PHE")  -- C7H7     benzyl group.
  , {-07:09-} (134, "TYR")  -- C7H7O    4-hydroxybenzyl.
  ]
