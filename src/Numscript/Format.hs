{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Numscript.Format (format) where

import qualified Data.List as List
import Data.Ratio (denominator, numerator)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Numscript
import Prelude hiding (lines)

format :: Numscript.Program -> Text
format = T.intercalate "\n\n" . map (render . fmtStatement)

fmtBlock :: [Doc] -> Doc
fmtBlock docs =
  concatDocs
    [ "{"
    , nest docs
    , Newline <> "}"
    ]

fmtStatement :: Numscript.Statement -> Doc
fmtStatement statement = case statement of
  Numscript.Send amt src dest ->
    concatDocs
      [ "send " <> fmtMonetary amt <> " ("
      , nest
          [ "source = " <> fmtSrc src
          , "destination = " <> fmtDest dest
          ]
      , Newline <> ")"
      ]

fmtMonetary :: Numscript.Monetary -> Doc
fmtMonetary (Numscript.Monetary asset amt) =
  Text $ "[" <> asset <> " " <> showText amt <> "]"

fmtAccount :: Text -> Doc
fmtAccount txt = Text $ T.cons '@' txt

fmtPortion :: Rational -> Doc
fmtPortion rat =
  Text $
    showText (numerator rat) <> "/" <> showText (denominator rat)

showText :: (Show a) => a -> Text
showText = T.pack . show

fmtSrc :: Numscript.Source -> Doc
fmtSrc src = case src of
  Numscript.SrcAccount x -> fmtAccount x
  Numscript.SrcCapped monetary src' ->
    "max " <> fmtMonetary monetary <> " from " <> fmtSrc src'
  Numscript.SrcAccountOverdraft addr Nothing ->
    fmtAccount addr <> " allowing unbounded overdraft"
  Numscript.SrcAccountOverdraft addr (Just maxOverdraft) ->
    fmtAccount addr <> " allowing overdraft up to " <> fmtMonetary maxOverdraft
  Numscript.SrcInorder sources -> fmtBlock $ map fmtSrc sources
  Numscript.SrcAllotment clauses -> fmtBlock $ map fmtAllotmentSrc clauses

fmtAllotmentSrc :: Numscript.AllotmentClause Numscript.Source -> Doc
fmtAllotmentSrc (Numscript.AllotmentClause portion src) =
  fmtPortion portion <> " from " <> fmtSrc src

fmtDest :: Numscript.Destination -> Doc
fmtDest src = case src of
  Numscript.DestAccount x -> fmtAccount x
  Numscript.DestInorder cappedClauses remaining ->
    fmtBlock $ map fmtInorderDest cappedClauses ++ ["remaining " <> fmtKeptOrDest remaining]
  Numscript.DestAllotment clauses -> fmtBlock $ map fmtAllotmentDest clauses

fmtKeptOrDest :: Numscript.KeptOrDest -> Doc
fmtKeptOrDest keptOrDest = case keptOrDest of
  Numscript.Kept -> "kept"
  Numscript.To dest -> "to " <> fmtDest dest

fmtInorderDest :: (Numscript.Monetary, Numscript.KeptOrDest) -> Doc
fmtInorderDest (mon, keptOrDest) =
  "max " <> fmtMonetary mon <> " " <> fmtKeptOrDest keptOrDest

fmtAllotmentDest :: Numscript.AllotmentClause Numscript.KeptOrDest -> Doc
fmtAllotmentDest (Numscript.AllotmentClause portion keptOrDest) =
  fmtPortion portion <> " " <> fmtKeptOrDest keptOrDest

-- PPrint algebra
data Doc
  = Newline
  | Text Text
  | Cons Doc Doc
  | Nest Doc

nil :: Doc
nil = Text ""

concatDocs :: [Doc] -> Doc
concatDocs = List.foldl' Cons nil

nest :: [Doc] -> Doc
nest = Nest . lines

lines :: [Doc] -> Doc
lines docs = concatDocs [Newline <> doc | doc <- docs]

render :: Doc -> Text
render = renderWith 0

indentationSymbol :: Text
indentationSymbol = T.pack "  "

renderWith :: Int -> Doc -> Text
renderWith ind doc = case doc of
  Newline -> "\n" <> leftPadding
  Text t -> t
  Cons l r -> renderWith ind l <> renderWith ind r
  Nest n -> renderWith (ind + 1) n
 where
  leftPadding = T.replicate ind indentationSymbol

instance IsString Doc where
  fromString :: String -> Doc
  fromString = Text . T.pack

instance Semigroup Doc where
  (<>) :: Doc -> Doc -> Doc
  x <> y = x `Cons` y
