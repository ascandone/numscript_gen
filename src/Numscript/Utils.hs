{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Numscript.Utils (
    cleanupNumscript,
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Numscript (Statement (source))
import qualified Numscript

-- | Remove some of programs which would fail compilation in the old machine
cleanupNumscript :: Numscript.Program -> Numscript.Program
cleanupNumscript = map cleanupStm

cleanupStm :: Numscript.Statement -> Numscript.Statement
cleanupStm s =
    s{source = cleanupSrc s.source}

cleanupSrc :: Numscript.Source -> Numscript.Source
cleanupSrc (Numscript.SrcInorder srcs) = Numscript.SrcInorder (cleanupSrcs srcs)
cleanupSrc (Numscript.SrcCapped cap s) = Numscript.SrcCapped cap (cleanupSrc s)
cleanupSrc (Numscript.SrcAllotment clauses) =
    Numscript.SrcAllotment $
        map (\(Numscript.AllotmentClause r src) -> Numscript.AllotmentClause r (cleanupSrc src)) clauses
cleanupSrc s@(Numscript.SrcAccount _) = s
cleanupSrc s@(Numscript.SrcAccountOverdraft _ _) = s

isSrcUnbounded :: Numscript.Source -> Bool
isSrcUnbounded (Numscript.SrcAccount "world") = True
isSrcUnbounded (Numscript.SrcAccountOverdraft _ Nothing) = True
isSrcUnbounded _ = False

cleanupSrcs :: [Numscript.Source] -> [Numscript.Source]
cleanupSrcs = cleanupSrcs' Set.empty

cleanupSrcs' :: Set Text -> [Numscript.Source] -> [Numscript.Source]
cleanupSrcs' _ [] = []
cleanupSrcs' found (src : xs@(_ : _)) | isSrcUnbounded src = cleanupSrcs' found xs
cleanupSrcs' found (Numscript.SrcAccount account : xs) | Set.member account found = cleanupSrcs' found xs
cleanupSrcs' found (x@(Numscript.SrcAccount account) : xs) = x : cleanupSrcs' (Set.insert account found) xs
cleanupSrcs' found (x : xs) = cleanupSrc x : cleanupSrcs' found xs
