{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant <$>" #-}

module Numscript.Utils (
    cleanupNumscript,
) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.State.Strict as State
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
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
    s{source = removeEmptyAllotments' $ State.evalState (cleanupSrc s.source) initialCleanupData}
  where
    removeEmptyAllotments' = fromMaybe "world" . removeEmptyAllotments

newtype CleanupData
    = CleanupData
    { emptiedAccounts :: Set Text
    }

initialCleanupData :: CleanupData
initialCleanupData =
    CleanupData
        { emptiedAccounts = Set.empty
        }

type CleanupState = State.State CleanupData

cleanupSrc :: Numscript.Source -> CleanupState Numscript.Source
cleanupSrc (Numscript.SrcInorder srcs) = Numscript.SrcInorder <$> cleanupSrcs srcs
cleanupSrc (Numscript.SrcCapped cap src) = Numscript.SrcCapped cap <$> cleanupSrc src
cleanupSrc (Numscript.SrcAllotment clauses) =
    Numscript.SrcAllotment <$> clauses'
  where
    clauses' = mapM (\(Numscript.AllotmentClause r src) -> Numscript.AllotmentClause r <$> cleanupSrc src) clauses
cleanupSrc s@(Numscript.SrcAccount _) = return s
cleanupSrc s@(Numscript.SrcAccountOverdraft _ _) = return s

isSrcUnbounded :: Numscript.Source -> Bool
isSrcUnbounded (Numscript.SrcAccount "world") = True
isSrcUnbounded (Numscript.SrcAccountOverdraft _ Nothing) = True
isSrcUnbounded _ = False

cleanupSrcs :: [Numscript.Source] -> CleanupState [Numscript.Source]
cleanupSrcs [] = return []
cleanupSrcs (src : srcs@(_ : _)) | isSrcUnbounded src = cleanupSrcs srcs
cleanupSrcs (src@(Numscript.SrcAccount account) : srcs) = cleanupSrcsHelper src account srcs
cleanupSrcs (src@(Numscript.SrcCapped _ (Numscript.SrcAccount account)) : srcs) = cleanupSrcsHelper src account srcs
cleanupSrcs (src@(Numscript.SrcAccountOverdraft account _) : srcs) = cleanupSrcsHelper src account srcs
cleanupSrcs (src : srcs) = cleanupSrc src `mCons` cleanupSrcs srcs

cleanupSrcsHelper :: Numscript.Source -> Text -> [Numscript.Source] -> CleanupState [Numscript.Source]
cleanupSrcsHelper src account srcs = do
    isAccountEmptied <- Set.member account <$> emptiedAccounts <$> State.get
    if isAccountEmptied
        then cleanupSrcs srcs
        else do
            State.modify $ \m -> m{emptiedAccounts = Set.insert account m.emptiedAccounts}
            cleanupSrc src `mCons` cleanupSrcs srcs

mCons :: (Applicative m) => m a -> m [a] -> m [a]
mCons = liftA2 (:)

removeEmptyAllotments :: Numscript.Source -> Maybe Numscript.Source
removeEmptyAllotments (Numscript.SrcInorder []) = Nothing
removeEmptyAllotments (Numscript.SrcCapped cap src) = Numscript.SrcCapped cap <$> removeEmptyAllotments src
removeEmptyAllotments (Numscript.SrcInorder (Numscript.SrcInorder [] : srcs)) = removeEmptyAllotments $ Numscript.SrcInorder srcs
removeEmptyAllotments (Numscript.SrcInorder (src : srcs)) =
    Numscript.SrcInorder <$> case src' ++ srcs' of
        [] -> Nothing
        xs@(_ : _) -> Just xs
  where
    src' = maybeToList $ removeEmptyAllotments src
    srcs' = mapMaybe removeEmptyAllotments srcs
removeEmptyAllotments (Numscript.SrcAllotment clauses) =
    Numscript.SrcAllotment
        <$> mapM (\(Numscript.AllotmentClause r src) -> Numscript.AllotmentClause r <$> removeEmptyAllotments src) clauses
removeEmptyAllotments src = return src
