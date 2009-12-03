module EmpireZipper
where

import Data.Maybe

import qualified Data.Edison.Assoc.StandardMap as E

import Civilization
import World

type EmpireZipper = (E.FM CivKey Empire, Maybe (Empire, Maybe Colony))

newEmpireZipper :: World -> EmpireZipper
newEmpireZipper w = (empires w, Nothing)

upEZ :: EmpireZipper -> EmpireZipper
upEZ (es, Nothing) = (es, Nothing)
upEZ (es, Just (e, Nothing)) = (es, Nothing)
upEZ (es, Just (e, Just c)) = (es, Just (e, Nothing))

tryDownEZ :: CivKey -> EmpireZipper -> Maybe EmpireZipper
tryDownEZ s (es, Nothing) =
  case E.lookupM s es of
    Nothing -> Nothing
    Just em -> Just (es, Just (em, Nothing))
tryDownEZ s (es, Just (e, Nothing)) = 
  case E.lookupM s (colonies e) of
    Nothing -> Nothing
    Just co -> Just (es, Just (e, Just co))
tryDownEZ _ _ = Nothing

