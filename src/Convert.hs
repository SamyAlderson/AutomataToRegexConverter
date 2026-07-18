-- | Module for converting finite automata to regular expressions using Brzozowski's algorithm
module Convert where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Automata
import Regex

-- | Convert a finite automaton to a regular expression using Brzozowski's algorithm
convertAutomaton :: Automaton -> Regex
convertAutomaton automaton = brzozowski automaton

-- | Brzozowski's algorithm to convert a finite automaton to a regular expression
brzozowski :: Automaton -> Regex
brzozowski (Automaton states transitions start accept) =
    let
        -- | Get the set of all states that can be reached from the initial state
        reachableStates :: Set.Set State
        reachableStates = reachFromStart states transitions start

        -- | Get the set of all accepting states that can be reached from the initial state
        reachableAccepting :: Set.Set State
        reachableAccepting = filter (`Set.member` accept) reachableStates

        -- | Get the set of all non-accepting states that can be reached from the initial state
        reachableNonAccepting :: Set.Set State
        reachableNonAccepting = Set.difference reachableStates reachableAccepting

        -- | Combine the accepting states with the non-accepting states to form the regular expression
        regex :: Regex
        regex =
            if Set.null reachableAccepting
                then emptyRegex
                else combine reachableAccepting reachableNonAccepting
    in
    regex

-- | Get the set of all states that can be reached from the initial state
reachFromStart :: Set.Set State -> [(State, State)] -> State -> Set.Set State
reachFromStart states transitions start =
    let
        -- | Get the set of all states that can be reached from a given state
        reachableFrom :: State -> Set.Set State
        reachableFrom s = Set.fromList $ concatMap (transitionsFrom s) transitions
    in
    Set.insert start $ foldr reachableFrom Set.empty states

-- | Get the set of all states that can be reached from a given state
transitionsFrom :: State -> State -> [State]
transitionsFrom s s' = case lookup s transitions of
    Nothing -> []
    Just ts -> ts

-- | Combine two sets of states to form a regular expression
combine :: Set.Set State -> Set.Set State -> Regex
combine accepting nonAccepting =
    let
        -- | Get the set of all symbols that can be read from a state
        symbols :: State -> [Symbol]
        symbols s = concatMap (transitionsTo s) transitions

        -- | Get the set of all symbols that can be read from a set of states
        symbolsFrom :: Set.Set State -> [Symbol]
        symbolsFrom states = nub $ concatMap symbols (Set.toList states)

        -- | Get the set of all symbols that can be read from the accepting states
        acceptingSymbols :: [Symbol]
        acceptingSymbols = symbolsFrom accepting

        -- | Get the set of all symbols that can be read from the non-accepting states
        nonAcceptingSymbols :: [Symbol]
        nonAcceptingSymbols = symbolsFrom nonAccepting
    in
    union (union acceptingSymbols nonAcceptingSymbols) (union (union acceptingSymbols nonAcceptingSymbols) complement acceptingSymbols)

-- | Get the set of all symbols that can be read from a state
transitionsTo :: State -> [(State, [Symbol])] -> [Symbol]
transitionsTo s ts = case lookup s ts of
    Nothing -> []
    Just symbols -> symbols

-- | Complement a set of states
complement :: Set.Set State -> Set.Set State
complement = Set.difference (Set.union Set.empty) 

-- | Union of two sets of symbols
union :: [Symbol] -> [Symbol] -> [Symbol]
union = nub . concat