module Automata (
    Automata,
    Transition,
    State,
    Alphabet,
    -- | Type alias for a finite map from states to states
    StateMap,
    -- | Type alias for a finite map from states to transitions
    TransitionMap,
    -- | Create a new automaton from a list of states, a transition function, and an alphabet
    newAutomaton,
    -- | Get the set of all states in the automaton
    getStates,
    -- | Get the alphabet of the automaton
    getAlphabet,
    -- | Perform a transition from one state to another on the given input
    transition,
    -- | Get the initial state of the automaton
    getInitialState,
    -- | Get the accepting states of the automaton
    getAcceptingStates,
    -- | Check if a given state is accepting
    isAccepting,
    -- | Check if a given state is unreachable
    isUnreachable,
    -- | Minimize the automaton using Brzozowski's algorithm
    minimizeAutomaton
) where

import qualified Data.FiniteMap as FM
import qualified Data.Map as M
import Control.Arrow ((>>>))
import Control.Monad (liftM2, replicateM_)
import Data.List (findIndex, elemIndex, elemIndices)
import Data.Maybe (fromJust, fromMaybe)

-- | A state in the automaton
type State = Int

-- | A transition in the automaton
type Transition = (State, Char, State)

-- | The alphabet of the automaton
type Alphabet = Char

-- | Type alias for a finite map from states to states
type StateMap = FM.FiniteMap State State

-- | Type alias for a finite map from states to transitions
type TransitionMap = FM.FiniteMap State [Transition]

-- | Create a new automaton from a list of states, a transition function, and an alphabet
newAutomaton :: [State] -> TransitionMap -> Alphabet -> Automata
newAutomaton states transitions alphabet = Automata states transitions alphabet

data Automata = Automata {
    states :: StateMap,
    transitions :: TransitionMap,
    alphabet :: Alphabet
} deriving (Show, Eq)

-- | Get the set of all states in the automaton
getStates :: Automata -> [State]
getStates automaton = FM.toList $ states automaton

-- | Get the alphabet of the automaton
getAlphabet :: Automata -> Alphabet
getAlphabet automaton = alphabet automaton

-- | Perform a transition from one state to another on the given input
transition :: Automata -> State -> Char -> Maybe State
transition automaton state input = case FM.lookup state (transitions automaton) of
    Nothing -> Nothing
    Just transitions -> case find (\(t, c, _) -> c == input) transitions of
        Nothing -> Nothing
        Just (t, c, s) -> Just s

-- | Get the initial state of the automaton
getInitialState :: Automata -> Maybe State
getInitialState automaton = findIndex (\s -> FM.size (FM.filter (FM.null . snd) (states automaton)) == 1) (getStates automaton)

-- | Get the accepting states of the automaton
getAcceptingStates :: Automata -> [State]
getAcceptingStates automaton = FM.elems $ FM.filter (FM.null . snd) (states automaton)

-- | Check if a given state is accepting
isAccepting :: Automata -> State -> Bool
isAccepting automaton state = state `elem` getAcceptingStates automaton

-- | Check if a given state is unreachable
isUnreachable :: Automata -> State -> Bool
isUnreachable automaton state = state `notElem` getStates automaton

-- | Minimize the automaton using Brzozowski's algorithm
minimizeAutomaton :: Automata -> Automata
minimizeAutomaton automaton = automaton -- TODO: implement Brzozowski's algorithm