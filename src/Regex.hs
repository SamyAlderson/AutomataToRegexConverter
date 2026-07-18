-- | Module containing the regular expression data type and operations.
module Regex where

-- | Import necessary modules
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.List (nub)
import Text.Regex.Pcre (Regex, re)

-- | Data type representing a regular expression
data Regex = Empty
           | Literal Char
           | Sequence Regex Regex
           | Union Regex Regex
           | Star Regex
           deriving (Eq)

-- | Data type representing an error in the regular expression
data RegexError = InvalidRegex T.Text
                | NoPatternFound
                deriving (Eq)

-- | Function to parse a string into a regular expression
parseRegex :: T.Text -> Maybe Regex
parseRegex reStr = case re reStr of
                    Just (match, _) -> Just (Sequence (Literal $ T.head match) (Literal $ T.last match))
                    Nothing -> Nothing

-- | Function to convert a regular expression into a string
regexToString :: Regex -> T.Text
regexToString Empty = ""
regexToString (Literal c) = T.pack [c]
regexToString (Sequence r1 r2) = regexToString r1 <> " " <> regexToString r2
regexToString (Union r1 r2) = "(" <> regexToString r1 <> "|" <> regexToString r2 <> ")"
regexToString (Star r) = regexToString r <> "*"

-- | Function to check if a regular expression is valid
isValidRegex :: Regex -> Bool
isValidRegex Empty = True
isValidRegex (Literal _) = True
isValidRegex (Sequence r1 r2) = isValidRegex r1 && isValidRegex r2
isValidRegex (Union r1 r2) = isValidRegex r1 && isValidRegex r2
isValidRegex (Star r) = isValidRegex r

-- | Function to check if two regular expressions are equal
(==) :: Regex -> Regex -> Bool
Empty == Empty = True
Literal c1 == Literal c2 = c1 == c2
Sequence r1 r2 == Sequence r3 r4 = r1 == r3 && r2 == r4
Union r1 r2 == Union r3 r4 = r1 == r3 && r2 == r4
Star r == Star r' = r == r'
_ == _ = False

-- | Function to get the union of two regular expressions
union :: Regex -> Regex -> Regex
union Empty r = r
union r Empty = r
union (Literal c1) (Literal c2) = Literal c2
union (Sequence r1 r2) (Sequence r3 r4) = Sequence (union r1 r3) (union r2 r4)
union (Union r1 r2) (Union r3 r4) = Union (union r1 r3) (union r2 r4)
union (Star r1) (Star r2) = Star (union r1 r2)

-- | Function to get the concatenation of two regular expressions
(>>) :: Regex -> Regex -> Regex
Empty >>> _ = Empty
Literal c >>> r = Sequence (Literal c) r
Sequence r1 r2 >>> r = Sequence r1 (Sequence r2 r)
Union r1 r2 >>> r = Union (Sequence r1 r) (Sequence r2 r)
Star r >>> r' = Union r (Sequence r r')

-- | Function to get the Kleene star of a regular expression
star :: Regex -> Regex
star Empty = Empty
star (Literal c) = Star (Literal c)
star (Sequence r1 r2) = star r2
star (Union r1 r2) = Union (star r1) (star r2)
star (Star r) = Union r (star r)

-- | Function to get the negation of a regular expression
negate :: Regex -> Regex
negate Empty = Empty
negate (Literal c) = Empty
negate (Sequence r1 r2) = Union (negate r1) (negate r2)
negate (Union r1 r2) = Union (negate r1) (negate r2)
negate (Star r) = Union (negate r) (Union (negate r) (negate r))

-- | Function to get the complement of a regular expression
complement :: Regex -> Regex
complement Empty = Empty
complement (Literal c) = Empty
complement (Sequence r1 r2) = Union (complement r1) (complement r2)
complement (Union r1 r2) = Union (complement r1) (complement r2)
complement (Star r) = Union (complement r) (Union (complement r) (complement r))