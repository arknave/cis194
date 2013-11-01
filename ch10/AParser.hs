{- CIS 194 HW 10
-}

module AParser where

import           Control.Applicative
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-} 
-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a, c) -> (b, c)
first f t = (f $ fst t, snd t)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser (\x -> Just (a, ""))
  pf <*> pv = Parser p
    where
      --there should be a cleaner way to write this
      p s = case (runParser pf s) of
              Just (nf, ns) -> case (runParser pv ns) of 
                  Just (a, nns) -> Just (nf a, nns)
                  Nothing       -> Nothing
              Nothing       -> Nothing

--This too
instance Alternative Parser where
  empty = Parser (const Nothing)
  f <|> g = Parser p
    where
      p s = case (runParser f s) of
              Nothing -> runParser g s
              Just a  -> Just a


abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (const . const ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x y z -> [x, z]) <$> posInt <*> (char ' ') <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = fmap (const ()) (posInt) <|> fmap (const ()) (satisfy isUpper)
