-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Dim Hoogeveen (s1728938)
-- Student 2: Other Name (syyyyyyy)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

------------------------------------------------------------------
--  D-3: FP1.1
------------------------------------------------------------------
-- Parser that receive Stream and results in type a with remaining Stream
newtype Parser a = P { runParser :: Stream -> [(a, Stream)]}

------------------------------------------------------------------
--  D-3: FP1.2
------------------------------------------------------------------
-- Functor instance that maps function over first value of tuple Parser
instance Functor Parser where
  fmap f (P g) = P $ fmap (\(x,y)-> (f x, y)) . g

------------------------------------------------------------------
--  D-3: FP1.3
------------------------------------------------------------------
-- Returns value if cond satisfies
satisfy  :: (Char -> Bool) -> Parser Char
satisfy cond = P p
  where p (Stream []) = []
        p (Stream (x:xs))
          | cond x    = [(x, Stream xs)]
          | otherwise = []

-- Parses one character
char :: Char -> Parser Char
char c = satisfy (==c)

-- Simple tests
parseOne :: Parser Char
parseOne = char '1'

parseOneInt :: Parser Int -- To check Functor
parseOneInt = fmap digitToInt parseOne

parseOneFail    = runParser parseOne (Stream "2a2")
parseOneSatisfy = runParser parseOne (Stream "1a21")

parseOneIntFail    = runParser parseOneInt (Stream "2a2")
parseOneIntSatisfy = runParser parseOneInt (Stream "1a21")

------------------------------------------------------------------
--  D-3: FP1.4
------------------------------------------------------------------
-- Consumes no input and fails
failure :: Parser a
failure = P (\x -> [])

-- Simple tests
parseFailure1    = runParser failure (Stream "2a2")
parseFailure2    = runParser failure (Stream "  a1a21")

------------------------------------------------------------------
-- D-3: FP1.5
------------------------------------------------------------------
-- Applicative instance for sequence combinator
instance Applicative Parser where
  pure f = P (\x -> [(f, x)])
  (P f) <*> (P g) = P (\x -> [(r1 r2, x2) | (r1, x1) <- f x, (r2, x2) <- g x1])

------------------------------------------------------------------
-- D-3: FP1.6
------------------------------------------------------------------
-- Alternative instance that tries as few alternatives as possible
instance Alternative Parser where
  empty = failure
  (P f) <|> (P g) = P $ \x -> case f x of [] -> g x
                                          v  -> v

------------------------------------------------------------------
-- D-3: FP1.7
------------------------------------------------------------------
-- Monoid instance that tries all alternatives
instance Monoid (Parser a) where
  mempty = empty
  mappend (P f) (P g)  = P (\x -> f x ++ g x)

------------------------------------------------------------------
-- D-3: FP1.8
------------------------------------------------------------------
-- Wrapper for Parser to define Monoid wrapper
newtype ParseWrap a = PW { getParser :: Parser a}

-- Monoid wrapper that combines parsers sequentially
instance (Monoid a) => Monoid (ParseWrap a) where
  mempty = PW empty
  mappend (PW f)  (PW g) = PW (mappend <$> f <*> g)
