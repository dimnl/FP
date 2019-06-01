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
newtype Parser a = P { runParser :: Stream -> [(a, Stream)]}

------------------------------------------------------------------
--  D-3: FP1.2
------------------------------------------------------------------
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

------------------------------------------------------------------
--  D-3: FP1.4
------------------------------------------------------------------
failure :: Parser a
failure = P (\x -> [])

------------------------------------------------------------------
-- D-3: FP1.5
------------------------------------------------------------------
instance Applicative Parser where
  pure f = P (\x -> [(f, x)])
  (P f) <*> (P g) = P (\x -> [(r1 r2, x2) | (r1, x1) <- f x, (r2, x2) <- g x1])

------------------------------------------------------------------
-- D-3: FP1.6
------------------------------------------------------------------
instance Alternative Parser where
  empty = failure
  (P f) <|> (P g) = P $ \x -> case f x of [] -> g x
                                          v  -> v

------------------------------------------------------------------
-- D-3: FP1.7
------------------------------------------------------------------
instance Monoid (Parser a) where
  mempty = empty
  mappend (P f) (P g)  = P (\x -> f x ++ g x)

------------------------------------------------------------------
-- D-3: FP1.8
------------------------------------------------------------------
-- instance (Monoid a) => Monoid (Parser a) where
--   mempty = empty
--   mappend (P p1) (P p2)= p1 <*> p2

------------------------------------------------------------------
-- Some tests
------------------------------------------------------------------
parseOne :: Parser Char
parseOne = char '1'

parseOneInt :: Parser Int
parseOneInt = fmap digitToInt parseOne

parseOneIntFail    = runParser parseOneInt (Stream "2a2")
parseOneIntSatisfy = runParser parseOneInt (Stream "1a21")
