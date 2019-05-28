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
  fmap f (P g) = P $ fmap (applyOnFirst f) . g

applyOnFirst :: (a -> b) -> (a, Stream) -> (b,Stream)
applyOnFirst f (x,y) = (f x, y)

------------------------------------------------------------------
--  D-3: FP1.3
------------------------------------------------------------------
char :: Char -> Parser Char
char c = P p
  where p (Stream []) = []
        p (Stream (x:xs))
          | c == x    = [(x, Stream xs)]
          | otherwise = []

------------------------------------------------------------------
--  D-3: FP1.4
------------------------------------------------------------------
failure :: Parser a
failure = P p
  where p (Stream _) = []

------------------------------------------------------------------
-- D-3: FP1.5
------------------------------------------------------------------
instance Applicative Parser where
  pure f = P (\x -> [(f, x)])
  (P f) <*> (P g) = P (\x -> [(resultF resultG, x2) | (resultF, x1) <- f x, (resultG, x2) <- g x1])

------------------------------------------------------------------
-- D-3: FP1.6
------------------------------------------------------------------
instance Alternative Parser where
  empty = failure
  (P f) <|> (P g) = P (\x -> f x ++ g x)

------------------------------------------------------------------
-- D-3: FP1.7
------------------------------------------------------------------
instance Monoid (Parser a) where
  mempty = failure
  mappend p1 p2 = p1 <|> p2 -- not sure if correct yet

------------------------------------------------------------------
-- D-3: FP1.8
------------------------------------------------------------------


------------------------------------------------------------------
-- Some tests
------------------------------------------------------------------
parseOne :: Parser Char
parseOne = char '1'

parserRun :: Parser r -> Stream -> [(r, Stream)]
parserRun p s = runParser p s

parseOneInt :: Parser Int
parseOneInt = fmap digitToInt parseOne

parseOneIntFail    = parserRun parseOneInt (Stream "2a2")
parseOneIntSatisfy = parserRun parseOneInt (Stream "1a21")
