module Parser (TrackFile (..), parseTrackFile) where
import Control.Monad
import ApplicativeParsec
import Data.List (intersperse)
import Numeric

-- | TrackFile format here is a skeleton without much in the way of
-- semantics.  Basically it is comment-preserving, line-based, a
-- series of entries beginning with a number, followed by an
-- alphabetic code, followed by some optional numbers, and then a
-- string description.
data TrackFile = Comment String | Entry (Double, String, [Double], String)

instance Show TrackFile where
  show (Comment c) = "#" ++ c++ "\n"
  show (Entry (d, t, a, n)) = show d ++ " " ++ t ++
                              concatMap ((' ':) . show) a ++
                              (if null n then "" else " " ++ n) ++
                              "\n"

whiteSpace = many (oneOf " \t") >> return ()

number = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

trackFile = do
  x <- many $ do
         e <- comment <|> trackEntry
         (newline >> return ()) <|> eof
         return e
  eof
  return x

trackEntry = do
  dist <- number
  whiteSpace
  typ  <- many1 letter
  whiteSpace
  args <- many number
  whiteSpace
  name <- many (noneOf "\n")
  return $ Entry (dist, typ, args, name)

comment = do
  char '#'
  c <- many (noneOf "\n")
  return $ Comment c

-- | Parse a file into a TrackFile
parseTrackFile :: SourceName -> IO (Either ParseError [TrackFile])
parseTrackFile = parseFromFile trackFile

-- | Parse a string into a TrackFile
parseTrackString :: String -> Either ParseError [TrackFile]
parseTrackString = parse trackFile "string"
