import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import System.Environment

timeR :: ReadP Int
timeR = do
  times <- sepBy (munch isDigit) (satisfy (flip elem ":,"))
  return $ foldr (+) 0 $ zipWith (*) [1,1000,60*1000,60*60*1000] $ reverse $ map read times

spaces :: ReadP String
spaces = many $ char ' '
newline = optional (char '\r') >> char '\n'

timeLineR :: ReadP (Int, Int)
timeLineR = do
  t1 <- timeR
  spaces
  string "-->"
  spaces
  t2 <- timeR
  spaces
  newline
  return (t1, t2)

subtitleR :: ReadP (Int, Int, String)
subtitleR = do
  munch isDigit
  newline
  (t1, t2) <- timeLineR
  contents <- sepBy (munch1 (not . flip elem "\r\n")) newline
  return (t1, t2, concat $ intersperse "\n" contents)

srtFileR :: ReadP [(Int, Int, String)]
srtFileR = do
  subs <- sepBy subtitleR (newline >> newline)
  skipSpaces
  eof
  return subs

parseTry :: ReadP a -> String -> String -> a
parseTry reader errorTxt txt = case [ x | (x,"") <- readP_to_S reader txt] of
                                  [] -> error errorTxt
                                  (parsed:_) -> parsed

parseSrt :: String -> [(Int, Int, String)]
parseSrt = parseTry srtFileR "Invalid subtitles format"

formatNum :: Int -> Int -> String
formatNum digits n = reverse $ take digits $ reverse (show n) ++ repeat '0'

showTime :: Int -> String
showTime time = let (0, [ms, s, m, h]) = foldl'
                      (\(remaining, s) t ->
                        (remaining `mod` t, (remaining `div` t):s))
                      (max 0 time, [])
                      [60*60*1000, 60*1000, 1000, 1]
                in
                  concat [formatNum 2 h, ":",
                          formatNum 2 m, ":",
                          formatNum 2 s, ",",
                          formatNum 3 ms]

showSub :: Int -> (Int, Int, String) -> String
showSub n (t1, t2, txt) = unlines [show n,
                                    concat [showTime t1, " --> ", showTime t2],
                                    txt, ""]

printSrt :: [(Int, Int, String)] -> String
printSrt subs = concat $ zipWith showSub [1..] subs

moveSub :: Int -> (Int, Int, String) -> (Int, Int, String)
moveSub dt (t1, t2, txt) = (t1+dt, t2+dt, txt)

timeArgR :: ReadP Int
timeArgR = do
  sign <- option (1::Int) (char '-' >> return (-1))
  minutes <- option 0 (munch1 isDigit >>= \hh -> char ':' >> return (read hh))
  seconds <- munch (\c -> isDigit c || c == '.') >>= return.read
  return $ sign * (minutes * 60*1000 + round(seconds * 1000))

parseArg :: String -> Int
parseArg = parseTry timeArgR "Invalid time format. Use [[-]minutes:]seconds[,milliseconds]"

main = do
  input <- getContents
  args <- getArgs
  case args of
    [time] -> putStr $
              printSrt $
              filter (\(t1, t2, _) -> t2 > 0 && t1 < t2) $
              map (moveSub (parseArg time)) $
              parseSrt input
    _ -> error "Exactly 1 argument required\nExample usage:\n\tsrtmove -1:30 < in.srt > out.srt"
