

module Day09 where


data Stream = Garbage String | Group [Stream] deriving Show

parseGarbage buf ('!':c:cs) = parseGarbage buf cs
parseGarbage buf ('>':cs)   = (Garbage $ reverse buf, cs)
parseGarbage buf (c:cs)     = parseGarbage (c:buf) cs


{-parseGroup :: String -> ([Stream], String)-}
parseGroup buf ('{':cs) = let (g,cs') = parseGroup [] cs in parseGroup (g:buf) cs'
parseGroup buf ('<':cs) = let (g,cs') = parseGarbage [] cs in parseGroup (g:buf) cs'
parseGroup buf (',':cs) = let (g,cs') = parse cs in parseGroup (g:buf) cs'
parseGroup buf ('}':cs) = (Group (reverse buf), cs)



parse :: String -> (Stream,String)
parse ('{':cs) = parseGroup [] cs
parse ('<':cs) = parseGarbage [] cs

runParse :: String -> Stream
runParse a = b where (b,_) = parse a


score s (Garbage _) = 0
score s (Group cs)  = s + sum (map (score (s+1)) cs)

countGarbage (Garbage s) = length s
countGarbage (Group cs)  = sum (map countGarbage cs)


run i = (score 1 s, countGarbage s) where s = runParse i
