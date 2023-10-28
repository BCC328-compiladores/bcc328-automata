module Automata.DFA ( DFA (..)
                    , deltaStar
                    , accept
                    , longest
                    , unionDFA
                    , intersectionDFA
                    , lexer
                    ) where

-- definition of a deterministic finite automata

data DFA a
  = DFA {
      start :: a
    , delta :: a -> Char -> a
    , finals :: a -> Bool
    } 

-- producing the resulting state

deltaStar :: DFA a -> String -> a
deltaStar m = foldl (delta m) (start m)

-- checking membership

accept :: DFA a -> String -> Bool
accept m s = finals m (deltaStar m s) 

-- longest match

longest :: DFA a -> String -> Maybe (String, String)
longest m s
  = go (start m) (finals m (start m), Just "", Just  s)
    where
      go _ (True, Just pre, Just "") = Just (pre, "")
      go _ (False, _, Just "") = Nothing
      go e (True, Just pre, Just (c : cs))
        | finals m (delta m e c) = go (delta m e c) (True, Just (c : pre), Just cs)
        | otherwise   = Just (pre, (c : cs))
      go e (False, Just pre, Just (c : cs))
        | finals m (delta m e c) = go (delta m e c)(True, Just (c : pre), Just cs)
        | otherwise = go (delta m e c) (False, Just (c : pre), Just cs)
      go _ _ = Nothing

-- lexing

lexer :: DFA a -> (String -> Maybe [b]) -> String -> Maybe [b]
lexer _ _ "" = return []
lexer m action s
  = do
      (pref, suf) <- longest m s
      token <- action (reverse pref)
      tokens <- lexer m action suf
      return (token ++ tokens)

-- product construction

dfaProduct :: DFA a -> DFA b -> ((a,b) -> Bool) -> DFA (a, b)
dfaProduct m1 m2 fin
  = DFA {
      start = (start m1, start m2)
    , delta = delta'
    , finals = fin
    }
    where
      delta' (e1,e2) c = (delta m1 e1 c, delta m2 e2 c) 

-- union / intersection

unionDFA :: DFA a -> DFA b -> DFA (a,b)
unionDFA m1 m2 = dfaProduct m1 m2 g
  where
    g (e1, e2) = finals m1 e1 || finals m2 e2

intersectionDFA :: DFA a -> DFA b -> DFA (a,b)
intersectionDFA m1 m2 = dfaProduct m1 m2 g
  where
    g (e1, e2) = finals m1 e1 && finals m2 e2
