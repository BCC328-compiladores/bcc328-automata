module Automata.ExamplesDFA ( numberDFA
                            , ifDFA
                            , identDFA
                            , spaceDFA
                            , spaceDFAAction
                            , ifOrIdentDFA
                            , ifOrIdentDFAAction
                            ) where

import Data.Char

import Automata.DFA


-- example dfas

numberDFA :: DFA (Maybe Bool)
numberDFA
  = DFA {
      start = Just False
    , delta = numberTrans
    , finals = \ e -> e == Just True
    }
    where
      numberTrans (Just False) c
        | isDigit c = Just True
        | otherwise = Nothing
      numberTrans (Just True) c
        | isDigit c = Just True
        | otherwise = Nothing
      numberTrans _ _ = Nothing

numberDFAAction :: String -> Maybe String
numberDFAAction s
  | all isDigit s = Just s
  | otherwise     = Nothing

-- recognizing if keyword

ifDFA :: DFA (Maybe Int)
ifDFA
  = DFA {
      start = Just 0
    , delta = ifTrans
    , finals = \ e -> e == Just 2
    }
 where
   ifTrans (Just 0) 'i' = Just 1
   ifTrans (Just 1) 'f' = Just 2
   ifTrans _ _ = Nothing

data Token = If | Ident String deriving (Show, Eq)

ifDFAAction :: String -> Maybe [Token]
ifDFAAction "if" = Just [If]
ifDFAAction _    = Nothing

-- recognizing identifiers

identDFA :: DFA (Maybe Int)
identDFA
  = DFA {
      start = Just 0
    , delta = identTrans
    , finals = \ e -> e == Just 1
    }
  where
    identTrans (Just 0) c
      | isLetter c = Just 1
      | otherwise = Nothing
    identTrans (Just 1) c
      | isAlphaNum c = Just 1
      | otherwise = Nothing
    identTrans _ _ = Nothing

identDFAAction :: String -> Maybe [Token]
identDFAAction [] = Nothing
identDFAAction s@(c : cs)
  | isLetter c && all isAlphaNum cs = Just [Ident s]
  | otherwise                       = Nothing

-- spaces DFA

spaceDFA :: DFA (Maybe Int)
spaceDFA
  = DFA {
      start = Just 0
    , delta = spaceTrans
    , finals = \ e -> e == Just 1
    }
  where
    spaceTrans (Just 0) c
      | isSpace c = Just 1
      | otherwise = Nothing
    spaceTrans (Just 1) c
      | isSpace c = Just 1
      | otherwise = Nothing
    spaceTrans _ _ = Nothing

spaceDFAAction :: String -> Maybe [Token]
spaceDFAAction s
  | all isSpace s = Just []
  | otherwise     = Nothing

-- recognizing if or identifiers

ifOrIdentDFA :: DFA ((Maybe Int, Maybe Int), Maybe Int)
ifOrIdentDFA = ifDFA `unionDFA` identDFA `unionDFA` spaceDFA

ifOrIdentDFAAction :: String -> Maybe [Token]
ifOrIdentDFAAction s
  = case spaceDFAAction s of
      Nothing -> case ifDFAAction s of
                   Nothing -> identDFAAction s
                   tk -> tk
      tk      -> tk
