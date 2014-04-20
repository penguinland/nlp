{-# OPTIONS_GHC -Wall #-}

module GrammarFilters where

import Grammar

isFullSentence :: Node -> Bool
isFullSentence (Node (FullSentence _) _ _) = True
isFullSentence _ = False

isSentence :: Node -> Bool
isSentence (Node (Sentence _ _) _ _) = True
isSentence _ = False

isSubject :: Node -> Bool
isSubject (Node (Subject _) _ _) = True
isSubject _ = False

isANP :: Node -> Bool
isANP (Node (ArticledNounPhrase _ _ _) _ _) = True
isANP _ = False

isNounPhrase :: Node -> Bool
isNounPhrase (Node (NounPhrase _ _) _ _) = True
isNounPhrase _ = False

isPredicate :: Node -> Bool
isPredicate (Node (Predicate _ _) _ _) = True
isPredicate _ = False

isRawPredicate :: Node -> Bool
isRawPredicate (Node (RawPredicate _ _) _ _) = True
isRawPredicate _ = False

isInfinitive :: Node -> Bool
isInfinitive (Node (Infinitive _ _) _ _) = True
isInfinitive _ = False

isPrepositionalPhrase :: Node -> Bool
isPrepositionalPhrase (Node (PrepositionalPhrase _ _) _ _) = True
isPrepositionalPhrase _ = False

isArticle :: Node -> Bool
isArticle (Node (Article _) _ _) = True
isArticle _ = False

isNoun :: Node -> Bool
isNoun (Node (Noun _ _) _ _) = True
isNoun _ = False

isAdjective :: Node -> Bool
isAdjective (Node (Adjective _) _ _) = True
isAdjective _ = False

isVerb :: Node -> Bool
isVerb (Node (Verb _) _ _) = True
isVerb _ = False

isPreposition :: Node -> Bool
isPreposition (Node (Preposition _) _ _) = True
isPreposition _ = False

isPeriod :: Node -> Bool
isPeriod (Node Period _ _) = True
isPeriod _ = False

isEOF :: Node -> Bool
isEOF (Node EOF _ _) = True
isEOF _ = False

testNoun :: (NounAttributes -> Bool) -> Node -> Bool
testNoun test (Node grammar _ _) =
  let
    testNoun' (Noun _ attributes) = test attributes
    testNoun' (NounPhrase _ noun) = testNoun' noun
    testNoun' (ArticledNounPhrase _ nounphrase _) = testNoun' nounphrase
    --testNoun' (PrepositionalPhrase _ nounphrase) = testNoun' nounphrase
    -- TODO: I don't think an infinitive can be a subject of a verb besides "to
    -- be." revisit this later.
    testNoun' (Infinitive _ _)  = True  -- Can be a subject or an object
    testNoun' other =
        error ("Tried testing non-noun grammar " ++ show other ++
               "for noun-like properties")
  in
    testNoun' grammar
