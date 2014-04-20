{-# OPTIONS_GHC -Wall #-}

module GrammarFilters where

import Grammar

liftFilter :: (Grammar -> Bool) -> Node -> Bool
liftFilter test (Node g _ _) = test g

isFullSentence :: Grammar -> Bool
isFullSentence (FullSentence _) = True
isFullSentence _ = False

isSentence :: Grammar -> Bool
isSentence (Sentence _ _) = True
isSentence _ = False

isSubject :: Grammar -> Bool
isSubject (Subject _) = True
isSubject _ = False

isANP :: Grammar -> Bool
isANP (ArticledNounPhrase _ _ _) = True
isANP _ = False

isNounPhrase :: Grammar -> Bool
isNounPhrase (NounPhrase _ _) = True
isNounPhrase _ = False

isPredicate :: Grammar -> Bool
isPredicate (Predicate _ _) = True
isPredicate _ = False

isRawPredicate :: Grammar -> Bool
isRawPredicate (RawPredicate _ _) = True
isRawPredicate _ = False

isInfinitive :: Grammar -> Bool
isInfinitive (Infinitive _ _) = True
isInfinitive _ = False

isPrepositionalPhrase :: Grammar -> Bool
isPrepositionalPhrase (PrepositionalPhrase _ _) = True
isPrepositionalPhrase _ = False

isArticle :: Grammar -> Bool
isArticle (Article _) = True
isArticle _ = False

isNoun :: Grammar -> Bool
isNoun (Noun _ _) = True
isNoun _ = False

isAdjective :: Grammar -> Bool
isAdjective (Adjective _) = True
isAdjective _ = False

isVerb :: Grammar -> Bool
isVerb (Verb _) = True
isVerb _ = False

isPreposition :: Grammar -> Bool
isPreposition (Preposition _ _) = True
isPreposition _ = False

isPeriod :: Grammar -> Bool
isPeriod Period = True
isPeriod _ = False

isEOF :: Grammar -> Bool
isEOF EOF = True
isEOF _ = False

testNoun :: (NounAttributes -> Bool) -> Grammar -> Bool
testNoun test (Noun _ attributes) = test attributes
testNoun test (NounPhrase _ noun) = testNoun test noun
testNoun test (ArticledNounPhrase _ nounphrase _) = testNoun test nounphrase
--testNoun test (PrepositionalPhrase _ nounphrase) = testNoun test nounphrase
-- TODO: I don't think an infinitive can be a subject of a verb besides "to
-- be." revisit this later.
testNoun _ (Infinitive _ _)  = True  -- Can be a subject or an object
testNoun _ other = error ("Tried testing non-noun grammar " ++ show other ++
                          " for noun-like properties")
