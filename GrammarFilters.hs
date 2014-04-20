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

class Attributes a where
    checkAttrs :: (a -> Bool) -> (Grammar -> Bool)

instance Attributes NounAttributes where
    checkAttrs test (Noun _ attributes) = test attributes
    checkAttrs test (NounPhrase _ noun) = checkAttrs test noun
    checkAttrs test (ArticledNounPhrase _ nounphrase _) =
        checkAttrs test nounphrase
    checkAttrs _ (Infinitive _ _)  = True  -- Can be a subject or an object
    checkAttrs _ other =
        error ("Tried testing non-noun grammar " ++ show other ++
               " for noun-like properties")

instance Attributes PrepositionAttributes where
    checkAttrs test (Preposition _ attributes) = test attributes
    checkAttrs test (PrepositionalPhrase preposition _) =
        checkAttrs test preposition
    checkAttrs _ other =
        error ("Tried testing non-preposition grammar " ++ show other ++
               " for preposition-like properties")
