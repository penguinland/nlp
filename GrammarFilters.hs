{-# OPTIONS_GHC -Wall #-}

module GrammarFilters where

import Grammar

liftFilter :: (Grammar -> a) -> Node -> a
liftFilter get (Node g _ _) = get g

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
isInfinitive (Infinitive _ _ _) = True
isInfinitive _ = False

isPrepositionalPhrase :: Grammar -> Bool
isPrepositionalPhrase (PrepositionalPhrase _ _) = True
isPrepositionalPhrase _ = False

isConjunctivePhrase :: Grammar -> Bool
isConjunctivePhrase (ConjunctivePhrase _ _ _) = True
isConjunctivePhrase _ = False

isArticle :: Grammar -> Bool
isArticle (Article _) = True
isArticle _ = False

isNoun :: Grammar -> Bool
isNoun (Noun _ _) = True
isNoun _ = False

isAdjective :: Grammar -> Bool
isAdjective (Adjective _) = True
isAdjective _ = False

isConjunction :: Grammar -> Bool
isConjunction (Conjunction _) = True
isConjunction _ = False

isVerb :: Grammar -> Bool
isVerb (Verb _ _) = True
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
    getAttrs :: (a -> b) -> (Grammar -> b)

-- TODO: make errors more graceful.

instance Attributes NounAttributes where
    getAttrs get (Noun _ attributes) = get attributes
    getAttrs get (NounPhrase _ noun) = getAttrs get noun
    getAttrs get (ArticledNounPhrase _ nounphrase _) = getAttrs get nounphrase
    getAttrs get (Infinitive _ _ attributes)  = get attributes
    getAttrs get (Subject anp) = getAttrs get anp
    -- We don't include conjunctive phrases here because they will be wrapped in
    -- their own ANP. You should never need to try getting noun attributes from
    -- a conjunctive phrase.
    getAttrs _ other =
        error ("Tried getting noun-like attributes from non-noun grammar " ++
               show other)


instance Attributes VerbAttributes where
    getAttrs get (Verb _ attributes) = get attributes
    getAttrs get (RawPredicate verb _) = getAttrs get verb
    getAttrs get (Predicate rawPred _) = getAttrs get rawPred
    -- For a conjunctive phrase to be created, we require all pieces of it to
    -- have identical attributes. Just check the last one, because the rest will
    -- be the same.
    getAttrs get (ConjunctivePhrase _ _ right) = getAttrs get right
    getAttrs _ other =
        error ("Tried getting verb-like attributes from non-verb grammar " ++
               show other)

instance Attributes PrepositionAttributes where
    getAttrs get (Preposition _ attributes) = get attributes
    getAttrs get (PrepositionalPhrase preposition _) = getAttrs get preposition
    -- For a conjunctive phrase to be created, we require all pieces of it to
    -- have identical attributes. Just check the last one, because the rest will
    -- be the same.
    getAttrs get (ConjunctivePhrase _ _ right) = getAttrs get right
    getAttrs _ other =
        error ("Tried getting preposition-like attributes from " ++
               "non-preposition grammar " ++ show other)
