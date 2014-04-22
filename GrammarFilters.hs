{-# OPTIONS_GHC -Wall #-}

module GrammarFilters where

import Control.Monad

import Grammar

liftFilter :: (Grammar -> a) -> Node -> a
liftFilter get (Node g _ _) = get g

-- Read this type signature as (a -> Bool) -> (a -> Bool) -> a -> Bool
andAlso :: (Monad m) => m Bool -> m Bool -> m Bool
andAlso = liftM2 (&&)

isFullSentence :: Grammar -> Bool
isFullSentence (FullSentence _) = True
isFullSentence _ = False

isSentence :: Grammar -> Bool
isSentence (Sentence _ _) = True
isSentence (ConjunctivePhrase _ _ end) = isSentence end
isSentence _ = False

isSubject :: Grammar -> Bool
isSubject (Subject _) = True
isSubject (ConjunctivePhrase _ _ end) = isSubject end
isSubject _ = False

isANP :: Grammar -> Bool
isANP (ArticledNounPhrase _ _ _) = True
isANP (ConjunctivePhrase _ _ end) = isANP end
isANP _ = False

isNounPhrase :: Grammar -> Bool
isNounPhrase (NounPhrase _ _) = True
isNounPhrase (ConjunctivePhrase _ _ end) = isNounPhrase end
isNounPhrase _ = False

isPredicate :: Grammar -> Bool
isPredicate (Predicate _ _) = True
isPredicate (ConjunctivePhrase _ _ end) = isPredicate end
isPredicate _ = False

isRawPredicate :: Grammar -> Bool
isRawPredicate (RawPredicate _ _) = True
isRawPredicate (ConjunctivePhrase _ _ end) = isRawPredicate end
isRawPredicate _ = False

isInfinitive :: Grammar -> Bool
isInfinitive (Infinitive _ _ _) = True
isInfinitive (ConjunctivePhrase _ _ end) = isInfinitive end
isInfinitive _ = False

isPrepositionalPhrase :: Grammar -> Bool
isPrepositionalPhrase (PrepositionalPhrase _ _) = True
isPrepositionalPhrase (ConjunctivePhrase _ _ end) = isPrepositionalPhrase end
isPrepositionalPhrase _ = False

isConjunctivePhrase :: Grammar -> Bool
isConjunctivePhrase (ConjunctivePhrase _ _ _) = True
isConjunctivePhrase _ = False

isArticle :: Grammar -> Bool
isArticle (Article _) = True
isArticle _ = False

isNoun :: Grammar -> Bool
isNoun (Noun _ _) = True
isNoun (ConjunctivePhrase _ _ end) = isNoun end
isNoun _ = False

isAdjective :: Grammar -> Bool
isAdjective (Adjective _) = True
isAdjective (ConjunctivePhrase _ _ end) = isAdjective end
isAdjective _ = False

isConjunction :: Grammar -> Bool
isConjunction (Conjunction _) = True
isConjunction _ = False

-- TODO: can you have conjunctions of verbs that are not in predicates?
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
    getAttrs :: (a -> b) -> Grammar -> Maybe b
    checkAttrs :: (a -> Bool) -> Grammar -> Bool
    checkAttrs getter grammar =
      let
        verify Nothing = False
        verify (Just x) = x
      in
        verify (getAttrs getter grammar)

instance Attributes NounAttributes where
    getAttrs get (Noun _ attributes) = Just $ get attributes
    getAttrs get (NounPhrase _ noun) = getAttrs get noun
    getAttrs get (ArticledNounPhrase _ nounphrase _) = getAttrs get nounphrase
    getAttrs get (Infinitive _ _ attributes)  = Just $ get attributes
    getAttrs get (Subject anp) = getAttrs get anp
    -- We don't include conjunctive phrases here because they will be wrapped in
    -- their own ANP. You should never need to try getting noun attributes from
    -- a conjunctive phrase.
    getAttrs _ _ = Nothing


instance Attributes VerbAttributes where
    getAttrs get (Verb _ attributes) = Just $ get attributes
    getAttrs get (RawPredicate verb _) = getAttrs get verb
    getAttrs get (Predicate rawPred _) = getAttrs get rawPred
    -- For a conjunctive phrase to be created, we require all pieces of it to
    -- have identical attributes. Just check the last one, because the rest will
    -- be the same.
    getAttrs get (ConjunctivePhrase _ _ right) = getAttrs get right
    getAttrs _ _ = Nothing

instance Attributes PrepositionAttributes where
    getAttrs get (Preposition _ attributes) = Just $ get attributes
    getAttrs get (PrepositionalPhrase preposition _) = getAttrs get preposition
    -- For a conjunctive phrase to be created, we require all pieces of it to
    -- have identical attributes. Just check the last one, because the rest will
    -- be the same.
    getAttrs get (ConjunctivePhrase _ _ right) = getAttrs get right
    getAttrs _ _ = Nothing
