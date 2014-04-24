{-# OPTIONS_GHC -Wall #-}

module GrammarFilters where

import Control.Monad

import Attributes
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
isSentence (ConjunctivePhrase _ _ end _) = isSentence end
isSentence _ = False

isSubject :: Grammar -> Bool
isSubject (Subject _) = True
isSubject _ = False

isANP :: Grammar -> Bool
isANP (ArticledNounPhrase _ _ _) = True
isANP (ConjunctivePhrase _ _ end _) = isANP end
isANP _ = False

isNounPhrase :: Grammar -> Bool
isNounPhrase (NounPhrase _ _) = True
isNounPhrase (ConjunctivePhrase _ _ end _) = isNounPhrase end
isNounPhrase _ = False

isPredicate :: Grammar -> Bool
isPredicate (Predicate _ _) = True
isPredicate (ConjunctivePhrase _ _ end _) = isPredicate end
isPredicate _ = False

isRawPredicate :: Grammar -> Bool
isRawPredicate (RawPredicate _ _) = True
isRawPredicate (ConjunctivePhrase _ _ end _) = isRawPredicate end
isRawPredicate _ = False

isInfinitive :: Grammar -> Bool
isInfinitive (Infinitive _ _ _) = True
isInfinitive (ConjunctivePhrase _ _ end _) = isInfinitive end
isInfinitive _ = False

isPrepositionalPhrase :: Grammar -> Bool
isPrepositionalPhrase (PrepositionalPhrase _ _) = True
isPrepositionalPhrase (ConjunctivePhrase _ _ end _) = isPrepositionalPhrase end
isPrepositionalPhrase _ = False

isConjunctivePhrase :: Grammar -> Bool
isConjunctivePhrase (ConjunctivePhrase _ _ _ _) = True
isConjunctivePhrase _ = False

isArticle :: Grammar -> Bool
isArticle (Article _) = True
isArticle _ = False

isNoun :: Grammar -> Bool
isNoun (Noun _ _) = True
isNoun (ConjunctivePhrase _ _ end _) = isNoun end
isNoun _ = False

isAdjective :: Grammar -> Bool
isAdjective (Adjective _) = True
isAdjective (ConjunctivePhrase _ _ end _) = isAdjective end
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

compatiblePluralities :: Plurality -> Plurality -> Bool
compatiblePluralities EitherPlurality _ = True
compatiblePluralities _ EitherPlurality = True
compatiblePluralities x y = x == y

compatiblePersons :: Person -> Person -> Bool
compatiblePersons AnyPerson _ = True
compatiblePersons _ AnyPerson = True
compatiblePersons x y = x == y

class Attributes a where
    getAttrs :: (a -> b) -> Grammar -> Maybe b
    checkAttrs :: (a -> Bool) -> Grammar -> Bool
    checkAttrs =
      let
        verify Nothing = False
        verify (Just x) = x
      in
        -- Apply two arguments to getAttrs before verifying
        liftM (liftM verify) getAttrs
    attributeExists :: Maybe a -> Bool
    attributeExists Nothing = False
    attributeExists (Just _) = True

instance Attributes NounAttributes where
    getAttrs get (Noun _ attributes) = Just $ get attributes
    getAttrs get (NounPhrase _ noun) = getAttrs get noun
    getAttrs get (ArticledNounPhrase _ nounphrase _) = getAttrs get nounphrase
    getAttrs get (Infinitive _ _ attributes)  = Just $ get attributes
    getAttrs get (Subject anp) = getAttrs get anp
    getAttrs get (ConjunctivePhrase _ _ _ (NounConjunction attrs)) =
        Just $ get attrs
    getAttrs _ _ = Nothing


instance Attributes VerbAttributes where
    getAttrs get (Verb _ attributes) = Just $ get attributes
    getAttrs get (RawPredicate verb _) = getAttrs get verb
    getAttrs get (Predicate rawPred _) = getAttrs get rawPred
    getAttrs get (ConjunctivePhrase _ _ _ (VerbConjunction attrs)) =
        Just $ get attrs
    getAttrs _ _ = Nothing

instance Attributes PrepositionAttributes where
    getAttrs get (Preposition _ attributes) = Just $ get attributes
    getAttrs get (PrepositionalPhrase preposition _) = getAttrs get preposition
    getAttrs get (ConjunctivePhrase _ _ _ (PrepConjunction attrs)) =
        Just $ get attrs
    getAttrs _ _ = Nothing
