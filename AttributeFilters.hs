{-# OPTIONS_GHC -Wall #-}

module AttributeFilters where

import Control.Monad

import Attributes
import Grammar

compatiblePluralities :: Plurality -> Plurality -> Bool
compatiblePluralities EitherPlurality _ = True
compatiblePluralities _ EitherPlurality = True
compatiblePluralities x y = x == y

compatiblePersons :: Person -> Person -> Bool
compatiblePersons AnyPerson _ = True
compatiblePersons _ AnyPerson = True
compatiblePersons x y = x == y

attributeExists :: Maybe a -> Bool
attributeExists Nothing = False
attributeExists (Just _) = True

subjectVerbAgreement :: Grammar -> Grammar -> Bool
subjectVerbAgreement subject predicate =
  let
    subjectPerson = getAttrs personN subject
    subjectNumber = getAttrs pluralN subject
    predicatePerson = getAttrs personV predicate
    predicateNumber = getAttrs pluralV predicate
    personCheck =
        liftM2 compatiblePersons subjectPerson predicatePerson
    pluralCheck =
        liftM2 compatiblePluralities subjectNumber predicateNumber
    predicateTense = getAttrs tense predicate
  in
    (predicateTense == Just Past) ||
    (pluralCheck == Just True && personCheck == Just True)

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

instance Attributes NounAttributes where
    getAttrs getter (Noun _ attributes) = Just $ getter attributes
    getAttrs getter (NounPhrase _ noun) = getAttrs getter noun
    getAttrs getter (ArticledNounPhrase _ nounphrase _) =
        getAttrs getter nounphrase
    getAttrs getter (Infinitive _ _ attributes)  = Just $ getter attributes
    getAttrs getter (Subject anp) = getAttrs getter anp
    getAttrs getter (ConjunctivePhrase _ _ _ (NounConjunction attrs)) =
        Just $ getter attrs
    getAttrs _ _ = Nothing

instance Attributes VerbAttributes where
    getAttrs getter (Verb _ attributes) = Just $ getter attributes
    getAttrs getter (RawPredicate verb _) = getAttrs getter verb
    getAttrs getter (Predicate rawPred _) = getAttrs getter rawPred
    getAttrs getter (VerbPhrase _ _ attributes) = Just $ getter attributes
    getAttrs getter (ConjunctivePhrase _ _ _ (VerbConjunction attrs)) =
        Just $ getter attrs
    getAttrs _ _ = Nothing

instance Attributes PrepositionAttributes where
    getAttrs getter (Preposition _ attributes) = Just $ getter attributes
    getAttrs getter (PrepositionalPhrase preposition _) =
        getAttrs getter preposition
    getAttrs getter (ConjunctivePhrase _ _ _ (PrepConjunction attrs)) =
        Just $ getter attrs
    getAttrs _ _ = Nothing
