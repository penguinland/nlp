{-# OPTIONS_GHC -Wall #-}

module Rules where

import Control.Monad

import Attributes
import AttributeFilters
import Grammar
import GrammarFilters
import RuleGenerators

-- I would like to split these lists out to a separate file, but cannot. The
-- file with the lists would need to import the file with all the functions, and
-- the file with all the functions would need to import the file with the lists
-- (because the Rules create new Nodes that use these lists).
fullSentenceRules :: [Rule]
fullSentenceRules = []
sentenceRules :: [Rule]
sentenceRules = [ fullSentenceFromSentence
                , sentenceAndSentence ]
subjectRules :: [Rule]
subjectRules = [ sentenceFromSubject ]
predicateRules :: [Rule]
predicateRules = [predicateWithPrepositionalPhrase, predicateAndPredicate]
anpRules :: [Rule]
anpRules = [subjectFromANP, anpWithPrepositionalPhrase, anpAndAnp]
rawPredicateRules :: [Rule]
rawPredicateRules = [predicateFromRawPredicate]
articleRules :: [Rule]
articleRules = [articledNounPhraseFromArticle]
nounPhraseRules :: [Rule]
nounPhraseRules = [articledNounPhraseFromNounPhrase]
nounRules :: [Rule]
nounRules = [nounPhraseFromNoun, nounAndNoun]
adjectiveRules :: [Rule]
adjectiveRules = [nounPhraseFromAdjective, adjectiveAndAdjective]
intVerbRules :: [Rule]
intVerbRules = [rawPredicateFromIntVerb]
transVerbRules :: [Rule]
transVerbRules = [rawPredicateFromTransVerb]
conjunctionRules :: [Rule]
conjunctionRules = []  -- TODO: fill these in
prepositionRules :: [Rule]
prepositionRules = [prepositionalPhraseFromANP]
prepositionalPhraseRules :: [Rule]
prepositionalPhraseRules = [ prepositionAndPreposition ]
prepListRules :: [Rule]
prepListRules = []  -- TODO: fill this in
eofRules :: [Rule]
eofRules = []
periodRules :: [Rule]
periodRules = []

prepositionAndPreposition :: Rule
prepositionAndPreposition =
  let
    prepositionsMatch left _ right =
      let
        leftAttrs :: Maybe PrepositionAttributes
        leftAttrs = getAttrs id left
        rightAttrs :: Maybe PrepositionAttributes
        rightAttrs = getAttrs id right
      in
        attributeExists leftAttrs && leftAttrs == rightAttrs
    conjoinPrepositions left conjunction right =
      let
        Just attributes = getAttrs id right
      in
        ConjunctivePhrase [left] conjunction right (PrepConjunction attributes)
  in
    makeRule3 isPrepositionalPhrase isConjunction isPrepositionalPhrase
        prepositionsMatch conjoinPrepositions prepositionalPhraseRules

sentenceAndSentence :: Rule
sentenceAndSentence =
  let
    conjoinSentences left conjunction right =
        ConjunctivePhrase [left] conjunction right OtherConjunction
  in
    makeRule3 isSentence isConjunction isSentence constTrue3
        conjoinSentences sentenceRules

adjectiveAndAdjective :: Rule
adjectiveAndAdjective =
  let
    conjoinAdjectives left conjunction right =
        ConjunctivePhrase [left] conjunction right OtherConjunction
  in
    makeRule3 isAdjective isConjunction isAdjective constTrue3
        conjoinAdjectives adjectiveRules

predicateAndPredicate :: Rule
predicateAndPredicate =
  let
    verbsMatch left _ right =
      let
        leftAttrs :: Maybe VerbAttributes
        leftAttrs = getAttrs id left
        rightAttrs :: Maybe VerbAttributes
        rightAttrs = getAttrs id right
      in
        attributeExists leftAttrs && leftAttrs == rightAttrs
    conjoinPredicates left conjunction right =
      let
        Just attributes = getAttrs id right
      in
        ConjunctivePhrase [left] conjunction right (VerbConjunction attributes)
  in
    makeRule3 isPredicate isConjunction isPredicate verbsMatch
        conjoinPredicates sentenceRules

nounlikeAndNounlike :: (Grammar -> Bool) -> [Rule] -> Rule
nounlikeAndNounlike nounlike rules =
  let
    nounsMatch left _ right =
      let
        leftAttrs :: Maybe NounAttributes
        leftAttrs = getAttrs id left
        rightAttrs :: Maybe NounAttributes
        rightAttrs = getAttrs id right
      in
        attributeExists leftAttrs && leftAttrs == rightAttrs
    pluralize noun =
      let
        Just attrs = getAttrs id noun
      in
        attrs{pluralN = Plural}
    conjoinNouns left conjunction right =
        ConjunctivePhrase [left] conjunction right
            (NounConjunction $ pluralize right)
  in
     makeRule3 nounlike isConjunction nounlike nounsMatch conjoinNouns rules

nounAndNoun :: Rule
nounAndNoun = nounlikeAndNounlike isNoun nounRules

nounPhraseAndNounPhrase :: Rule
nounPhraseAndNounPhrase = nounlikeAndNounlike isNounPhrase nounPhraseRules

anpAndAnp :: Rule
anpAndAnp = nounlikeAndNounlike isANP anpRules

infinitiveRule :: Rule
infinitiveRule =
  let
    isTo (Preposition "to" _) = True
    isTo _ = False
    toInfinitive to predicate =
        ArticledNounPhrase Nothing
            (Infinitive to predicate
                 (NounAttributes True True Singular ThirdPerson)) []
    notConjugated g = getAttrs personV g == Just OtherPerson
  in
    makeRule2 isTo (isPredicate `andAlso` notConjugated) constTrue2
        toInfinitive anpRules

predicateFromRawPredicate :: Rule
predicateFromRawPredicate =
    makeRule1 isRawPredicate (\p -> Predicate p []) predicateRules

rawPredicateFromIntVerb :: Rule
rawPredicateFromIntVerb =
    makeRule1 isVerb (\v -> RawPredicate v Nothing) rawPredicateRules

prepositionalPhraseFromANP :: Rule
prepositionalPhraseFromANP =
    makeRule2 isPreposition isANP constTrue2 PrepositionalPhrase
        prepositionalPhraseRules

rawPredicateFromTransVerb :: Rule
rawPredicateFromTransVerb =
    makeRule2 isVerb isANP constTrue2 (\v n -> RawPredicate v (Just n))
        rawPredicateRules

articledNounPhraseFromNounPhrase :: Rule
articledNounPhraseFromNounPhrase =
    makeRule1 isNounPhrase (\n -> ArticledNounPhrase Nothing n []) anpRules

articledNounPhraseFromArticle :: Rule
articledNounPhraseFromArticle =
    makeRule2 isArticle isNounPhrase constTrue2
        (\a n -> ArticledNounPhrase (Just a) n []) anpRules

subjectFromANP :: Rule
subjectFromANP =
    makeRule1 (isANP `andAlso` (checkAttrs canBeSubject)) Subject subjectRules

nounPhraseFromNoun :: Rule
nounPhraseFromNoun =
    makeRule1 isNoun (\n -> NounPhrase [] n) nounPhraseRules

nounPhraseFromAdjective :: Rule
nounPhraseFromAdjective =
  let
    toNounPhrase adjective (NounPhrase adjectives noun) =
        NounPhrase (adjective : adjectives) noun
    -- It's safe to use an error here; this cannot be called.
    toNounPhrase _ _ = error "Unexpected non-NounPhrase node!"
  in
    makeRule2 isAdjective isNounPhrase constTrue2 toNounPhrase nounPhraseRules

sentenceFromSubject :: Rule
sentenceFromSubject =
  let
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
  in
    makeRule2 isSubject isPredicate subjectVerbAgreement Sentence sentenceRules

fullSentenceFromSentence :: Rule
fullSentenceFromSentence =
    makeRule2 isSentence isPeriod constTrue2 FullSentence fullSentenceRules

predicateWithPrepositionalPhrase :: Rule
predicateWithPrepositionalPhrase =
  let
    toPredicate (Predicate predicate prepPhrases) prepPhrase =
        -- Keep the order of the prepositional phrases.
        Predicate predicate (prepPhrases ++ [prepPhrase])
    -- It's safe to use an error here; this cannot be called.
    toPredicate _ _ = error ("Unexpected nodes when merging predicate and " ++
                             "prepositional phrase!")
    isAcceptablePreposition =
        isPrepositionalPhrase `andAlso` (checkAttrs canModifyVerb)
  in
    makeRule2 isPredicate isAcceptablePreposition constTrue2 toPredicate
        predicateRules

anpWithPrepositionalPhrase :: Rule
anpWithPrepositionalPhrase =
  let
    toANP (ArticledNounPhrase a n prepPhrases) prepPhrase =
        -- Keep the order of the prepositional phrases.
        ArticledNounPhrase a n (prepPhrases ++ [prepPhrase])
    toANP _ _ =
        -- It's safe to use an error here; this cannot be called.
        error "Unexpected nodes when merging ANP and prepositional phrase!"
    isAcceptablePreposition =
        isPrepositionalPhrase `andAlso` (checkAttrs canModifyNoun)
  in
    makeRule2 isANP isAcceptablePreposition constTrue2 toANP anpRules
