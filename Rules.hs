{-# OPTIONS_GHC -Wall #-}

module Rules where

import Grammar
import GrammarFilters

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

makeRule1 :: (Grammar -> Bool) -> (Grammar -> Grammar) -> [Rule] -> Rule
makeRule1 isCorrectGrammar toNewGrammar nextRules =
  let
    newRule (Node g _ next)
      | isCorrectGrammar g = [Node (toNewGrammar g) nextRules next]
    newRule _ = []
  in
    newRule

makeRule2 :: (Grammar -> Bool) -> (Grammar -> Bool) ->
             (Grammar -> Grammar -> Grammar) -> [Rule] -> Rule
makeRule2 isCorrectGrammar1 isCorrectGrammar2 toNewGrammar nextRules =
  let
    toNewNode first (Node second _ next) =
        Node (toNewGrammar first second) nextRules next
    newRule (Node first _ seconds) =
        [toNewNode first second | isCorrectGrammar1 first,
         second <- seconds, liftFilter isCorrectGrammar2 second]
  in
    newRule

-- This is for joining nodes together with "and"
conjoin :: (Grammar -> Bool) -> (Grammar -> Bool) -> (Grammar -> Bool) ->
           (Grammar -> Grammar -> Bool) ->
           (Grammar -> Grammar -> Grammar -> Grammar) -> [Rule] -> Rule
conjoin isCorrectGrammar1 isCorrectGrammar2 isCorrectGrammar3 grammars13Match
        toNewGrammar nextRules =
  let
    toNewNode first second third next =
        Node (toNewGrammar first second third) nextRules next
    newRule (Node firstG _ secondNs) =
        [toNewNode firstG secondG thirdG nextNs | isCorrectGrammar1 firstG,
         (Node secondG _ thirdNs) <- secondNs, isCorrectGrammar2 secondG,
         (Node thirdG _ nextNs) <- thirdNs, isCorrectGrammar3 thirdG,
         grammars13Match firstG thirdG]
  in
    newRule

prepositionAndPreposition :: Rule
prepositionAndPreposition =
  let
    prepositionsMatch left right =
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
    conjoin isPrepositionalPhrase isConjunction isPrepositionalPhrase
        prepositionsMatch conjoinPrepositions prepositionalPhraseRules

sentenceAndSentence :: Rule
sentenceAndSentence =
  let
    conjoinSentences left conjunction right =
        ConjunctivePhrase [left] conjunction right OtherConjunction
  in
    conjoin isSentence isConjunction isSentence (const . const $ True)
        conjoinSentences sentenceRules

adjectiveAndAdjective :: Rule
adjectiveAndAdjective =
  let
    conjoinAdjectives left conjunction right =
        ConjunctivePhrase [left] conjunction right OtherConjunction
  in
    conjoin isAdjective isConjunction isAdjective (const . const $ True)
        conjoinAdjectives adjectiveRules

predicateAndPredicate :: Rule
predicateAndPredicate =
  let
    verbsMatch left right =
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
    conjoin isPredicate isConjunction isPredicate verbsMatch
        conjoinPredicates sentenceRules

nounlikeAndNounlike :: (Grammar -> Bool) -> [Rule] -> Rule
nounlikeAndNounlike nounlike rules =
  let
    nounsMatch left right =
      let
        leftAttrs :: Maybe NounAttributes
        leftAttrs = getAttrs id left
        rightAttrs :: Maybe NounAttributes
        rightAttrs = getAttrs id right
      in
        leftAttrs == rightAttrs
    pluralize noun =
      let
        Just attrs = getAttrs id noun
      in
        attrs{isPluralN = True}
    conjoinNouns left conjunction right =
        ConjunctivePhrase [left] conjunction right
            (NounConjunction $ pluralize right)
  in
     conjoin nounlike isConjunction nounlike nounsMatch conjoinNouns rules

nounAndNoun :: Rule
nounAndNoun = nounlikeAndNounlike isNoun nounRules

-- TODO: is there ever a time to conjoin noun phrases in which they're not both
-- articled?
--nounPhraseAndNounPhrase :: Rule
--nounPhraseAndNounPhrase = nounlikeAndNounlike isNounPhrase nounPhraseRules

anpAndAnp :: Rule
anpAndAnp = nounlikeAndNounlike isANP anpRules

infinitiveRule :: Rule
infinitiveRule =
  let
    isTo (Preposition "to" _) = True
    isTo _ = False
    toInfinitive to predicate =
        ArticledNounPhrase Nothing
            (Infinitive to predicate (NounAttributes True True False Third)) []
    notConjugated g = getAttrs personV g == Just Other
  in
    makeRule2 isTo (isPredicate `andAlso` notConjugated) toInfinitive anpRules

predicateFromRawPredicate :: Rule
predicateFromRawPredicate =
    makeRule1 isRawPredicate (\p -> Predicate p []) predicateRules

rawPredicateFromIntVerb :: Rule
rawPredicateFromIntVerb =
    makeRule1 isVerb (\v -> RawPredicate v Nothing) rawPredicateRules

prepositionalPhraseFromANP :: Rule
prepositionalPhraseFromANP =
    makeRule2 isPreposition isANP PrepositionalPhrase prepositionalPhraseRules

rawPredicateFromTransVerb :: Rule
rawPredicateFromTransVerb =
    makeRule2 isVerb isANP (\v n -> RawPredicate v (Just n)) rawPredicateRules

articledNounPhraseFromNounPhrase :: Rule
articledNounPhraseFromNounPhrase =
    makeRule1 isNounPhrase (\n -> ArticledNounPhrase Nothing n []) anpRules

articledNounPhraseFromArticle :: Rule
articledNounPhraseFromArticle =
    makeRule2 isArticle isNounPhrase (\a n -> ArticledNounPhrase (Just a) n [])
        anpRules

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
    makeRule2 isAdjective isNounPhrase toNounPhrase nounPhraseRules

sentenceFromSubject :: Rule
sentenceFromSubject node =
  let
    subjectPerson = liftFilter (getAttrs personN) node
    subjectNumber = liftFilter (getAttrs isPluralN) node
    subjectVerbAgreement :: Grammar -> Bool
    subjectVerbAgreement predicate =
        subjectPerson /= Nothing && subjectNumber /= Nothing &&
        subjectPerson == getAttrs personV predicate &&
        subjectNumber == getAttrs isPluralV predicate
  in
    makeRule2 isSubject (isPredicate `andAlso` subjectVerbAgreement)
        Sentence sentenceRules node

fullSentenceFromSentence :: Rule
fullSentenceFromSentence =
    -- FullSentence doesn't store the period, so just gobble that argument.
    makeRule2 isSentence isPeriod (const . FullSentence) fullSentenceRules

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
    makeRule2 isPredicate isAcceptablePreposition toPredicate predicateRules

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
    makeRule2 isANP isAcceptablePreposition toANP anpRules
