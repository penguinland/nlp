{-# OPTIONS_GHC -Wall #-}

module Rules where

import Grammar
import GrammarFilters

fullSentenceRules :: [Rule]
fullSentenceRules = []
sentenceRules :: [Rule]
sentenceRules = [fullSentenceFromSentence]
subjectRules :: [Rule]
subjectRules = [sentenceFromSubject]
predicateRules :: [Rule]
predicateRules = [predicateWithPrepositionalPhrase]
anpRules :: [Rule]
anpRules = [subjectFromANP, anpWithPrepositionalPhrase]
rawPredicateRules :: [Rule]
rawPredicateRules = [predicateFromRawPredicate]
articleRules :: [Rule]
articleRules = [articledNounPhraseFromArticle]
nounPhraseRules :: [Rule]
nounPhraseRules = [articledNounPhraseFromNounPhrase]
nounRules :: [Rule]
nounRules = [nounPhraseFromNoun]
adjectiveRules :: [Rule]
adjectiveRules = [nounPhraseFromAdjective]
intVerbRules :: [Rule]
intVerbRules = [rawPredicateFromIntVerb]
transVerbRules :: [Rule]
transVerbRules = [rawPredicateFromTransVerb]
conjunctionRules :: [Rule]
conjunctionRules = []  -- TODO: fill these in
prepositionRules :: [Rule]
prepositionRules = [ prepositionalPhraseFromANP
                   , prepositionalPhraseFromSentence ]
prepositionalPhraseRules :: [Rule]
prepositionalPhraseRules = [ prepositionalPhraseToList ]
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
    newRule (Node first _ seconds)
      | isCorrectGrammar1 first =
            map (toNewNode first) . filter (liftFilter isCorrectGrammar2) $
                seconds
    newRule _ = []
    toNewNode first (Node second _ next) =
        Node (toNewGrammar first second) nextRules next
  in
    newRule

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
    makeRule2 (isPreposition `andAlso` (checkAttrs canContainNoun))
        isANP PrepositionalPhrase prepositionalPhraseRules

prepositionalPhraseFromSentence :: Rule
prepositionalPhraseFromSentence =
    makeRule2 (isPreposition `andAlso` (checkAttrs canContainSentence))
        isSentence PrepositionalPhrase prepositionalPhraseRules

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
        error "Unexpected nodes when merging ANP and prepositional phrase!"
    isAcceptablePreposition =
        isPrepositionalPhrase `andAlso` (checkAttrs canModifyNoun)
  in
    makeRule2 isANP isAcceptablePreposition toANP anpRules

-- TODO: this is going to introduce massive ambiguity. Is there a way to prevent
-- that?
prepositionalPhraseToList :: Rule
prepositionalPhraseToList node =
  let
    inConjunction test (ConjunctivePhrase _ _ contents) = test contents
    inConjunction _ g = error ("Unexpected non-conjunction " ++ show g)
    nodeAttributes :: Maybe PrepositionAttributes
    nodeAttributes = liftFilter (getAttrs id) node
    isPrepositionalList =
        -- Because monads are associative, we don't care which way the
        -- `andAlso`'s chain.
        isConjunction `andAlso` (inConjunction isPrepositionalPhrase) `andAlso`
        (\p -> nodeAttributes /= Nothing && nodeAttributes == getAttrs id p)
    addToList newPrep (ConjunctivePhrase preps conj final) =
        ConjunctivePhrase (newPrep : preps) conj final
    addToList _ g = error ("Unexpected non-conjunction " ++ show g)
  in
    makeRule2
        isPrepositionalPhrase isPrepositionalList addToList prepListRules node
