{-# OPTIONS_GHC -Wall #-}

module Rules where

import Control.Monad
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
prepositionRules :: [Rule]
prepositionRules = [ prepositionalPhraseFromANP
                   , prepositionalPhraseFromSentence]
prepositionalPhraseRules :: [Rule]
prepositionalPhraseRules = []
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
        ArticledNounPhrase Nothing (Infinitive to predicate) []
  in
    makeRule2 isTo isPredicate toInfinitive anpRules

predicateFromRawPredicate :: Rule
predicateFromRawPredicate =
    makeRule1 isRawPredicate (\p -> Predicate p []) predicateRules

rawPredicateFromIntVerb :: Rule
rawPredicateFromIntVerb =
    makeRule1 isVerb (\v -> RawPredicate v Nothing) rawPredicateRules

prepositionalPhraseFromANP :: Rule
prepositionalPhraseFromANP =
    makeRule2 isPreposition isANP PrepositionalPhrase prepositionalPhraseRules

prepositionalPhraseFromSentence :: Rule
prepositionalPhraseFromSentence =
    makeRule2
        isPreposition isSentence PrepositionalPhrase prepositionalPhraseRules

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
    makeRule1 (liftM2 (&&) isANP (checkAttrs canBeSubject)) Subject subjectRules

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
sentenceFromSubject =
    makeRule2 isSubject isPredicate Sentence sentenceRules

fullSentenceFromSentence :: Rule
fullSentenceFromSentence =
    -- FullSentence doesn't store the period, so just gobble that argument.
    makeRule2 isSentence isPeriod (curry fst . FullSentence) fullSentenceRules

predicateWithPrepositionalPhrase :: Rule
predicateWithPrepositionalPhrase =
  let
    toPredicate (Predicate predicate prepPhrases) prepPhrase =
        -- Keep the order of the prepositional phrases.
        Predicate predicate (prepPhrases ++ [prepPhrase])
    toPredicate _ _ = error ("Unexpected nodes when merging predicate and " ++
                             "prepositional phrase!")
  in
    makeRule2 isPredicate isPrepositionalPhrase toPredicate predicateRules

anpWithPrepositionalPhrase :: Rule
anpWithPrepositionalPhrase =
  let
    toANP (ArticledNounPhrase a n prepPhrases) prepPhrase =
        -- Keep the order of the prepositional phrases.
        ArticledNounPhrase a n (prepPhrases ++ [prepPhrase])
    toANP _ _ =
        error "Unexpected nodes when merging ANP and prepositional phrase!"
  in
    makeRule2 isANP isPrepositionalPhrase toANP anpRules
