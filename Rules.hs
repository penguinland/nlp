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
makeRule1 isCorrectNode toNewGrammar nextRules =
  let
    newRule (Node g _ next)
      | isCorrectNode g = [Node (toNewGrammar g) nextRules next]
    newRule _ = []
  in
    newRule

{-
makeRule2 :: (Node -> Bool) -> (Node -> Bool) ->
             (Grammar -> Grammar -> Grammar) -> [Rule] -> Rule
makeRule2 isCorrectFirstNode isCorrectSecondNode toNewGrammar nextRules =
  let
    newRule (Node first _ others)
      | isCorrectFirstNode first =
            map (toNewNode first) . filter isCorrectSecondNode $ others
    toNewNode first (Node second _ next) =
        Node (toNewGrammar first second) nextRules next
  in
    newRule
-}

infinitiveRule :: Rule
infinitiveRule (Node to _ others) =
  let
    toInfinitive :: Node -> Node
    toInfinitive (Node predicate _ next) =
        Node (ArticledNounPhrase Nothing (Infinitive to predicate) [])
              anpRules next
  in
    map toInfinitive . filter (liftFilter isPredicate) $ others

predicateFromRawPredicate :: Rule
predicateFromRawPredicate =
    makeRule1 isRawPredicate (\p -> Predicate p []) predicateRules

rawPredicateFromIntVerb :: Rule
rawPredicateFromIntVerb =
    makeRule1 isVerb (\v -> RawPredicate v Nothing) rawPredicateRules

prepositionalPhraseFromANP :: Rule
prepositionalPhraseFromANP (Node p@(Preposition _) _ others) =
  let
    toPrepositionalPhrase :: Node -> Node
    toPrepositionalPhrase (Node nounPhrase _ next) =
        Node (PrepositionalPhrase p nounPhrase) prepositionalPhraseRules next
  in
    map toPrepositionalPhrase . filter (liftFilter isANP) $ others
prepositionalPhraseFromANP _ = []

prepositionalPhraseFromSentence :: Rule
prepositionalPhraseFromSentence (Node p@(Preposition _) _ others) =
  let
    toPrepositionalPhrase :: Node -> Node
    toPrepositionalPhrase (Node sentence _ next) =
        Node (PrepositionalPhrase p sentence) prepositionalPhraseRules next
  in
    map toPrepositionalPhrase . filter (liftFilter isSentence) $ others
prepositionalPhraseFromSentence _ = []

rawPredicateFromTransVerb :: Rule
rawPredicateFromTransVerb (Node v@(Verb _) _ others) =
  let
    toPredicate :: Node -> Node
    toPredicate (Node nounPhrase _ next) =
        Node (RawPredicate v (Just nounPhrase)) rawPredicateRules next
  in
    map toPredicate . filter (liftFilter isANP) $ others
rawPredicateFromTransVerb _ = []

articledNounPhraseFromNounPhrase :: Rule
articledNounPhraseFromNounPhrase =
    makeRule1 isNounPhrase (\n -> ArticledNounPhrase Nothing n []) anpRules

articledNounPhraseFromArticle :: Rule
articledNounPhraseFromArticle (Node a@(Article _) _ others) =
  let
    toANP :: Node -> Node
    toANP (Node noun _ next) =
        Node (ArticledNounPhrase (Just a) noun []) anpRules next
  in
    map toANP . filter (liftFilter isNounPhrase) $ others
articledNounPhraseFromArticle _ = []

subjectFromANP :: Rule
subjectFromANP =
    makeRule1 (liftM2 (&&) isANP (testNoun canBeSubject)) Subject subjectRules

nounPhraseFromNoun :: Rule
nounPhraseFromNoun =
    makeRule1 isNoun (\n -> NounPhrase [] n) nounPhraseRules

nounPhraseFromAdjective :: Rule
nounPhraseFromAdjective (Node adjective@(Adjective _) _ others) =
  let
    toNounPhrase :: Node -> Node
    toNounPhrase (Node (NounPhrase adjectives noun) _ next) =
        Node (NounPhrase (adjective : adjectives) noun) nounPhraseRules next
    toNounPhrase _ = error "Unexpected non-NounPhrase node!"
  in
    map toNounPhrase . filter (liftFilter isNounPhrase) $ others
nounPhraseFromAdjective _ = []

sentenceFromSubject :: Rule
sentenceFromSubject (Node s@(Subject _) _ others) =
  let
    toSentence :: Node -> Node
    toSentence (Node predicate _ next) =
        Node (Sentence s predicate) sentenceRules next
  in
    map toSentence . filter (liftFilter isPredicate) $ others
sentenceFromSubject _ = []

fullSentenceFromSentence :: Rule
fullSentenceFromSentence (Node s@(Sentence _ _) _ others) =
  let
    toSentence :: Node -> Node
    toSentence (Node Period _ next) =
        Node (FullSentence s) fullSentenceRules next
    toSentence _ = error "Unexpected non-Period node!"
  in
    map toSentence . filter (liftFilter isPeriod) $ others
fullSentenceFromSentence _ = []

predicateWithPrepositionalPhrase :: Rule
predicateWithPrepositionalPhrase
    (Node (Predicate rawPredicate prepositionalPhrases) _ others) =
  let
    toPredicate :: Node -> Node
    toPredicate (Node prepositionalPhrase _ next) =
        Node (Predicate rawPredicate
                        -- Keep the order of the prepositional phrases.
                        (prepositionalPhrases ++ [prepositionalPhrase]))
             fullSentenceRules next
  in
    map toPredicate . filter (liftFilter isPrepositionalPhrase) $ others
predicateWithPrepositionalPhrase _ = []

anpWithPrepositionalPhrase :: Rule
anpWithPrepositionalPhrase
    (Node (ArticledNounPhrase article nounPhrase prepositionalPhrases)
          _ others) =
  let
    toANP :: Node -> Node
    toANP (Node prepositionalPhrase _ next) =
        Node (ArticledNounPhrase article nounPhrase
                        -- Keep the order of the prepositional phrases.
                        (prepositionalPhrases ++ [prepositionalPhrase]))
             anpRules next
  in
    map toANP . filter (liftFilter isPrepositionalPhrase) $ others
anpWithPrepositionalPhrase _ = []
