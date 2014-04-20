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
prepositionRules :: [Rule]
prepositionRules = [ prepositionalPhraseFromANP
                   , prepositionalPhraseFromSentence]
prepositionalPhraseRules :: [Rule]
prepositionalPhraseRules = []
eofRules :: [Rule]
eofRules = []
periodRules :: [Rule]
periodRules = []

{-
 -- This doesn't compile because grammarInner cannot be both an argument to the
 -- function and a pattern to match against. Consider doing this with Template
 -- Haskell instead.
makeRuleA1 :: (a -> Grammar) -> (Grammar -> Grammar) [Rule] -> Rule
makeRuleA1 grammarInner grammarOuter nextRules =
  let
    newRule (Node g@(grammarInner _) _ next) =
        [Node (grammarOuter g) nextRules next]
    newRule _ = []
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
    map toInfinitive . filter isPredicate $ others

predicateFromRawPredicate :: Rule
predicateFromRawPredicate (Node p@(RawPredicate _ _) _ next) =
    [Node (Predicate p []) predicateRules next]
predicateFromRawPredicate _ = []

rawPredicateFromIntVerb :: Rule
rawPredicateFromIntVerb (Node v@(Verb _) _ next) =
    [Node (RawPredicate v Nothing) rawPredicateRules next]
rawPredicateFromIntVerb _ = []

prepositionalPhraseFromANP :: Rule
prepositionalPhraseFromANP (Node p@(Preposition _) _ others) =
  let
    toPrepositionalPhrase :: Node -> Node
    toPrepositionalPhrase (Node nounPhrase _ next) =
        Node (PrepositionalPhrase p nounPhrase) prepositionalPhraseRules next
  in
    map toPrepositionalPhrase . filter isANP $ others
prepositionalPhraseFromANP _ = []

prepositionalPhraseFromSentence :: Rule
prepositionalPhraseFromSentence (Node p@(Preposition _) _ others) =
  let
    toPrepositionalPhrase :: Node -> Node
    toPrepositionalPhrase (Node sentence _ next) =
        Node (PrepositionalPhrase p sentence) prepositionalPhraseRules next
  in
    map toPrepositionalPhrase . filter isSentence $ others
prepositionalPhraseFromSentence _ = []

rawPredicateFromTransVerb :: Rule
rawPredicateFromTransVerb (Node v@(Verb _) _ others) =
  let
    toPredicate :: Node -> Node
    toPredicate (Node nounPhrase _ next) =
        Node (RawPredicate v (Just nounPhrase)) rawPredicateRules next
  in
    map toPredicate . filter isANP $ others
rawPredicateFromTransVerb _ = []

articledNounPhraseFromNounPhrase :: Rule
articledNounPhraseFromNounPhrase (Node n@(NounPhrase _ _) _ next) =
    [Node (ArticledNounPhrase Nothing n []) anpRules next]
articledNounPhraseFromNounPhrase _ = []

articledNounPhraseFromArticle :: Rule
articledNounPhraseFromArticle (Node a@(Article _) _ others) =
  let
    toANP :: Node -> Node
    toANP (Node noun _ next) =
        Node (ArticledNounPhrase (Just a) noun []) anpRules next
  in
    map toANP . filter isNounPhrase $ others
articledNounPhraseFromArticle _ = []

subjectFromANP :: Rule
subjectFromANP (Node n@(ArticledNounPhrase _ _ _) _ next)
 | testNoun canBeSubject n = [Node (Subject n) subjectRules next]
subjectFromANP _ = []

nounPhraseFromNoun :: Rule
nounPhraseFromNoun (Node n@(Noun _ _) _ next) =
    [Node (NounPhrase [] n) nounPhraseRules next]
nounPhraseFromNoun _ = []

nounPhraseFromAdjective :: Rule
nounPhraseFromAdjective (Node adjective@(Adjective _) _ others) =
  let
    toNounPhrase :: Node -> Node
    toNounPhrase (Node (NounPhrase adjectives noun) _ next) =
        Node (NounPhrase (adjective : adjectives) noun) nounPhraseRules next
    toNounPhrase _ = error "Unexpected non-NounPhrase node!"
  in
    map toNounPhrase . filter isNounPhrase $ others
nounPhraseFromAdjective _ = []

sentenceFromSubject :: Rule
sentenceFromSubject (Node s@(Subject _) _ others) =
  let
    toSentence :: Node -> Node
    toSentence (Node predicate _ next) =
        Node (Sentence s predicate) sentenceRules next
  in
    map toSentence . filter isPredicate $ others
sentenceFromSubject _ = []

fullSentenceFromSentence :: Rule
fullSentenceFromSentence (Node s@(Sentence _ _) _ others) =
  let
    toSentence :: Node -> Node
    toSentence (Node Period _ next) =
        Node (FullSentence s) fullSentenceRules next
    toSentence _ = error "Unexpected non-Period node!"
  in
    map toSentence . filter isPeriod $ others
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
    map toPredicate . filter isPrepositionalPhrase $ others
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
    map toANP . filter isPrepositionalPhrase $ others
anpWithPrepositionalPhrase _ = []
