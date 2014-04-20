module Rules where

import Grammar

fullSentenceRules = []
sentenceRules = [fullSentenceFromSentence]
subjectRules = [sentenceFromSubject]
predicateRules = [predicateWithPrepositionalPhrase]
anpRules = [subjectFromANP, anpWithPrepositionalPhrase]
rawPredicateRules = [predicateFromRawPredicate]
articleRules = [articledNounPhraseFromArticle]
nounPhraseRules = [articledNounPhraseFromNounPhrase]
nounRules = [nounPhraseFromNoun]
adjectiveRules = [nounPhraseFromAdjective]
intVerbRules = [rawPredicateFromIntVerb]
transVerbRules = [rawPredicateFromTransVerb]
prepositionRules = [prepositionalPhraseFromPreposition]
prepositionalPhraseRules = []
eofRules = []
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
    isPredicate (Node (Predicate _ _) _ _) = True
    isPredicate _ = False
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

prepositionalPhraseFromPreposition :: Rule
prepositionalPhraseFromPreposition (Node p@(Preposition _) _ others) =
  let
    isANP (Node (ArticledNounPhrase _ _ _) _ _) = True
    isANP _ = False
    toPrepositionalPhrase :: Node -> Node
    toPrepositionalPhrase (Node nounPhrase _ next) =
        Node (PrepositionalPhrase p nounPhrase) prepositionalPhraseRules next
  in
    map toPrepositionalPhrase . filter isANP $ others
prepositionalPhraseFromPreposition _ = []

rawPredicateFromTransVerb :: Rule
rawPredicateFromTransVerb (Node v@(Verb _) _ others) =
  let
    isANP (Node (ArticledNounPhrase _ _ _) _ _) = True
    isANP _ = False
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
articledNounPhraseFromArticle (Node a@(Article _) rules others) =
  let
    isNounPhrase (Node (NounPhrase _ _) _ _) = True
    isNounPhrase _ = False
    toANP :: Node -> Node
    toANP (Node noun _ next) =
        Node (ArticledNounPhrase (Just a) noun []) anpRules next
  in
    map toANP . filter isNounPhrase $ others
articledNounPhraseFromArticle _ = []

subjectFromANP :: Rule
subjectFromANP (Node n@(ArticledNounPhrase _ _ _) _ next) =
    [Node (Subject n) subjectRules next]
subjectFromANP _ = []

nounPhraseFromNoun :: Rule
nounPhraseFromNoun (Node n@(Noun _) _ next) =
    [Node (NounPhrase [] n) nounPhraseRules next]
nounPhraseFromNoun _ = []

nounPhraseFromAdjective :: Rule
nounPhraseFromAdjective (Node adjective@(Adjective _) _ others) =
  let
    isNounPhrase (Node (NounPhrase _ _) _ _) = True
    isNounPhrase _ = False
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
    isPredicate (Node (Predicate _ _) _ _) = True
    isPredicate _ = False
    toSentence :: Node -> Node
    toSentence (Node predicate _ next) =
        Node (Sentence s predicate) sentenceRules next
  in
    map toSentence . filter isPredicate $ others
sentenceFromSubject _ = []

fullSentenceFromSentence :: Rule
fullSentenceFromSentence (Node s@(Sentence _ _) _ others) =
  let
    isPeriod (Node (Period) _ _) = True
    isPeriod _ = False
    toSentence :: Node -> Node
    toSentence (Node Period _ next) =
        Node (FullSentence s) fullSentenceRules next
  in
    map toSentence . filter isPeriod $ others
fullSentenceFromSentence _ = []

predicateWithPrepositionalPhrase :: Rule
predicateWithPrepositionalPhrase
    (Node p@(Predicate rawPredicate prepositionalPhrases) _ others) =
  let
    isPrepositionalPhrase (Node (PrepositionalPhrase _ _) _ _) = True
    isPrepositionalPhrase _ = False
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
    (Node p@(ArticledNounPhrase article nounPhrase prepositionalPhrases)
          _ others) =
  let
    isPrepositionalPhrase (Node (PrepositionalPhrase _ _) _ _) = True
    isPrepositionalPhrase _ = False
    toANP :: Node -> Node
    toANP (Node prepositionalPhrase _ next) =
        Node (ArticledNounPhrase article nounPhrase
                        -- Keep the order of the prepositional phrases.
                        (prepositionalPhrases ++ [prepositionalPhrase]))
             anpRules next
  in
    map toANP . filter isPrepositionalPhrase $ others
anpWithPrepositionalPhrase _ = []
