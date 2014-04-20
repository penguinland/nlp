{-# OPTIONS_GHC -Wall #-}

module Lexer where

import Data.Set

import Grammar
import qualified Parser
import Rules

lexNodes :: String -> [Node]
lexNodes input =
  let
    -- TODO: get a smarter way of splitting things up
    text = words input
    end = Node EOF [] [end]
    toNodes [] = [end]
    toNodes (word : rest) =
        Parser.applyAllRules $ wordToNodes word (toNodes rest)
  in
    toNodes text

wordToNodes :: String -> [Node] -> [Node]
wordToNodes "." next = [Node Period [] next]
wordToNodes word next =
  let
    result = concatMap (\f -> f word next) makePartsOfSpeech
  in
    if length result == 0 then error ("unknown word: " ++ word) else result

makePartsOfSpeech :: [String -> [Node] -> [Node]]
makePartsOfSpeech = [ makeNoun
                    , makeArticle
                    , makeIntVerb
                    , makeTransVerb
                    , makeAdjective
                    , makePreposition
                    , makeMisc]

-- Yes, I know that many of these are possessive adjectives and not articles.
-- However, they act like articles, so that's what I'm going to call it here. In
-- particular, these are words that could be substituted for the word "the" in
-- the phrase "the big yellow house" but could not be substituted for "big" or
-- "yellow."
articles :: Set String
articles = fromList ["a", "another", "her", "his", "my", "the", "their"]
isArticle :: String -> Bool
isArticle = flip member articles

normalNouns :: Set String
normalNouns = fromList ["ball", "cat", "dog"]
-- TODO: either remove this or fix it in some way.
isNoun :: String -> Bool
isNoun = flip member normalNouns

normalIntransitiveVerbs :: Set String
normalIntransitiveVerbs = fromList ["play", "played", "ran", "run"]
-- TODO: either remove this or fix it in some way.
isIntVerb :: String -> Bool
isIntVerb = flip member normalIntransitiveVerbs

normalTransitiveVerbs :: Set String
normalTransitiveVerbs = fromList [
    "chew", "chewed", "eat", "found", "love", "play", "threw", "was"]
-- TODO: either remove this or fix it in some way.
isTransVerb :: String -> Bool
isTransVerb = flip member normalTransitiveVerbs

adjectives :: Set String
adjectives = fromList ["big", "blue", "hungry", "red", "yellow"]
isAdjective :: String -> Bool
isAdjective = flip member adjectives

prepositions :: Set String
prepositions = fromList ["after", "to", "when", "with"]
isPreposition :: String -> Bool
isPreposition = flip member prepositions

addRule :: Node -> Rule -> Node
addRule (Node grammar rules next) newRule = Node grammar (newRule : rules) next

makeNode :: (String -> Bool) -> (String -> Grammar) -> [Rule] ->
            String -> [Node] -> [Node]
makeNode isType nodeType rules word next =
    if   isType word
    then [Node (nodeType word) rules next]
    else []

makeNoun :: String -> [Node] -> [Node]
makeNoun word next =
  let
    singularNounAttributes = NounAttributes True True False
    pluralNounAttributes = NounAttributes True True True
  in
    if member word normalNouns
    then [Node (Noun word singularNounAttributes) nounRules next]
    -- TODO: fix this for nouns that end is 's'.
    else if last word == 's' && member (init word) normalNouns
    then [Node (Noun word pluralNounAttributes) nounRules next]
    else []

makeArticle :: String -> [Node] -> [Node]
makeArticle = makeNode isArticle Article articleRules

makeIntVerb :: String -> [Node] -> [Node]
makeIntVerb word next =
    if member word normalIntransitiveVerbs
    then [Node (Verb word) intVerbRules next]
    -- TODO: fix this for verbs that end in 's'.
    else if last word == 's' && member (init word) normalIntransitiveVerbs
    then [Node (Verb word) intVerbRules next]
    else []

-- TODO: Can you think of a verb that *requires* a direct object?
makeTransVerb :: String -> [Node] -> [Node]
makeTransVerb word next =
    if member word normalTransitiveVerbs
    then [Node (Verb word) (intVerbRules ++ transVerbRules) next]
    -- TODO: fix this for verbs that end in 's'.
    else if last word == 's' && member (init word) normalTransitiveVerbs
    then [Node (Verb word) (intVerbRules ++ transVerbRules) next]
    else []

makeAdjective :: String -> [Node] -> [Node]
makeAdjective = makeNode isAdjective Adjective adjectiveRules

-- TODO: refactor this, maybe?.
permissivePreposition :: PrepositionAttributes
permissivePreposition = PrepositionAttributes True True True True
makePreposition :: String -> [Node] -> [Node]
-- "When" should always be followed by a sentence when used as a prepositional
-- phrase.
makePreposition "when" next =
    [Node (Preposition "when" permissivePreposition{canContainNoun = False})
     prepositionRules next]
-- "To" might be an infinitive.
makePreposition "to" next =
    [Node (Preposition "to" permissivePreposition)
     (infinitiveRule : prepositionRules) next]
makePreposition word next =
    makeNode isPreposition (flip Preposition permissivePreposition)
        prepositionRules word next

makeMisc :: String -> [Node] -> [Node]
makeMisc "I" next = [Node (Noun "I" (NounAttributes { canBeSubject = True
                                                    , canBeObject = False
                                                    , isPlural = False}))
                          nounRules next]
makeMisc "he" next = [Node (Noun "he" (NounAttributes { canBeSubject = True
                                                      , canBeObject = False
                                                      , isPlural = False}))
                           nounRules next]
makeMisc "me" next = [Node (Noun "me" (NounAttributes { canBeSubject = False
                                                      , canBeObject = True
                                                      , isPlural = False}))
                           nounRules next]
makeMisc "it" next = [Node (Noun "it" (NounAttributes { canBeSubject = True
                                                      , canBeObject = True
                                                      , isPlural = False}))
                           nounRules next]
makeMisc _ _ = []
