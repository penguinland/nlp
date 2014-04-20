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
wordToNodes "to" next = [Node (Preposition "to") [infinitiveRule] next]
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
                    , makePreposition]

-- Yes, I know that many of these are possessive adjectives and not articles.
-- However, they act like articles, so that's what I'm going to call it here. In
-- particular, these are words that could be substituted for the word "the" in
-- the phrase "the big yellow house" but could not be substituted for "big" or
-- "yellow."
articles :: Set String
articles = fromList ["a", "another", "her", "his", "my", "the", "their"]
isArticle :: String -> Bool
isArticle = flip member articles

nouns :: Set String
nouns = fromList ["ball", "cat", "dog", "he", "it"]
isNoun :: String -> Bool
isNoun = flip member nouns

intransitiveVerbs :: Set String
intransitiveVerbs = fromList ["played", "ran", "run", "runs"]
isIntVerb :: String -> Bool
isIntVerb = flip member intransitiveVerbs

transitiveVerbs :: Set String
transitiveVerbs = fromList [
    "chew", "chewed", "eats", "found", "loves", "play", "threw", "was"]
isTransVerb :: String -> Bool
isTransVerb = flip member transitiveVerbs

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
makeNoun = makeNode isNoun (\w -> Noun w (NounAttributes True True)) nounRules

makeArticle :: String -> [Node] -> [Node]
makeArticle = makeNode isArticle Article articleRules

makeIntVerb :: String -> [Node] -> [Node]
makeIntVerb = makeNode isIntVerb Verb intVerbRules

-- Can you think of a verb that *requires* a direct object?
makeTransVerb :: String -> [Node] -> [Node]
makeTransVerb = makeNode isTransVerb Verb (transVerbRules ++ intVerbRules)

makeAdjective :: String -> [Node] -> [Node]
makeAdjective = makeNode isAdjective Adjective adjectiveRules

makePreposition :: String -> [Node] -> [Node]
makePreposition = makeNode isPreposition Preposition prepositionRules
