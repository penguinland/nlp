{-# OPTIONS_GHC -Wall #-}

module Lexer where

import qualified Data.Set

import Attributes
import Grammar
import LexerHelpers
import qualified Parser
import Rules

import Nouns
import Verbs

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
wordToNodes word next
  | last word == '.' = wordToNodes (init word) (wordToNodes "." next)
  | last word == '?' = wordToNodes (init word) (wordToNodes "?" next)
  | otherwise =
      let
        result = concatMap (\f -> f word next) makePartsOfSpeech
      in
        if length result == 0 then error ("unknown word: " ++ word) else result

makePartsOfSpeech :: [String -> [Node] -> [Node]]
makePartsOfSpeech = [ makeNoun
                    , makeArticle
                    , makeIntVerb
                    , makeTransVerb
                    , makeUnusualVerbs
                    , makeAdjective
                    , makePreposition
                    , makeMisc]

-- Yes, I know that many of these are possessive adjectives and not articles.
-- However, they act like articles, so that's what I'm going to call it here. In
-- particular, these are words that could be substituted for the word "the" in
-- the phrase "the big yellow house" but could not be substituted for "big" or
-- "yellow."
-- TODO: eventually split out articles and possessive adjectives, maybe?
articles :: Data.Set.Set String
articles = Data.Set.fromList ["a", "another", "her", "his", "my", "the",
    "their"]
makeArticle :: String -> [Node] -> [Node]
makeArticle = makeNode articles Article articleRules

adjectives :: Data.Set.Set String
adjectives = Data.Set.fromList ["big", "blue", "hungry", "red", "yellow"]
makeAdjective :: String -> [Node] -> [Node]
makeAdjective = makeNode adjectives Adjective adjectiveRules

prepositions :: Data.Set.Set String
prepositions = Data.Set.fromList ["after", "in", "of", "over", "through",
    "with"]
permissivePreposition :: PrepositionAttributes
permissivePreposition = PrepositionAttributes True True
makePreposition :: String -> [Node] -> [Node]
-- "To" might be an infinitive.
makePreposition "to" next =
    [Node (Preposition "to" permissivePreposition)
     (infinitiveRule : prepositionRules) next]
makePreposition word next =
    makeNode prepositions (flip Preposition permissivePreposition)
        prepositionRules word next

addRule :: Node -> Rule -> Node
addRule (Node grammar rules next) newRule = Node grammar (newRule : rules) next

makeMisc :: String -> [Node] -> [Node]
makeMisc "and" next = [Node (Conjunction "and") conjunctionRules next]
makeMisc "when" next = [Node (Conjunction "when") conjunctionRules next]
makeMisc "because" next = [Node (Conjunction "because") conjunctionRules next]
makeMisc _ _ = []
