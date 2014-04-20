module Lexer where

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

makePartsOfSpeech = [ makeNoun
                    , makeArticle
                    , makeIntVerb
                    , makeTransVerb
                    , makeAdjective
                    , makePreposition]

isArticle :: String -> Bool
isArticle "the" = True
isArticle "a" = True
-- Yes, I know that "my" is a possessive adjective and not an article. However,
-- it acts like an article, so that's what I'm going to call it here.
isArticle "my" = True
isArticle "his" = True
isArticle "her" = True
isArticle "their" = True
-- Similarly for "another," which acts like an article even though it's not one.
isArticle "another" = True
isArticle _ = False

isNoun :: String -> Bool
isNoun "cat" = True
isNoun "dog" = True
isNoun "ball" = True
isNoun "it" = True
isNoun "he" = True
isNoun _ = False

isIntVerb :: String -> Bool
isIntVerb "runs" = True
isIntVerb "played" = True
isIntVerb _ = False

isTransVerb :: String -> Bool
isTransVerb "eats" = True
isTransVerb "loves" = True
isTransVerb "chewed" = True
isTransVerb "chew" = True
isTransVerb "found" = True
isTransVerb "was" = True
isTransVerb "play" = True
isTransVerb _ = False

isAdjective :: String -> Bool
isAdjective "yellow" = True
isAdjective "red" = True
isAdjective "blue" = True
isAdjective "hungry" = True
isAdjective "big" = True
isAdjective _ = False

isPreposition :: String -> Bool
isPreposition "to" = True
isPreposition "with" = True
isPreposition "when" = True
isPreposition _ = False

addRule :: Node -> Rule -> Node
addRule (Node grammar rules next) newRule = Node grammar (newRule : rules) next

makeNode :: (String -> Bool) -> (String -> Grammar) -> [Rule] ->
            String -> [Node] -> [Node]
makeNode isType nodeType rules word next =
    if   isType word
    then [Node (nodeType word) rules next]
    else []

makeNoun :: String -> [Node] -> [Node]
makeNoun = makeNode isNoun Noun nounRules

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
