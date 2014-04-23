{-# OPTIONS_GHC -Wall #-}

module Verbs where

import qualified Data.Set

import Grammar
import LexerHelpers
import Rules

normalIntransitiveVerbs :: Data.Set.Set String
normalIntransitiveVerbs = Data.Set.fromList ["ran", "run", "sleep", "went"]

normalTransitiveVerbs :: Data.Set.Set String
normalTransitiveVerbs = Data.Set.fromList ["chew", "chewed", "eat", "found",
    "help", "like", "love", "play", "played", "threw", "wanted", "was"]

-- Verbs where the proper conjugation of "it ___" is to add an "es" to the verb
withEsTransitiveVerbs :: Data.Set.Set String
withEsTransitiveVerbs = Data.Set.fromList ["brush", "catch", "boss"]

withEsIntransitiveVerbs :: Data.Set.Set String
withEsIntransitiveVerbs = Data.Set.fromList ["fuss"]

sameConjugation :: [VerbAttributes]
sameConjugation = [ VerbAttributes First  False Present
                  , VerbAttributes Second False Present
                  , VerbAttributes Other  False Present
                  , VerbAttributes First  True  Present
                  , VerbAttributes Second True  Present
                  , VerbAttributes Third  True  Present]

conjugateVerb ::
    Data.Set.Set String -> String -> String -> [Rule] -> [Node] -> [Node]
conjugateVerb list ending word rules next =
    if Data.Set.member word list
    then map (\a -> Node (Verb word a) rules next) sameConjugation
    else case getRootFrom ending word of
        Just root | Data.Set.member root list ->
            [Node (Verb word (VerbAttributes Third False Present)) rules next]
        _ -> []

makeIntVerb :: String -> [Node] -> [Node]
makeIntVerb word next =
    concatMap (\(s, e) -> conjugateVerb s e word intVerbRules next)
        [(normalIntransitiveVerbs, "s"), (withEsIntransitiveVerbs, "es")]

-- TODO: Can you think of a verb that *requires* a direct object?
makeTransVerb :: String -> [Node] -> [Node]
makeTransVerb word next =
  let
    verbRules = intVerbRules ++ transVerbRules
  in
    concatMap (\(s, e) -> conjugateVerb s e word verbRules next)
        [(normalTransitiveVerbs, "s"), (withEsTransitiveVerbs, "es")]
