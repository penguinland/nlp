{-# OPTIONS_GHC -Wall #-}

module Nouns where

import qualified Data.Set
import Grammar
import LexerHelpers
import Rules

-- Nouns whose plural appends an "s"
normalNouns :: Data.Set.Set String
normalNouns = Data.Set.fromList ["ball", "carrot", "cat", "dip", "dog", "lace",
    "plate", "refrigerator", "sink", "snack", "yard"]

-- Nouns whose plural appends an "es"
pluralEsNouns :: Data.Set.Set String
pluralEsNouns = Data.Set.fromList ["bush", "class"]

singularNounAttributes :: NounAttributes
singularNounAttributes = NounAttributes True True False Third
pluralNounAttributes :: NounAttributes
pluralNounAttributes = NounAttributes True True True Third

makeNounCase :: Data.Set.Set String -> String -> String -> [Node] -> [Node]
makeNounCase list plural word next =
    if Data.Set.member word list
    then [Node (Noun word singularNounAttributes) nounRules next]
    else case getRootFrom plural word of
        Just root | Data.Set.member root list ->
            [Node (Noun root pluralNounAttributes) nounRules next]
        _ -> []
