{-# OPTIONS_GHC -Wall #-}

module Nouns where

--import Control.Monad

import qualified Data.Set
import Grammar
import LexerHelpers
import Rules

-- Nouns whose plural appends an "s"
normalNouns :: Data.Set.Set String
normalNouns = Data.Set.fromList ["ball", "carrot", "cat", "chips", "dip", "dog",
    "food", "ham", "lace", "mile", "plate", "refrigerator", "sink", "snack",
    "store", "yard"]

-- Nouns whose plural appends an "es"
pluralEsNouns :: Data.Set.Set String
pluralEsNouns = Data.Set.fromList ["bush", "class"]

-- Proper Nouns
properNouns :: Data.Set.Set String
properNouns = Data.Set.fromList ["Sam", "Zac"]

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

makePronouns :: String -> [Node] -> [Node]
makePronouns word next =
  let
    pronouns = Data.Set.fromList ["I", "me", "you", "he", "she", "it", "him",
                                  "her", "we", "us", "they", "them"]
    makePronounNode subject object plural person =
        [Node (Noun word (NounAttributes
         { canBeSubject = subject, canBeObject = object,
           isPluralN = plural, personN = person })) nounRules next]
    makePronouns' "I"    = makePronounNode True  False False First
    makePronouns' "me"   = makePronounNode False True  False First
    makePronouns' "you"  = makePronounNode True  True  False Second ++
                           makePronounNode True  True  True  Second
    makePronouns' "he"   = makePronounNode True  False False Third
    makePronouns' "she"  = makePronounNode True  False False Third
    makePronouns' "it"   = makePronounNode True  True  False Third
    makePronouns' "him"  = makePronounNode False True  False Third
    makePronouns' "her"  = makePronounNode False True  False Third
    makePronouns' "we"   = makePronounNode True  False True  First
    makePronouns' "us"   = makePronounNode False True  True  First
    makePronouns' "they" = makePronounNode True  False True  Third
    makePronouns' "them" = makePronounNode False True  True  Third
    makePronouns' _      = error ("Non-pronoun made it into makepronouns'")
  in
    if Data.Set.member word pronouns
    then makePronouns' word
    else []

makeProperNoun :: String -> [Node] -> [Node]
makeProperNoun word next =
    if Data.Set.member word properNouns
    then [Node (Noun word singularNounAttributes) nounRules next]
    else []

makeNoun :: String -> [Node] -> [Node]
makeNoun word next =
    makePronouns word next ++
    makeProperNoun word next ++
    concatMap (\(set, plural) -> makeNounCase set plural word next)
        [(normalNouns, "s"), (pluralEsNouns, "es")]


