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
singularNounAttributes = NounAttributes True True False ThirdPerson
pluralNounAttributes :: NounAttributes
pluralNounAttributes = NounAttributes True True True ThirdPerson

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
    makePronouns' "I"    = makePronounNode True  False False FirstPerson
    makePronouns' "me"   = makePronounNode False True  False FirstPerson
    makePronouns' "you"  = makePronounNode True  True  False SecondPerson ++
                           makePronounNode True  True  True  SecondPerson
    makePronouns' "he"   = makePronounNode True  False False ThirdPerson
    makePronouns' "she"  = makePronounNode True  False False ThirdPerson
    makePronouns' "it"   = makePronounNode True  True  False ThirdPerson
    makePronouns' "him"  = makePronounNode False True  False ThirdPerson
    makePronouns' "her"  = makePronounNode False True  False ThirdPerson
    makePronouns' "we"   = makePronounNode True  False True  FirstPerson
    makePronouns' "us"   = makePronounNode False True  True  FirstPerson
    makePronouns' "they" = makePronounNode True  False True  ThirdPerson
    makePronouns' "them" = makePronounNode False True  True  ThirdPerson
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
