{-# OPTIONS_GHC -Wall #-}

module Nouns where

import qualified Data.Set
import Attributes
import Grammar
import LexerHelpers
import Rules

-- Nouns whose plural appends an "s"
normalNouns :: Data.Set.Set String
normalNouns = Data.Set.fromList ["ball", "carrot", "cat", "chips", "dip", "dog",
    "food", "ham", "lace", "mile", "plate", "refrigerator", "sink", "snack",
    "snowman", "store", "yard"]

-- Nouns whose plural appends an "es"
pluralEsNouns :: Data.Set.Set String
pluralEsNouns = Data.Set.fromList ["bush", "class"]

-- Nouns whose plural changes a "y" to an "ies"
pluralIesNouns :: Data.Set.Set String
pluralIesNouns = Data.Set.fromList ["baby", "city"]

-- Proper Nouns
properNouns :: Data.Set.Set String
properNouns = Data.Set.fromList ["Sam", "Zac"]

singularNounAttributes :: NounAttributes
singularNounAttributes = NounAttributes True True Singular ThirdPerson
pluralNounAttributes :: NounAttributes
pluralNounAttributes = NounAttributes True True Plural ThirdPerson

makeNounCase :: Data.Set.Set String -> String -> String -> String -> [Node] ->
                [Node]
makeNounCase list plural singular word next =
    if Data.Set.member word list
    then [Node (Noun word singularNounAttributes) nounRules next]
    else case getRootFrom plural word of
        Just root | Data.Set.member (root ++ singular) list ->
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
           pluralN = plural, personN = person })) nounRules next]
    makePronouns' "I"    = makePronounNode True  False Singular FirstPerson
    makePronouns' "me"   = makePronounNode False True  Singular FirstPerson
    makePronouns' "you"  = makePronounNode True  True  Singular SecondPerson ++
                           makePronounNode True  True  Plural   SecondPerson
    makePronouns' "he"   = makePronounNode True  False Singular ThirdPerson
    makePronouns' "she"  = makePronounNode True  False Singular ThirdPerson
    makePronouns' "it"   = makePronounNode True  True  Singular ThirdPerson
    makePronouns' "him"  = makePronounNode False True  Singular ThirdPerson
    makePronouns' "her"  = makePronounNode False True  Singular ThirdPerson
    makePronouns' "we"   = makePronounNode True  False Plural   FirstPerson
    makePronouns' "us"   = makePronounNode False True  Plural   FirstPerson
    makePronouns' "they" = makePronounNode True  False Plural   ThirdPerson
    makePronouns' "them" = makePronounNode False True  Plural   ThirdPerson
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
    concatMap (\(set, plural, singular) ->
               makeNounCase set plural singular word next)
        [(normalNouns, "s", ""), (pluralEsNouns, "es", ""),
         (pluralIesNouns, "ies", "y")]
