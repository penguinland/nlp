{-# OPTIONS_GHC -Wall #-}

module Verbs where

import qualified Data.Set

import Attributes
import Grammar
import LexerHelpers
import Rules

verbRules :: [Rule]
verbRules = intVerbRules ++ transVerbRules

normalIntransitiveVerbs :: Data.Set.Set String
normalIntransitiveVerbs = Data.Set.fromList ["live", "long", "ran", "run",
    "sleep", "went"]

normalTransitiveVerbs :: Data.Set.Set String
normalTransitiveVerbs = Data.Set.fromList ["build", "chew", "eat", "fit",
    "found", "help", "like", "love", "play", "threw", "want"]

-- Verbs where the proper conjugation of "it ___" is to add an "es" to the verb
withEsTransitiveVerbs :: Data.Set.Set String
withEsTransitiveVerbs = Data.Set.fromList ["brush", "catch", "boss"]

withEsIntransitiveVerbs :: Data.Set.Set String
withEsIntransitiveVerbs = Data.Set.fromList ["go", "fuss"]

sameConjugation :: [VerbAttributes]
sameConjugation = [ VerbAttributes FirstPerson  Singular Present
                  , VerbAttributes SecondPerson Singular Present
                  , VerbAttributes OtherPerson  Singular Present
                  , VerbAttributes FirstPerson  Plural   Present
                  , VerbAttributes SecondPerson Plural   Present
                  , VerbAttributes ThirdPerson  Plural   Present]

conjugateVerb ::
    Data.Set.Set String -> String -> String -> [Rule] -> [Node] -> [Node]
conjugateVerb list ending word rules next =
    if Data.Set.member word list
    then map (\a -> Node (Verb word a) rules next) sameConjugation
    else case getRootFrom ending word of
        Just root | Data.Set.member root list ->
            [Node (Verb word (VerbAttributes ThirdPerson Singular Present))
                 rules next]
        _ -> []

pastTenseVerbs :: Data.Set.Set String -> String -> [Rule] -> [Node] -> [Node]
pastTenseVerbs list word rules next =
    case getRootFrom "ed" word of
        Just root | Data.Set.member root list ->
            [Node (Verb word (VerbAttributes AnyPerson EitherPlurality Past))
                 rules next]
        Just root | Data.Set.member (root ++ "e") list ->
            [Node (Verb word (VerbAttributes AnyPerson EitherPlurality Past))
                 rules next]
        _ -> []

makeIntVerb :: String -> [Node] -> [Node]
makeIntVerb word next =
    pastTenseVerbs normalIntransitiveVerbs word intVerbRules next ++
    pastTenseVerbs withEsIntransitiveVerbs word intVerbRules next ++
    concatMap (\(s, e) -> conjugateVerb s e word intVerbRules next)
        [(normalIntransitiveVerbs, "s"), (withEsIntransitiveVerbs, "es")]

-- TODO: Can you think of a verb that *requires* a direct object?
makeTransVerb :: String -> [Node] -> [Node]
makeTransVerb word next =
    concatMap (\s -> pastTenseVerbs s word verbRules next)
        [normalTransitiveVerbs, withEsTransitiveVerbs] ++
    concatMap (\(s, e) -> conjugateVerb s e word verbRules next)
        [(normalTransitiveVerbs, "s"), (withEsTransitiveVerbs, "es")]

unusualVerbs :: Data.Set.Set String
unusualVerbs = Data.Set.fromList ["am", "are", "did", "do", "does", "had",
    "has", "have", "is", "was", "were"]

makeUnusualVerbs :: String -> [Node] -> [Node]
makeUnusualVerbs word next =
  let
    makeUnusualVerbs' "have" =
        map (\a -> Node (Verb "have" a) verbRules next) sameConjugation
    makeUnusualVerbs' "has" =
        [Node (Verb "has" (VerbAttributes ThirdPerson Singular Present))
             verbRules next]
    makeUnusualVerbs' "had" =
        [Node (Verb "had" (VerbAttributes AnyPerson EitherPlurality Past))
             verbRules next]
    makeUnusualVerbs' "do" =
        map (\a -> Node (Verb "do" a) (questionFromVerb : verbRules) next)
                       sameConjugation
    makeUnusualVerbs' "does" =
        [Node (Verb "does" (VerbAttributes ThirdPerson Singular Present))
             (questionFromVerb : verbRules) next]
    makeUnusualVerbs' "did" =
        [Node (Verb "did" (VerbAttributes AnyPerson EitherPlurality Past))
             (questionFromVerb : verbRules) next]
    makeUnusualVerbs' "am" =
        [Node (Verb "am" (VerbAttributes FirstPerson Singular Present))
             verbRules next]
    makeUnusualVerbs' "are" =
        map (\(person, plural) -> Node (Verb "are"
                                      (VerbAttributes person plural Present))
                                      verbRules next)
            [(SecondPerson, Singular), (FirstPerson, Plural),
             (SecondPerson, Plural), (ThirdPerson, Plural)]
    makeUnusualVerbs' "is" =
        [Node (Verb "is" (VerbAttributes ThirdPerson Singular Present))
             verbRules next]
    makeUnusualVerbs' "was" =
        [Node (Verb "was" (VerbAttributes FirstPerson Singular Past))
             verbRules next,
         Node (Verb "was" (VerbAttributes ThirdPerson Singular Past))
             verbRules next]
    makeUnusualVerbs' "were" =
        map (\(person, plural) -> Node (Verb "were"
                                      (VerbAttributes person plural Past))
                                      verbRules next)
            [(SecondPerson, Singular), (FirstPerson, Plural),
             (SecondPerson, Plural), (ThirdPerson, Plural)]
    makeUnusualVerbs' _ = error
        "Unexpected non-unusual verb in makeUnusualVerbs'!"
  in
    if Data.Set.member word unusualVerbs
    then makeUnusualVerbs' word
    else []
