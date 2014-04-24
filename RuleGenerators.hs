{-# OPTIONS_GHC -Wall #-}

module RuleGenerators where

import Grammar

-- Throw away arguments, give back True.
constTrue2 :: a -> b -> Bool
constTrue2 = (const . const $ True)
constTrue3 :: a -> b -> c -> Bool
constTrue3 = (const . const . const $ True)

makeRule1 :: (Grammar -> Bool) -> (Grammar -> Grammar) -> [Rule] -> Rule
makeRule1 isCorrectGrammar toNewGrammar nextRules =
  let
    newRule (Node g _ next)
      | isCorrectGrammar g = [Node (toNewGrammar g) nextRules next]
    newRule _ = []
  in
    newRule

makeRule2 :: (Grammar -> Bool) -> (Grammar -> Bool) ->
             (Grammar -> Grammar -> Bool) ->
             (Grammar -> Grammar -> Grammar) -> [Rule] -> Rule
makeRule2 isCorrectGrammar1 isCorrectGrammar2 grammarsAreCompatible
        toNewGrammar nextRules =
  let
    toNewNode first second next =
        Node (toNewGrammar first second) nextRules next
    newRule (Node first _ seconds) =
        [toNewNode first second next | isCorrectGrammar1 first,
         (Node second _ next) <- seconds, isCorrectGrammar2 second,
         grammarsAreCompatible first second]
  in
    newRule

makeRule3 :: (Grammar -> Bool) -> (Grammar -> Bool) -> (Grammar -> Bool) ->
             (Grammar -> Grammar -> Grammar -> Bool) ->
             (Grammar -> Grammar -> Grammar -> Grammar) -> [Rule] -> Rule
makeRule3 isCorrectGrammar1 isCorrectGrammar2 isCorrectGrammar3
        grammarsAreCompatible toNewGrammar nextRules =
  let
    toNewNode first second third next =
        Node (toNewGrammar first second third) nextRules next
    newRule (Node first _ seconds) =
        [toNewNode first second third next | isCorrectGrammar1 first,
         (Node second _ thirds) <- seconds, isCorrectGrammar2 second,
         (Node third _ next) <- thirds, isCorrectGrammar3 third,
         grammarsAreCompatible first second third]
  in
    newRule
