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

makeRule2ArbitraryRules :: (Node -> Node -> [Rule]) ->
                           (Grammar -> Bool) -> (Grammar -> Bool) ->
                           (Grammar -> Grammar -> Bool) ->
                           (Grammar -> Grammar -> Grammar) -> Rule
makeRule2ArbitraryRules makeNewRules isCorrectGrammar1 isCorrectGrammar2
        grammarsAreCompatible toNewGrammar =
  let
    toNewNode first second = Node (toNewGrammar first second)
    newRule firstNode@(Node first _ seconds) =
        [toNewNode first second (makeNewRules firstNode secondNode) next |
         isCorrectGrammar1 first,
         secondNode@(Node second _ next) <- seconds,
         isCorrectGrammar2 second,
         grammarsAreCompatible first second]
  in
    newRule

-- The Rules for the new Node are the rules for the last Node merged.
makeRule2AdoptingRules :: (Grammar -> Bool) -> (Grammar -> Bool) ->
             (Grammar -> Grammar -> Bool) ->
             (Grammar -> Grammar -> Grammar) -> Rule
makeRule2AdoptingRules =
    makeRule2ArbitraryRules (\_ (Node _ rules _) -> rules)

-- The rules for the new node are given as a static list
makeRule2 :: (Grammar -> Bool) -> (Grammar -> Bool) ->
             (Grammar -> Grammar -> Bool) ->
             (Grammar -> Grammar -> Grammar) -> [Rule] -> Rule
makeRule2 isCorrectGrammar1 isCorrectGrammar2 grammarsAreCompatible
        toNewGrammar nextRules =
    makeRule2ArbitraryRules (const . const $ nextRules) isCorrectGrammar1
        isCorrectGrammar2 grammarsAreCompatible toNewGrammar

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
