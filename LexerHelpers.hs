{-# OPTIONS_GHC -Wall #-}

module LexerHelpers where

import qualified Data.Set

import Grammar

-- If word ends with ending, give back Just the word with the ending stripped
-- off. If word doesn't end with ending, give back Nothing.
getRootFrom :: String -> String -> Maybe String
getRootFrom ending word =
  let
    rEnding = reverse ending
    rWord = reverse word
    getRootFrom' [] root = Just root
    getRootFrom' _ [] = Nothing
    getRootFrom' (e:es) (w:ws)
      | e == w = getRootFrom' es ws
      | otherwise = Nothing
  in
    getRootFrom' rEnding rWord >>= (return . reverse)

addRule :: Node -> Rule -> Node
addRule (Node grammar rules next) newRule = Node grammar (newRule : rules) next

makeNode :: Data.Set.Set String -> (String -> Grammar) -> [Rule] ->
            String -> [Node] -> [Node]
makeNode wordSet nodeType rules word next =
    if   Data.Set.member word wordSet
    then [Node (nodeType word) rules next]
    else []
