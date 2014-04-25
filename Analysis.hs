{-# OPTIONS_GHC -Wall #-}

module Analysis where

import Grammar
import GrammarFilters
import Lexer
import Parser

-- This class lets us analyze strings of text or their Node representation using
-- the same functions.
class Analyzable a where
    toNodes :: [a] -> [Node]

instance Analyzable Char where
    toNodes = Lexer.lexNodes

instance Analyzable Node where
    toNodes = id

getGrammar :: Node -> Grammar
getGrammar (Node g _ _) = g

ruleCount :: Node -> Int
ruleCount (Node _ rules _) = length rules

getChildren :: Node -> [Node]
getChildren (Node _ _ children) = children

isWellFormed :: (Analyzable a) => [a] -> Bool
isWellFormed = (isSingleSentence `andAlso` (not . isAmbiguous)) . toNodes

isAmbiguous :: (Analyzable a) => [a] -> Bool
isAmbiguous =
  let
    isAmbiguous' [Node EOF _ _] = False
    isAmbiguous' (_ : _ : _) = True
    isAmbiguous' [Node (FullSentence _ _) _ children] =
        isAmbiguous' children
    isAmbiguous' _ = True
  in
    isAmbiguous' . extractSentences . toNodes

isSingleSentence :: (Analyzable a) => [a] -> Bool
isSingleSentence = isSingleSentence' . extractSentences . toNodes

isText :: (Analyzable a) => [a] -> Bool
isText =
  let
    isText' (Node EOF _ _) = True
    isText' (Node (FullSentence _ _) _ next) = isText next
    isText' _ = False
  in
    all isText' . extractSentences . toNodes

-- Note: it's okay if the sentence is ambiguous, as long as all possible parses
-- result in a single sentence.
isSingleSentence' :: (Analyzable a) => [a] -> Bool
isSingleSentence' =
  let
    nodeIsSingleSentence' (Node (FullSentence _ _) _ [Node EOF _ _]) = True
    nodeIsSingleSentence' _ = False
  in
    all nodeIsSingleSentence' . toNodes

-- I've gotten some example text appropriate for first grade reading levels from
-- http://www.superteacherworksheets.com/1st-comprehension.html

-- Text taken from the first grade reading comprehension worksheet at
-- http://www.superteacherworksheets.com/reading-comp/1st-ball-for-my-dog_TZZMD.pdf
text1 :: String
text1 = "my dog found a ball . it was a yellow ball . my dog loves to chew . he chewed the yellow ball . my dog found another ball . it was a red ball . my dog loves to play . he played with the red ball . my dog found another ball . it was a blue ball . my dog loves to run . he ran after the blue ball when I threw it ."
results1partial :: [Node]
results1partial = lexNodes text1
results1 :: [Node]
results1 = extractSentences results1partial

text1basic :: String
text1basic = "my dogs found a yellow ball ."
results1basic :: [Node]
results1basic = extractSentences $ lexNodes text1basic

text1half :: String
text1half = "he ran after the blue ball when I threw it ."
results1halfpartial :: [Grammar.Node]
results1halfpartial = lexNodes text1half
results1half :: [Node]
results1half = extractSentences results1halfpartial
