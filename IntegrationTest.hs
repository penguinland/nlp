{-# OPTIONS_GHC -Wall #-}

module Main where

import Analysis
import GrammarFilters
import Lexer
import Test.HUnit

isWellFormed :: String -> Bool
isWellFormed = (isSingleSentence `andAlso` (not . isAmbiguous)) . lexNodes

isAmbiguousSentence :: String -> Bool
isAmbiguousSentence = (isSingleSentence `andAlso` (isAmbiguous)) . lexNodes

testBasicSentence :: Test
testBasicSentence =
    TestCase $ assertBool "Basic sentence"
        (isWellFormed "my dogs found a yellow ball .")

testPrepositionalPhraseUnambig :: Test
testPrepositionalPhraseUnambig =
    TestCase $ assertBool "Unambiguous prepositional phrase"
        (isWellFormed "the dog in my yard eats a carrot .")

testPrepositionalPhraseAmbig :: Test
testPrepositionalPhraseAmbig =
    TestCase $ assertBool "Ambiguous prepositional phrase"
        (isAmbiguousSentence "the dog eats a carrot in my refrigerator .")

testConjugation1 :: Test
testConjugation1 =
    TestCase $ assertBool "Conjugation: third person singular"
        (isSingleSentence . lexNodes $ "my dog likes to run .")

testConjugation2 :: Test
testConjugation2 =
    TestCase $ assertBool "Conjugation: third person plural"
        (isSingleSentence . lexNodes $ "my dogs like to run .")

testConjugation3 :: Test
testConjugation3 =
    TestCase $ assertBool "Conjugation: incorrect third person singular"
        (not . isSingleSentence . lexNodes $ "my dog like to run .")

testConjugation4 :: Test
testConjugation4 =
    TestCase $ assertBool "Conjugation: incorrect third person plural"
        (not . isSingleSentence . lexNodes $ "my dogs likes to run .")

testInfinitive :: Test
testInfinitive =
    TestCase $ assertBool "Infinitive should be a valid object"
        -- Each conjugation of "run" (first person, second person, plural first
        -- person, plural second person, plural third person) is separately
        -- turned into an infinitive, so there are 5 "valid" parsings here.
        (isAmbiguousSentence "I love to run .")

main :: IO Counts
main = runTestTT $ TestList [ testBasicSentence
                            , testPrepositionalPhraseUnambig
                            , testPrepositionalPhraseAmbig
                            , testInfinitive
                            , testConjugation1
                            , testConjugation2
                            , testConjugation3
                            , testConjugation4
                            ]
