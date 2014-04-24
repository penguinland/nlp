{-# OPTIONS_GHC -Wall #-}

module Main where

import Analysis
import GrammarFilters
import Lexer
import Test.HUnit

isAmbiguousSentence :: String -> Bool
isAmbiguousSentence = (isSingleSentence `andAlso` (isAmbiguous)) . lexNodes

testBasicSentence :: Test
testBasicSentence =
    TestCase $ assertBool "Basic sentence"
        (isWellFormed "my dogs found a yellow ball.")

testPrepositionalPhraseUnambig :: Test
testPrepositionalPhraseUnambig =
    TestCase $ assertBool "Unambiguous prepositional phrase"
        (isWellFormed "the dog in my yard eats a carrot.")

testPrepositionalPhraseAmbig :: Test
testPrepositionalPhraseAmbig =
    TestCase $ assertBool "Ambiguous prepositional phrase"
        (isAmbiguousSentence "the dog eats a carrot in my refrigerator.")

testInfinitive :: Test
testInfinitive =
    TestCase $ assertBool "Infinitive should be a valid, unambiguous object"
        (isWellFormed "I love to run.")

testConjugation1 :: Test
testConjugation1 =
    TestCase $ assertBool "Conjugation: third person singular"
        (isWellFormed "my dog likes to run.")

testConjugation2 :: Test
testConjugation2 =
    TestCase $ assertBool "Conjugation: third person plural"
        (isWellFormed "my dogs like to run.")

testConjugation3 :: Test
testConjugation3 =
    TestCase $ assertBool "Conjugation: incorrect third person singular"
        (not . isSingleSentence . lexNodes $ "my dog like to run.")

testConjugation4 :: Test
testConjugation4 =
    TestCase $ assertBool "Conjugation: incorrect third person plural"
        (not . isSingleSentence . lexNodes $ "my dogs likes to run.")

testPluralNouns :: Test
testPluralNouns =
    TestCase $ assertBool "Nouns ending in S have plurals with ES"
        (isWellFormed "bushes like classes.")

testVerbEsConjugation :: Test
testVerbEsConjugation =
    TestCase $ assertBool "Verbs ending in S have plurals with ES"
        (isWellFormed "he fusses." && isWellFormed "he catches the ball.")

testNounConjugation :: Test
testNounConjugation =
    TestCase $ assertBool "Nouns can be conjoined"
        (isSingleSentence . lexNodes $ "the dog and cat play in the yard.")

testWhen :: Test
testWhen =
    TestCase $ assertBool "Sentences can be conjoined"
        (isWellFormed "the dogs ran after the blue ball when I threw it.")

testPredicateConjunction :: Test
testPredicateConjunction =
    TestCase $ assertBool "Predicates can be conjoined"
        (isWellFormed "the dogs eat carrots and chew the blue ball.")

testPrepPhraseConjunction :: Test
testPrepPhraseConjunction =
    TestCase $ assertBool "Prepositional phrases can be conjoined"
        (isWellFormed "the dogs eat in the yard and with me.")

testAdjectiveConjunction :: Test
testAdjectiveConjunction =
    TestCase $ assertBool "Adjectives can be conjoined"
        (isWellFormed "I like the blue and yellow ball.")

testParagraph :: Test
testParagraph =
    TestCase $ assertBool "Larger texts can be parsed."
        (isText . lexNodes $
         "Zac and Sam went to the store because they wanted food. Zac " ++
         "wanted ham. Sam wanted chips and dip.")

main :: IO Counts
main = runTestTT $ TestList [ testBasicSentence
                            , testPrepositionalPhraseUnambig
                            , testPrepositionalPhraseAmbig
                            , testInfinitive
                            , testConjugation1
                            , testConjugation2
                            , testConjugation3
                            , testConjugation4
                            , testPluralNouns
                            , testVerbEsConjugation
                            , testNounConjugation
                            , testWhen
                            , testPredicateConjunction
                            , testPrepPhraseConjunction
                            , testAdjectiveConjunction
                            , testParagraph
                            ]
