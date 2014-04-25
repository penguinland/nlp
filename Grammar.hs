{-# OPTIONS_GHC -Wall #-}

module Grammar where

import Attributes

type Rule = Node -> [Node]

-- Nodes need to store their list of rules because it becomes much easier to
-- find which rules apply to each node, rather than trying to look them up in
-- the middle of parsing (example: checking whether a verb is transitive or
-- not).
data Node = Node Grammar [Rule] [Node]

-- The EOF node is followed by an infinite continuation of EOF nodes.
-- Consequently, we can't use the default implementation of Show. Instead, we
-- define our own:
instance Show Node where
    show (Node EOF _ _) = "EOF"
    show (Node g _ s) = "Node " ++ show g ++ " " ++ show s

data Grammar =
            -- FullSentence Sentence Punctuation
               FullSentence Grammar Grammar
            -- Sentence Subject Predicate
             | Sentence Grammar Grammar
            -- Question (Maybe QuestionModifier) QuestionVerb Subject Predicate
             | Question (Maybe Grammar) Grammar Grammar Grammar
            -- Subject ArticledNounPhrase
             | Subject Grammar
            -- ArticledNounPhrase (Maybe Article)
            --                    NounPhrase
            --                    [PrepositionalPhrase]
            -- ArticledNounPhrase Nothing Infinitive [PrepositionalPhrase]
             | ArticledNounPhrase (Maybe Grammar) Grammar [Grammar]
            -- NounPhrase [Adjective] Noun
             | NounPhrase [Grammar] Grammar
            -- Predicate RawPredicate [PrepositionalPhrase]
             | Predicate Grammar [Grammar]
            -- RawPredicate Verb (Maybe DirectObject)
             | RawPredicate Grammar (Maybe Grammar)
            -- Infinitive "To" Predicate
             | Infinitive Grammar Grammar NounAttributes
            -- PrepositionalPhrase Preposition ArticledNounPhrase
             | PrepositionalPhrase Grammar Grammar
            -- ConjunctivePhrase [Phrase] Conjunction Phrase
            -- We will require that all Grammars stored in a conjunctive phrase
            -- have identical attributes, so we just store one copy.
             | ConjunctivePhrase [Grammar] Grammar Grammar ConjunctionAttributes
             | Article String
             | Noun String NounAttributes
             | Adjective String
             | Conjunction String
            -- VerbPhrase [VerbModifier] Verb
             | VerbPhrase [Grammar] Grammar VerbAttributes
             | VerbModifier String  -- examples: could, can, should, will
             | Verb String VerbAttributes
             | Preposition String PrepositionAttributes
             | QuestionModifier String -- examples: who, why, how
             | Period
             | QuestionMark
             | ExclamationPoint
             | EOF
    deriving(Show, Eq)
