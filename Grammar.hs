{-# OPTIONS_GHC -Wall #-}

module Grammar where

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
            -- FullSentence Sentence
               FullSentence Grammar
            -- Sentence Subject Predicate
             | Sentence Grammar Grammar
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
            -- have identical attributes, except for ArticledNounPhrases, which
            -- will have their attributes merged, made plural, and wrapped
            -- around the conjunctive phrase.
             | ConjunctivePhrase [Grammar] Grammar Grammar ConjunctionAttributes
             | Article String
             | Noun String NounAttributes
             | Adjective String
             | Conjunction String
             | Verb String VerbAttributes
             | Preposition String PrepositionAttributes
             | Period
             | EOF
    deriving(Show, Eq)

-- Other is for verbs that aren't conjugated, like infinitives and gerunds.
data Person = First | Second | Third | Other deriving (Eq, Show)

data NounAttributes = NounAttributes { canBeSubject :: Bool
                                     , canBeObject :: Bool
                                     , isPluralN :: Bool
                                     , personN :: Person } deriving (Eq)
instance Show NounAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar

data VerbAttributes = VerbAttributes { personV :: Person
                                     , isPluralV :: Bool } deriving (Eq)
instance Show VerbAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar

data PrepositionAttributes = PrepositionAttributes { canModifyVerb :: Bool
                                                   , canModifyNoun :: Bool
                                                   } deriving (Eq)
instance Show PrepositionAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar

data ConjunctionAttributes = NounConjunction NounAttributes
                           | VerbConjunction VerbAttributes
                           | PrepConjunction PrepositionAttributes
                           | OtherConjunction deriving (Eq)
instance Show ConjunctionAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar
