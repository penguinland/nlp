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
             | Infinitive Grammar Grammar
            -- PrepositionalPhrase Preposition ArticledNounPhrase
             | PrepositionalPhrase Grammar Grammar
             | Article String
             | Noun String NounAttributes
             | Adjective String
             | Verb String
             | Preposition String PrepositionAttributes
             | Period
             | EOF
    deriving(Show, Eq)

class Attributes a where
    checkAttrs :: (a -> Bool) -> (Grammar -> Bool)

data NounAttributes = NounAttributes { canBeSubject :: Bool
                                     , canBeObject :: Bool } deriving (Eq)
instance Show NounAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar

data PrepositionAttributes = PrepositionAttributes { canFollowVerb :: Bool
                                                   , canFollowNoun :: Bool
                                                   , canContainSentence :: Bool
                                                   , canContainPredicate :: Bool
                                                   } deriving (Eq)
instance Show PrepositionAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar

{-
prepositionFollowsVerb :: Node -> Bool
prepositionFollowsVerb
        (Node (PrepositionalPhrase (Preposition _ attributes) _)) =
    canFollowVerb attributes
prepositionFollowsVerb _ = False

prepositionFollowsNoun :: Node -> Bool
prepositionFollowsNoun
        (Node (PrepositionalPhrase (Preposition _ attributes) _)) =
    canFollowNoun attributes
prepositionFollowsNoun _ = False
-}
