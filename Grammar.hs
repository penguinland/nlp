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
             | Preposition String-- PrepositionAttributes
             | Period
             | EOF
    deriving(Show, Eq)

data NounAttributes = NounAttributes { canBeSubject :: Bool
                                     , canBeObject :: Bool } deriving (Show, Eq)
testNoun :: (NounAttributes -> Bool) -> Grammar -> Bool
testNoun test (Noun _ attributes) = test attributes
testNoun test (NounPhrase _ noun) = testNoun test noun
testNoun test (ArticledNounPhrase _ nounphrase _) = testNoun test nounphrase
-- TODO: I don't think an infinitive can be a subject of a verb besides "to be."
-- revisit this later.
testNoun _ (Infinitive _ _) = True  -- Can be a subject or an object
testNoun _ grammar = error ("Tried testing non-noun grammar " ++ show grammar ++
                           "for noun-like properties")

{-
data PrepositionAttributes = { canFollowVerb :: Bool, canFollowNoun :: Bool }

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
