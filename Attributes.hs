{-# OPTIONS_GHC -Wall #-}

module Attributes where

-- OtherPerson is for verbs that aren't conjugated, like infinitives and
-- gerunds. AnyPerson is for verbs that work in any person, like past tense
-- verbs.
data Person =
    FirstPerson | SecondPerson | ThirdPerson | OtherPerson | AnyPerson
    deriving (Eq, Show)

data Plurality = Singular | Plural | EitherPlurality deriving (Eq, Show)

data NounAttributes = NounAttributes { canBeSubject :: Bool
                                     , canBeObject :: Bool
                                     , pluralN :: Plurality
                                     , personN :: Person } deriving (Eq)
instance Show NounAttributes where
    show _ = ""  -- Don't bother showing the attributes when showing a Grammar

-- TODO: should infinitives be on this list? What about gerunds?
data VerbTense = Present | Past deriving (Eq, Show)

data VerbAttributes = VerbAttributes { personV :: Person
                                     , pluralV :: Plurality
                                     , tense :: VerbTense } deriving (Eq)
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
