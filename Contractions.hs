{-# OPTIONS_GHC -Wall #-}

module Contractions where

import qualified Data.Set
import Grammar
import LexerHelpers

-- TODO: remove this when I'm confident it isn't going to be used.
commonContractions :: Data.Set.Set String
commonContractions = Data.Set.fromList ["I'm", "you're", "he's", "she's",
    "it's", "we're", "they're", "I've", "you've", "he'd", "she'd", "we'd",
    "they'd", "you'd", "that's", "would've", "should've", "could've", "don't",
    "can't", "shouldn't", "couldn't", "wouldn't", "aren't", "isn't", "hasn't",
    "hadn't", "didn't", "won't"]

replaceWord :: [Node] -> String -> [Node]
replaceWord nodes newWord =
  let
    replaceWordInGrammar (Verb _ attributes) = Verb newWord attributes
    replaceWordInGrammar (VerbModifier _) = VerbModifier newWord
    replaceWordInGrammar grammar =
        error $ "Tried to replace word in unexpected Grammar" ++ show grammar
    replaceWordInNode (Node grammar rules next) =
        Node (replaceWordInGrammar grammar) rules next
  in
    map replaceWordInNode nodes

-- We pass in the parser and lexer as the first two arguments so that we can
-- parse/lex pieces of the contraction that get created outside the main
-- tokenizer.
makeSpecificContraction :: ([Node] -> [Node]) ->
         (String -> [Node] -> [Node]) -> String -> String ->
         String -> [Node] -> [Node]
makeSpecificContraction parser lexer word contraction uncontraction next =
  case getRootFrom contraction word of
    Just root ->
      let
        -- The variable names suggest we're doing "n't" -> "not", but this
        -- function is more general than that. They're just there as an
        -- example.
        notNode = lexer uncontraction next
        n'tNode = parser $ replaceWord notNode contraction
      in
        lexer root n'tNode
    Nothing -> []

makeContraction :: ([Node] -> [Node]) -> (String -> [Node] -> [Node]) ->
        String -> [Node] -> [Node]
makeContraction parser lexer word next
  -- | Data.Set.member word commonContractions =
  | '\'' `elem` word =
      let
        tryContraction contraction uncontraction =
            makeSpecificContraction
                parser lexer word contraction uncontraction next
        makeContraction' "I'm" =
            makeSpecificContraction parser lexer "I'm" "'m" "am" next
        -- We need to special-case "can't" because the 'n' does double duty in
        -- the "can" and the "n't" here.
        makeContraction' "can't" =
            parser $ replaceWord (lexer "cannot" next) "can't"
        makeContraction' "won't" =
            replaceWord
                (makeSpecificContraction parser lexer "willn't" "n't" "not"
                     next)
                "won't"
        makeContraction' "shan't" =
            replaceWord
                (makeSpecificContraction parser lexer "shalln't" "n't" "not"
                     next)
                "shan't"
        makeContraction' _ =  -- It's the same as word, above
            concatMap (uncurry tryContraction) [ ("n't", "not")
                                               , ("'ve", "have")
                                               , ("'d", "had")
                                               , ("'d", "would")
                                               , ("'re", "are")
                                               , ("'s", "is")
                                               , ("'s", "has")
                                               ]
      in
        makeContraction' word
  | otherwise = []
