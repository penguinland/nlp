NLP - A parser for English text
===

The intention here is to get a way of parsing English text. I'd like to be able
to pass things I've written through this to make sure there aren't any grammar
problems, and I'd like to be able to pass text intended for a TTS engine through
here to disambiguate homographs (examples: how to pronounce "wind" or "lives" or
"read" or "tears").

I also hope to get to a point where it can learn new words without my explicitly
entering them, but that's still a ways off.

The overall plan is to have text stored as a DAG: each node contains some piece
of text (currently ranging from a word at the smallest to a sentence at the
largest), pointing to all possible subsequent nodes. Each node has some
associated rules about how it can merge with subsequent pieces to become a
larger piece. For instance, nodes that represent adjectives have a rule that if
they are followed by a noun node they can be merged together into a noun that
encapsulates both of them. However, this merging adds a node to the DAG, without
removing the old parts. This lets us have ambiguous grammars, unlike traditional
CFGs and their parsers. This also allows us to have ambiguous words stored as
multiple nodes, with the expectation that only one of them will eventually be
merged into a sentence (for instance, "fish" might be a noun or a verb, but
after we're done parsing, I expect that only one of the two will have turned
into an entire sentence).
