# Harkdown

[CommonMark]: http://commonmark.org/

**A [CommonMark] compliant Markdown parser written in Haskell using Parsec.**

## Team members
Palmer Paul ([palmerpa@seas][palmer email])
Jeremy Lautman ([jlautman@seas](mailto:jlautman@seas.upenn.edu))

## Goals
1) Create a pure Haskell functional Markdown parser based on the CommonMark standard
2) Contribute back to the CommonMark standard
3) Learn how to perform advanced parsing and design applications in Haskell

## Most significant challenge
The CommonMark standard is immense, and is intended to be writer friendly rather
than parser friendly. As such, parsing all elements requires handling a large
number of ambiguous edge cases. In addition, control sequence patterns in the
text were often defined including lookbehind, which is not supported by the
Parsec parser. To honor those requirements we had to design in a substantial
amount of additional lookahead and parser parameterization.

## Most impressive aspect of what was built
Markdown allows a great deal of flexibility to the writer related to how to form lists. This includes allowing the writer to mark their lists with arbitrary list ordinals, and expecting that the parser will replace the ordinals with the correct ones. This required a highly sophisticated list parser that would be able to change its behavior based on the list markers detected.

## Most interesting thing learned
We learned a lot about parsing complicated grammars. Over the course of the project, we developed numerous methods for parsing, such as using interrupt markers, looking ahead, faking looking behind, and more. Another interesting lesson that we learned is that real world specs are sometimes thornier than they appear to be. In this case, the unnecessary complexity arises from the fact that the spec was written to match existing implementations and that CommonMark tries to accommodate nearly all existing Markdown implementations.

## What resources were used in the project
* Parsec - Monadic parser with support for parsing arbitrary token streams
* HUnit
* QuickCheck
* Pretty
* CommonMark Spec - An industry attempt at a harmonized standard between the
various existing Markdown parsers.
* Aeson - JSON parsing support to load the supplied CommonMark test cases
* Cabal - build and dependency management system.


## What grade do we deserve and why?
We deserve an A, as we achieved our refined goal of correctly parsing a
workable subset of the Markdown specification. In addition, the project
required extensive design work around a spec that is very ambiguous, internally
inconsistent, and actively under development. The spec includes many alternative
syntaxes, resulting in over 100 pages of details written in a very technical
manner, requiring a lot of very close reading and research on our parts. In
fact, we found some flaws both in the spec and reference implementation, which
we reported on the CommonMark forum (e.g. <https://talk.commonmark.org/t/parentheses-in-link-destination-of-link-reference-definition/2667>).

## Other comments
It took several iterations of the parsing model to reach the current level of
features, as previous designs proved insufficient to handle the discovered
complexity of Markdown parsing, which can be observed in the git log.

[palmer email]: mailto:palmerpa@seas.upenn.edu
