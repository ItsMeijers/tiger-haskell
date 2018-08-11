# Notes on Chapter 2 - Lexical Analysis

The front end of the compiler performs analysis; the back end does synthesis.

The analysis is usually broken up into:

* __Lexical analysis__: breaking th einput into individual words or `tokens`;
* __Syntax analysis__: parsing the phrase structure of the program; and
* __Semantic analysis__: calculating the program's meaning.

A lexical token is a sequence of characters that can be treated as a unit in the
grammar of a programming language. A programming language classifies lexical tokens
into a finite set of token types.

Punctuation tokens such as IF, VOID, RETURN constructed from alphabetic characters
are called _reserved words_ and, in most languages, cannot be used as identifiers.

Some of the tokens, such as identifiers and literals, have semantic values attached
to them, giving auxiliary information in addition to the token type.

## Regular Expressions

Let us say that a _language_ is a set of strings; a string is a finite sequence of _symbols_.
The symbols themselves are taken from a finite alphabet.

|Expression Name            | Instruction | Meaning                                                                                     |
|---------------------------|-------------|---------------------------------------------------------------------------------------------|
|Symbol                     | `a`         | The regex __a__ denotes the langauge containing the string a                                |
|Alternation                | `a | b`     | The language of `a | b` constains the two strings a and b                                   |
|Concatenation              | `M · N`     | Concatenates two languages together                                                         |
|Epsilon (empty)            | `ε`         | The regex ε represents a lanague whoes only string is the empty string                      |
|Repetition (Kleen closure) | `M*`        | A string is in M* if it is the concatenation of zero or more strings, all of which are in M |

### Abbreviations

| Abbreviation | Full                                                   |
|--------------|--------------------------------------------------------|
| `ab | c`     | `(a · b ) | c`                                         |
| `(a |)`      | `(a | ε)`                                              |
| `[abcd]`     | `(a | b | c | d)`                                      |
| `[b-g]`      | `[bcdefg]`                                             |
| `[b-gM-Qkr]` | `[bcdefgMNOPQkr]`                                      |
| `M?`         | `(M | ε)`                                              |
| `M+`         | `(M · M*)`                                             |
| `.`          | Any single character except newline                    |
| `"a._*"`     | Quation, a string in quotes stand for itself literally |

Using this langauge, we can specify the lexical tokens of a programming language.

Two important disambiguration rules (used by Lex like lexical-analyzer generators):

* __Longest match__: The longest initial substring of the input that can match any regular expression is taken as the next token.
* __Rule priority__: For a _particular_ longest initial substring, the first regular expression that can match determines its token types. This
means that the order of writing down the regular-expression rules has significance.

## Finite Automata

A finite automaton has a finite set of _states_; _edges_ lead from one state to another, and each edge is labeled with a _symbol_.
One state is the _start_ state, and certain of the states are distinguished as _final_ states.

In a _deterministic_ finite automaton (DFA), no two edges leaving from the same state are labeled with the same symbol. The _language_
recognized by an automaton is the set of strings that it accepts. Each final state must be labeled with the token-type that it accepts.
If in a situtation where both states are final, _rule priority_ is used to disambiguate the label state, for instance because the token
needs to be recognized as a reserved word, not an identifier. We can encode a machine as a transition matrix: a two-dimensional array, ]
subscripted by state number and input character. There will be a "dead" state (state 0) that loops to itself on all characters; we use
this to encode the absence of an edge. There must also be a "finality" array, mapping state numbers to actions.

Keeping track of the longest match just means remembering the last time the automaton was in a final state with two variables, `Last-Final`
(the state number of the most recent final state encountered) and `Input-Position-at-Last-Final`. Every time a final state is entered, the
lexer updates these variables; when a dead state (a nonfinal state with no output transitions) is reached, the variables tell what token was
matched, and where it ended.

A _nondeterministic_ finite automaton (NFA) is one that has a choice of edges - labeled with the same symbol - to follow out of a state. It
may have special edges labeled with ε, that can be followeed without eating any symbol from the input. Nondeterministic automata are a useful
notion because it is easy to convert a (static, declarative) regular expression to a (simulatable, quasi-executable) NFA. The conversion
algorithm turns each regular expression into an NFA with a tail (start edge) and a head (ending state).

### Translation of regular Expressions to NFAs

We can define the translation of regular expressinos to NFAs by induction. Either an expression is primitive (a single symbol or ε) or it is
made from smaller expressions. Similarly, the NFA will be primitive or made from smaller NFAs. Each expression is translated to an NFA, the
"head" state of each NFA is marked final with a different token type, and the tails of all the expressions are joined to a new start node.

```text
a           --a--> ( )


ε           --ε--> ( )


                        |--M--> ( ) --ε--|
M | N       --ε--> ( ) -|                -->()
                        |--N--> ( ) --ε--|


M · N       --M--> ( ) --N--> ( )


M*          --------ε----------|
                               ---> ( )
                    |-> ( )--ε-|     |
                    |                |
                    ---M--------------


M+          constructed as M · M*


M?          constructed as M | ε


                    -----a---
                    |       |
[abc]       --ε--> ( )---b-----> ( )
                    |       |
                    ----c----


"abc"       constructed as a · b · c
```

### Converting an NFA to a DFA

Implementing NFAs is a bit harder than DFAs, since most computers don't have good "guessing" hardware. We can avoid the need to guess by
trying every possibility at once. We do this by computing the ε-closure of each possible state transition.

We formally define ε-closure as follows. let __edge__(s, c) be the set of all NFA states reachable by following a single edge with label
c from state s. For a set of states S, __closure__(S) is the set of states that can be reached from a state in S without consuming any of
the input, that is, by going only through ε edges. Mathematically, we can express the idea of going through ε edges by saying that
__closure__(S) is smallest set T such that

```text
T = S∪ (∪ s∈T -> edge(s, ε) )
           
```

We can calculate T by iteration:

```text
T <- S
repeat T' <- T
    T <- T'∪(∪ s∈T' -> edge(s, ε))
until T = T'
```

Now when simulating an NFA as described above, suppose we are in a set d = {si, sk, sl} of NFA states si, sk, sl. By starting d and eating the
input symbol c, we reach a new set of NFA states; we'll call this set __DFAedge(d,c):

```text
DFAedge(d, c) = closure(∪ s∈d -> edge(s,c))
```

Using __DFAedge__, we can write the NFA simulation algorithm more formally. If the start state of the NFA is s1, and the input string is
C1,.......,Ck, then the algorithm is:

```text
d <- closure({s1})
for i <- 1 to k
  d <- DFAedge(d, ci)
```

Manipulating sets of staes is expensive -- too costly to want to do on every character in the source program that is being lexically analyzed.
But it is possible to do all the sets-of-states calculations in advance. We make a DFA from the nFA, such thast each set of NFA states corresponds
to one DFA state. Since the NFA has a finite number n of states, the DFA will also have a finite number (at most 2n) of states. The DFA start state d1
is just __closure__(s1), as in the nFA simulation algorithm. Abstractly, there is an edge from di to dj labeled with c if dj = __DFAedge__(di,c).
We let Σ be the alphabet.

```text
states[0] <- {};   states[1] <- closure({s1})
p <- 1;      j <- 0
while j <= p
  foreach c∈Σ
    e <- DFAedge(states[j], c)
    if e == states[i] for some i <= p
      then trans[j, c] <- i
      else p <- p + 1
           states[p]  <- e
           trans[j,c] <- p
  j <- j + 1
```

The algorithm does not visit unreachable states of the DFA. This is extremely important, because in principle the DFA has 2n states, but in practice we
usually find that only about n of them are reachable from the start state. A state d is final in the DFA if any NFA-state in states[d] is final in the
NFA. Labeling a state final is not enough; we must also say what token is recognized; and perhaps serveral members of states[d] are final in the NFA.
In this case we label d with the token-type that occured first in the list of regular expressions that constitute the lexical specification. This is how
rule priority is implemented.

After constructing a DFA it is useful to apply an algorithm to minimize it by finding equivalent states. In general, we say that two states
s1 and s2 are equivalent when the machine starting in s1 accepts a string σ if and only if starting in s2 it accepts σ.

## Lexical Analyser generation in Haskell with Alex

Alex is a tool for generating lexical analysers in Haskell, given a description of the tokens to be recognised in the form of regular expressions. It is
similar to the tools lex and flex for C/C++. Alex takes a description of tokens based on regular expressions and generates a Haskell module containing
code for scanning text efficiently.

Alex specification layout (Tokens.x -> produces Tokens.hs):

```haskell
{
  module Main (main) where -- Code scrap to be placed directly in the output
}

%wrapper "basic" -- Controls what kind of support code Alex should produce along with the basic scanner

-- Macros for use in token definitions
$digit = 0-9
$alpha = [a-aA-Z]

-- Ends the macro definitions and starts the definition of the scanner.
tokens :-

  -- Each token specificatino takes the form of regexp  { code }
  -- Actions are all functions from String -> Token
  $white+                         ;
  "--".*                          ;
  let                             { \s -> Let }
  in                              { \s -> In }
  $digit+                         { \s -> Int (read s) }
  [\=\+\-\*\/\(\)]                { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*   { \s -> Var s }

-- Code fragment to declare the type of tokens, and give a main function for testing.
{
-- The token type:
data Token = Let
           | In
           | Sym Char
           | Var String
           | Int Int
           deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)

-- Provided by Alex
alexScanTokens :: String -> [Token]

}
```

`alex Tokens.x` will generate `Tokens.hs`

`alex Tokens.x -o Main.hs` will generate in a different output file named `Main.hs`.

### Lexical Syntax

Predefined macros:

```text
$digit      = [0-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic    = $printable # $white

@string     = \" ($graphic # \")* \"
@id         = [A-Za-z][A-Za-z'_]*
@smac       = '$' id
@rmac       = '@' id
@char       = ($graphic # $special) | @escape
@escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+)
@code       = -- curly braces surrounding a Haskell code fragment
```

 Alex uses an extended form of BNF, where optional phrases are enclosed in square brackets ([ ... ]), and phrases which may be repeated zero or more 
 times are enclosed in braces ({ ... }). Literal text is enclosed in single quotes.

The overall layout of an Alex file is:

```text
alex := [ @code ] [ wrapper ] { macrodef } @id ':-' { rule } [ @code ]
```

Don't add any functions to the first code part since Alex injects imports there too.

The lexer specification can contain a series of macro definitions. There are two kinds of macros, character set macros, which begin with a $, and regular
expression macros, which begin with a @.

```text
macrodef  :=  @smac '=' set
           |  @rmac '=' regexp
```

stayed at 3.2.2. Rules.