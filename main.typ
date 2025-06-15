#import "lib.typ": *
#import "@preview/mannot:0.3.0": *
#import "@preview/finite:0.5.0": automaton
#import "@preview/finite:0.5.0"
#import "@preview/finite:0.5.0": cetz

#let title = [Languages and Machines]
#set page(
   paper: "a4",
   header: align(left, title),
   numbering: "1",
)

#align(center, text(17pt)[
   *#title*
   
   Matteo Bongiovanni\
   Roman Gazarek
])

= Overview

+ Regular languages, recognized by FSM
+ Context-free languages, recognized by PDM
+ Decidable languages, recognized by always terminating TM
+ semi-decidable languages, recognized by TM
- Induction

= Grammars

The *Chomsky hierarchy* - in order of increasing complexity:

+ Right-linear grammars (_equivalent to_ FSM)
+ Context-free grammars (_equivalent to_ PDM)
+ Unrestricted grammars (_equivalent to_ semi-decidable TM)

#image("img/nutshell.png")

== Productive Grammars

A grammar (V, $Sigma$, P, S) is called *productive* if it satisfies:
+ The start symbol S is nonrecursive, i.e., it does not occur at the righthand side of any production rule in P.
+ For every production rule $(A -> w) in P$ with $A != S$ , we have $w in Sigma$ or $|w| >= 2$
Note: The empty string $epsilon$ is generated iff $S -> epsilon$.

=== Recipe

+ Make the start symbol *nonrecursive*
+ *Remove* all forbidden $epsilon$-productions
  - Ensure that $epsilon$ is *not produced by nonterminals* different from $S$
  - *Essentially noncontracting* grammars, *nullable* nonterminals
  
  #bluebox("", [
    A Grammar is *essentially noncontracting* if $S$ is nonrecursive and $(A -> epsilon) in.not P$
    for any $A != S$.

    $A in V$ is *nullable* for $G$ if there is a $G$-derivation $A => ""^* epsilon$
  ])

+ *Remove* forbiden chain productions
  - *Production rules* of the form $A -> B$, with $A, B in V$
  - *Reflexive-transitive closure* of $->$

== Chomsky Normal Form

A grammar $G = (V, Sigma, P, S)$ is in *Chomsky Normal Form* if every production rule
has one of the following forms:

+ $A -> B C$ with nonterminals $A, B, C$ and $B != S$ and $C !=S$
+ $A -> a$ with a nonterminal $A$ and a terminal symbol $a$
+ $S -> epsilon$ for the start symbol $S$ 

=== Productive to Chomsky

$
  T &-> A | epsilon \
  A &-> a A | B | a \
  B &-> b B | b \

  => \

  T &-> a | b | N A | M B | epsilon \
  N &-> a \
  M &-> b \
  A &-> N A | a | N B \
  B &-> M B | b
$

= Regular Languages
== Operations
- $L = {a a, b b}$ and $M = {c, d}$ then $L M = {a a c, a a  d, b b c, b b d}$
- ${a,b, a b}^2 = {a a, a b, a a b, b a, b b, b a b, a b a, a b b, a b a b}$
- ${a,b}^* = {epsilon} union {a, b} union {a a, b b, b a, b b} union {a a a...}$
- ${a b, c d}^R = {b a, d c}$ 
- $|w| =$ length of string $w$
- $n_a (w) =$ the occurances of $a$ in the string $w$

== Definitions
Recursively defined over an alphabet $Sigma$ from
  - $emptyset$
  - ${epsilon}$
  - ${a}$ for all $a in Sigma$
by applying union, concatenation, and Kleene star.

*Regular Expressions*: A notation to denote regular languages
- Example: $a^*(c|d)b^*$ denotes the *regular set* ${a}^*({c}union{d}){b}^*$
- The regular expression of a set is not unique

== Regular Expressions
Regular Expression Identities : \
- $emptyset u = u emptyset = emptyset$
- $epsilon u = u epsilon = u$
- $emptyset^* = emptyset$
- $epsilon^* = epsilon$
- $u | v = v | u$
- $u | emptyset = u$
- $u | u = u$
- $u(v | w) = u v | u w$
- $(u | v)w = u w | v w$
- $u^* = (u^*)^*$
- $(u | v)^* = (u^* | v)^* = u^*(u | v)^* = (u | v u^*)^* = (u^* v^*) = u^*(v u^*)^* = (u^* v u^*)^*$

== DFSM, NFSM, #NeFSM

A *deterministic finite state machine (DFSM)* is a quintuple $M = (Q, Sigma, delta, q_0, F)$ where:
- $Q$ is a finite set of states
- $Sigma$ is the input alphabet
- $delta : Q times Sigma arrow Q$ is the _transition function_
- $q_0$ is the initial state
- $F subset.eq$ is a set of _accepting_ (or _final_) states\
When symbol $a$ is read in a state $q_n$, the state becomes $delta (q_n, a)$.
#align(center, automaton(
  (
  q0: (q1: "a"),
  q1: (),
  ),
  labels: (q0: $q_n$, q1: $q_i$)
  ))
#align(center, text(12pt)[
  $delta(q_n, a) = q_i$
])

=== #NeFSM to DFSM

2 table method

#table(
  columns: 5,
  table.header([], [$epsilon^*$], [a], [b], [c]),
[$q_0$], [${q_0, q_3}$], [-], [-], [-],
[$q_1$], [${q_1}$], [$q_0$], [$q_2$], [$q_1$],
[$q_2$], [${q_1, q_2}$], [-], [$q_0$], [-],
[$q_3$], [${q_3}$], [$q_2$], [$a_0$], [-],
)

#table(
  columns: 4,
  table.header([$delta_D$], [$a epsilon^*$], [$b epsilon^*$], [$c epsilon^*$]),
  [$-> {q_0, q_3}$], [${q_1, q_2}$], [${q_0, q_3}$], [$emptyset$],
  [$*{q_1, q_2}$], [${q_0, q_3}$], [${q_0, q_1, q_2, q_3}$], [${q_1}$],
  [$*{q_0, q_1, q_2, q_3}$], [${q_0, q_1, q_2, q_3}$], [${q_0, q_1, q_2, q_3}$], [${q_1}$],
  [${q_1}$], [${q_0, q_3}$], [${q_1, q_2}$], [${q_1}$],
  [$emptyset$], [$emptyset$], [$emptyset$], [$emptyset$],
)

== FSM to regular expression

For machines in *normal form*

#bluebox("Normal Form", [
  - the start state $q_0$ has no incoming arrows
  - $q_f$ is the only accepting state, and has no outgoing arrows.

  Note: states different from $q_0$ and $q_f$ are called *internal nodes*
])

+ Initially, a label between $q_1$ and $q_2$ is the union of all possible symbols
 that transition from $q_1$ to $q_2$.
+ One by one, we eliminate the internal nodes of the graph, by chaining edges.
+ When all internal nodes are eliminated, there is only one remaining edge,
 namely from $q_0 -> q_f$.
+ The label of the last edge is the resulting regular expression


== FSM minimization

Minimize FSMs by *collapsing* some of its state, to create a different, but smaller machine
that also accepts the same language.

+ Never collapse an accpt state and a reject state
+ if $q$ and $p$ are collapsed, then $delta(q, a)$ and $delta(p, a)$ must also be collapsed
  - If $accent(delta, hat)(p, x) in F$ and $accent(delta, hat)(p, x) in.not F$, for some $x in Sigma^*$. then we cannot collapse $q$ and $p$, otherwise, we can.

=== Equivalence Relations

#let R = math.class(
  "relation",
  $cal(R)$
)

let #R be a relation on a set $S$
 - #R is an _equivalence_ if it is reflective, symmetric, and transitive.
   $
    s #R s "for all" s in S \
    s #R t "implies" t #R s \
    s #R t and t #R u "imply" s #R u \
   $

=== Equivalence Classes and Partitions

Given an equivalence #R on $S$, the _equivalence class_ of $s in S$ is the set

$
  [s] = {t in S | s #R t}
$

Given some non-empty set, a *partition* $cal(P)$ is a set consiting of non-empty subset,
called blocks, which *cover* the set (the union of all partitions of a set is the set itself),
and are *disjoint*, (the intersection of all partitions is the emptyset)

+ Equivalences induce partitions, the set of all equivalence classes is a partition
+ Partitions induce equivalences


=== Computing Equivalence

it s easier to computute $approx.not$

start with states that lead to final, and those that don t lead to final

+ Write down a table with all pairs initially  unmarked
+ Mark ${p, q}$ if $p in F$ and $q in.not F$
+ repeat until no more changes occur:
  if there is an unmarked pari ${p, q}$ such that the pair
  ${delta(p, a), delta(q, a)}$ is marked, then mark the pair
+ at the end we have that $p approx q "iff" {p, q}$ is not marked

=== Indistinguishability of strings

$
  w ~ v &<=> accent(delta, hat)(q_o, w) accent(delta, hat)(q_0, v) \ 
        &<=> forall u in Sigma^* : w u in L <-> v u in L
$
for $w, v in Sigma^*$




== Pumping lemma for regular languages
== Regular expression identities
== Regular grammars
A grammar $(V, Sigma, P, S)$ is *regular* if every production rule in $P$ has one of the following forms $(a in Sigma$ and $A, B in V)$:
- $A arrow a B$ or
- $A arrow epsilon$
A language is regular *iff* it is generated by a regular grammar.\
*Example*: A non-regular grammar for the regular expression $(a b)^* a^*$:\
#align(center, text(12pt)[$S arrow a b S A | epsilon \ A arrow A a | epsilon $])\
An equivalent regular grammar:\
#align(center, text(12pt)[$S arrow a B | epsilon \ B arrow b S | b A \ A arrow a A | epsilon$])

=== Useful, Generating & Generated Symbols
Let $G = (V , Sigma, P, S )$ be a grammar. Let $x in V union Sigma $ be a symbol.
- x is *useful* if there is a derivation\
#align(center, text(12pt)[ $S arrow^* u x v arrow^* w$ with $u, v in (V union Sigma)^*$ and $w in Sigma^*$])
- x is called *useless* if it is not useful
- x is *generating* if $x arrow^* w$ holds for some $w in Sigma^*$
- x is *generated* if there are $u, v in (V union Sigma)^*$ with $S arrow^* u x v$.
Therefore:
- Useful symbols are both generating and generated
- However, generating and generated symbols may not be useful

== Closure properties

= Context-free languages
== Definitions
*Context-Free Grammars*\
A Formal system used to generate the strings of a language. A quadruple ($V, Sigma, P, S$) where\
- $V$ is a set of *variables* or *nonterminals*
- $Sigma$ is an alphabet of *terminals*, disjoint from $V$
- $P$ is a finite set of *production rules*, taken from set $V times (V union Sigma)^*$. We write $A arrow$ instead of $(A, w)$.
- $S in V$ is the *start symbol*.

== PDM

A *pushdown machine* is a tuple $M = (q, Sigma, Gamma, delta, q_0, F)$ where
- $Q$ is a finite set of states
- $Sigma$ is the input alphabet
- $q_0$ is the start state
- $F subset.eq Q$ is a set of accepting/final states
- $Gamma$ is the alphabet for the *stack*
- $delta$ is the transition function
$
  delta : mark(Q, tag: #<Q>, color: #blue) times (mark(Sigma union {epsilon}, tag:#<input_symbol>, color: #purple))
  times (mark(Gamma union {epsilon}, tag:#<pop>, color: #red)) -> cal(P)(mark(Q, tag:#<newQ>, color: #blue) 
  times (mark(Gamma union {epsilon}, tag:#<push>, color: #red)))

  #annot(<Q>)[state]
  #annot(<input_symbol>, pos: top)[input symbol]
  #annot(<pop>)[symbol to pop off]
  #annot(<newQ>, pos: top)[new state]
  #annot(<push>)[symbol to push]
$

Acceptance:
read full input, halt with empty stack on a final state

#align(center, automaton(
  (
    q0: (q0: none, q1: none),
    q1: (q1: none),
  ),
    labels: (
      q0: $q_0$,
      q0-q0: $PDM(a, epsilon, A)$,
      q0-q1: $PDM(epsilon, epsilon, epsilon)$,
      q1: $q_1$,
      q1-q1: $PDM(b, A, epsilon)$
    ),
    style: (
      transition: (curve: 0),
    ),
  )
)


=== Extended PDM
Transitions push strings of symbols onto the stack, rather than just one symbol

== CFG to PDM

From a normalized Context-Free Grammar, we can construct a PDM that accepts that language.

#cetz.canvas({
  import finite.draw: state, loop, transition
  state((0,0), "q0", label: $q_0$)
  state((3,0), "q1", label: $q_1$)
  transition("q0", "q1", label:$PDM(epsilon, epsilon, S)$, curve: 0)
  loop("q1", label:$PDM(a, A, epsilon) "For each" A -> a in P, a in Sigma$)
  loop("q1", anchor: bottom, label:$PDM(epsilon, A, A_1...a_n) "For each" A -> A_1...A_n in P, accent(A, arrow) in V^*$)
})

The stack only stores nonterminals

== PDM to CFG

+ $S -> <Q_0, epsilon q_f>$ (for every $q_f in F$)
+ $<q, A, r> -> a <p, B, r>$ (Use $A$ now for the transition $p attach(->, t: PDM(a, A, B)) q$)
+ $<q, A, r> -> <q, epsilon, p><p, A, r>$(use $A$ from an intermediate $p$)
+ $<q, epsilon, q> -> epsilon$ (done processing)

== Closure properties and proofs

CFLs are closed under *union*, *concatenation* and *Kleene star*

_Not_ closed under _intersection_ and _complementation_

If $R$ is a regualr language and $L$ is a context-free language, then $R inter L$ is CFL

== making a grammar productive


/* Maybe irrelevant?
= Context-sensitive
== Linearly-bounded Machines
*/

= Decidable and semi-decidable languages
== Definitions
== TMs
- Computing a function vs recognizing a language
- Acceptance criteria
- Multi-track, multi-tape, nondeterministic
== Proving decidability, semi-decidability or undecidability


= Support lecture

== Pumping lemma

Prove tht L is not regular by the pumping lemma


There exists a $k >= 0$ such that
for every $z in L "for which" |z| >= k$, there exists a splitting
$z = u v w$ with $|u v| <= k$ and $v != epsilon$ such that
for every $i >= 0$ we have $u v^i w in L$

$forall exists forall exists$


$ L = {w w | w in {a, b}^*} $

Take $z = a^k b a^k b$ By the PL, there is a splitting $z = u v w$ such that $|u v| <= k$
and $v != epsilon$

Because the first $k$ symbols are all 'a'

$
 u = a^i, v = a^j, i + j <= k \
 w = a^(k - i - j) b a^k b
$

$z = u v^0 w$ is not in $L$ (prove), so $L$ is nonregular



