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

#set heading(numbering: "1.")
#outline()

= Grammars

The *Chomsky hierarchy* - in order of increasing complexity:

+ Right-linear grammars (_equivalent to_ FSM)
+ Context-free grammars (_equivalent to_ PDM)
+ Unrestricted grammars (_equivalent to_ semi-decidable TM)

#image("images/nutshell.png")

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

== Limitations of regular languages
- Can only read left to right
- Finite and bounded memory
- Can only keep track of bounded history

== Proving that a language is not regular
\
*Example 1: $L_1 = {a^n b^n | n >= 1}$*\
A proof that $L_1$ is not regular:
- If $L_1$ is regular, it would be accepted by a *DFSM* $M$
- Machine M has a finite number of states, $k$
- Consider the action of $M$ on input $a^n b^n$, with $n >> k:$\
// "Trust me bro typst is so much nicer than Latex"
// Skill issue - Matteo

#let colblue(math) = text(fill: blue, $#math$)

#align(center)[
  $colblue([q_0]) underbrace(a a a a a a a a a, n) thin underbrace(b b b b b b b b b, n) colblue([q_f])$
]

- By the pigeonhole principle (those who know), there is a state $q_i$ that is visited more than once in processing the sequence of $a$'s:
#align(center)[
  $
  colblue([q_0]) overbrace(a a a a colblue([q_i]) + a a + colblue([q_i]) + a a, n) thin overbrace(b b b b b b b b b, n) colblue([q_f])
  $
]

- Let's split $a^n b^n$ into three pieces $(u,v,w)$ according to $q_i$:

#align(center)[
  $
    colblue([q_0])
    underbrace(a a a a, u) colblue([q_i]) underbrace(a a, v) colblue([q_i]) underbrace(a a b b b b b b b b b, w) colblue([q_f])
  $
]

#align(center)[
  We have:
$
 underbrace(accent(delta, "^")(q_0, u)=q_i, -->),  underbrace(accent(delta, "^")(q_i, v)=q_i, arrow.ccw), and  underbrace(accent(delta, "^")(q_i, w)=q_f in F, -->).]
$
]

What does $arrow.ccw$ mean?
- We could erase $v$ and the obtained string would be accepted!

$
  colblue([q_0])  underbrace(a a a a, u) colblue([q_i]) underbrace(a a b b b b b b b b b, w) colblue([q_f])
$
#align(center)[This is wrong: $u w = a^(n-j)b^n in L(M)$ but $u w in.not L_1$]
- We could even insert extra copies of $v$ and the resulting string would also be accepted.

*Example 2: $L_2 = { a^(2^n) | n > 0}$*

- A DFSM $M$ with $k$ states, with $L(M) = L_2$ and start state $q_0$
- Let $n >> k$ and consider the action of $M$ on scanning string $a^(2^n)$
- By pigeonhole principle, $M$ must repeat a state $q$ while scanning the first $n$ symbols of $a^(2^n)$
- Now let $i,j,m$ be such that $2^n=i+j+m$, with $0 < j <= n$ and
#align(center)[
  $accent(delta, "^")(q_0, a^i)=q, accent(delta, "^")(q, a^j) = q, accent(delta, "^")(q, a^m) = q_f in F$
]
- Alternatively:
$
  colblue([q_0])
   overbrace(
    underbrace(a a a a a a a a a a, i) +
    underbrace(a a a a a, j) + underbrace(a a a a a a a a a a a a a a a a, m), 2^n
  )
  colblue([q_f])
$

- Now given $accent(delta, "^")(p, a^j) = p$, we could insert an extra $a^j$, to get $a^(2^n + j)$, and the resulting string would be erroneously accepted:

$
  colblue([q_0])
   underbrace(a a a a a a a a a a, i)
  colblue([q])
   underbrace(a a a a a, j)
  colblue([q])
   underbrace(a a a a a, j)
  colblue([q])
   underbrace(a a a a a a a a a a a a a a a a, m)
  colblue([q_f])
$

#align(center)[Indeed, we can derive $accent(delta, "^")(q_0, a^(2^n + j)) = q_f in F$]

- But this is wrong, because $2^n + j$ is not a power of 2:

#align(center)[
  $2^n + j <= 2^n + n$\
  $2^n + j < 2^n + 2^n$\
  $= 2^(n +1)$\
  $2^(n+1)$ is the next power of $2$ greater than $2^n$
]

Apparently this is a contradiction.

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
- $delta : Q times Sigma -> Q$ is the _transition function_
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

- *Determinism*
 - A system that is deterministic will always produce the same output/behaviour for a given input from a given starting state

- *Non-determinism*
 - A system is nondeterministic if it may produce a different output when running with the same input in the same state.

A *Non-deterministic finite state machine (NFSM)* is a quintuple $M = (Q, Sigma, delta, q_0, F)$ where:
- $Q$ is a finite set of states
- $Sigma$ is the input alphabet
- $delta : Q times Sigma -> PP(Q)$ is the _transition function_

#bluebox("Important Note!!!")[- $PP(Q)$ denotes the set of all subsets $Q$]

- $q_0$ is the initial state
- $F subset.eq$ is a set of _accepting_ (or _final_) states\
#bluebox("Important Note v.2!!!")[
- When symbol $a$ is read in a state $q$, the next state is in the set $delta (q, a)$.
// Cringe typst moment
- We define $(q, a w) tack.r""_M (q',w)$ if $q' in delta(q, a)$.
]

An *N$epsilon$FSM* is a quintuple $M = (Q, Sigma, delta, q_0, F)$ where:
- $Q$ is a finite set of states
- $Sigma$ is the input alphabet
- $delta : Q times (Sigma union {epsilon}) -> PP(Q)$ is the _transition function_

- $q_0$ is the initial state
- $F subset.eq$ is a set of _accepting_ (or _final_) states

$delta(q_n, epsilon):$ Set of states reachable from $q_n$ without reading input:

#align(center, automaton(
  (
    // For some reason doing $epsilon$ doesn't work
  q0: (q1: "\u{03b5}"),
  q1: (),
  ),
  labels: (q0: $q_n$, q1: $q_i$),
))

- For the transition relation we have
 - $(q, a w) tack.r""_M (q', w)$ if $q in delta(q, a);$
 - $(q, w) tack.r""_M (q', w)$ if $q' in delta(q, epsilon)$.

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
Dumbass lemma. You have a sufficiently long string, find a substring which you can pump (repeat an arbitrary amount of times),bam you've proven a language is not regular. Better explanation below:

Prove tht L is not regular by the pumping lemma


There exists a $k >= 1$ such that
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



== Regular grammars
A grammar $(V, Sigma, P, S)$ is *regular* if every production rule in $P$ has one of the following forms $(a in Sigma$ and $A, B in V)$:
- $A -> a B$ or
- $A -> epsilon$
A language is regular *iff* it is generated by a regular grammar.\
*Example*: A non-regular grammar for the regular expression $(a b)^* a^*$:\
#align(center, text(12pt)[$S -> a b S A | epsilon \ A -> A a | epsilon $])\
An equivalent regular grammar:\
#align(center, text(12pt)[$S -> a B | epsilon \ B -> b S | b A \ A -> a A | epsilon$])

=== Useful, Generating & Generated Symbols
Let $G = (V , Sigma, P, S )$ be a grammar. Let $x in V union Sigma $ be a symbol.
- x is *useful* if there is a derivation\
#align(center, text(12pt)[ $S =>""^* u x v =>""^* w$ with $u, v in (V union Sigma)^*$ and $w in Sigma^*$])
- x is called *useless* if it is not useful
- x is *generating* if $x =>""^* w$ holds for some $w in Sigma^*$
- x is *generated* if there are $u, v in (V union Sigma)^*$ with $S =>""^* u x v$.
Therefore:
- Useful symbols are both generating and generated
- However, generating and generated symbols may not be useful

== Closure properties

- _Union_, _concatenation_, _Kleene Star_:\ Immediate by considering regular expressions
- _Complement_:\Obtain a DFSA $M$ of the given language, and define another DFSA $M'$ in which the accepting states of $M$ become non-accepting states of $M'$, and vice versa
- _Intersection_: Two possibilities:\

+ Use the law $L_1 inter L_2 = overline(overline(L_1) union overline(L_2))$ (and reduce the cases above)
+ Define a construction that runs two DFSAs in "parallel"\
Given $M_i = (Q_i, Sigma, delta_i, q_i, F_i)$ (with $i in {1,2}$), define $M=(Q_1 times Q_2, Sigma, delta, (q_1,q_2), F_1 times F_2)$, where $delta((p,q),a) = (delta_1(p,a),delta_2(q,a))$.

- _Reversal_: Given a machine $M$ and create a machine $M^R$ by:
 - convert $M$ to normal form
 - reverse all arcs in the state diagram
 - swap the starting and the accepting states


== Regular grammars
A grammar $(V, Sigma, P, S)$ is *regular* if every production rule in $P$ has one of the following forms $(a in Sigma$ and $A, B in V)$:
- $A -> a B$ or
- $A -> epsilon$
A language is regular *iff* it is generated by a regular grammar.\
*Example*: A non-regular grammar for the regular expression $(a b)^* a^*$:\
#align(center, text(12pt)[$S -> a b S A | epsilon \ A -> A a | epsilon $])\
An equivalent regular grammar:\
#align(center, text(12pt)[$S -> a B | epsilon \ B -> b S | b A \ A -> a A | epsilon$])

=== Useful, Generating & Generated Symbols
Let $G = (V , Sigma, P, S )$ be a grammar. Let $x in V union Sigma $ be a symbol.
- x is *useful* if there is a derivation\
#align(center, text(12pt)[ $S =>""^* u x v =>""^* w$ with $u, v in (V union Sigma)^*$ and $w in Sigma^*$])
- x is called *useless* if it is not useful
- x is *generating* if $x =>""^* w$ holds for some $w in Sigma^*$
- x is *generated* if there are $u, v in (V union Sigma)^*$ with $S =>""^* u x v$.
Therefore:
- Useful symbols are both generating and generated
- However, generating and generated symbols may not be useful

== Closure properties

- _Union_, _concatenation_, _Kleene Star_:\ Immediate by considering regular expressions
- _Complement_:\Obtain a DFSA $M$ of the given language, and define another DFSA $M'$ in which the accepting states of $M$ become non-accepting states of $M'$, and vice versa
- _Intersection_: Two possibilities:\

+ Use the law $L_1 inter L_2 = overline(overline(L_1) union overline(L_2))$ (and reduce the cases above)
+ Define a construction that runs two DFSAs in "parallel"\
Given $M_i = (Q_i, Sigma, delta_i, q_i, F_i)$ (with $i in {1,2}$), define $M=(Q_1 times Q_2, Sigma, delta, (q_1,q_2), F_1 times F_2)$, where $delta((p,q),a) = (delta_1(p,a),delta_2(q,a))$.

- _Reversal_: Given a machine $M$ and create a machine $M^R$ by:
 - convert $M$ to normal form
 - reverse all arcs in the state diagram
 - swap the starting and the accepting states

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

#align(center,
  cetz.canvas({
    import finite.draw: state, loop, transition
    state((0,0), "q0", label: $q_0$)
    state((3,0), "q1", label: $q_1$)
    transition("q0", "q1", label:$PDM(epsilon, epsilon, S)$, curve: 0)
    loop("q1", label:$PDM(a, A, epsilon) "For each" A -> a in P, a in Sigma$)
    loop("q1", anchor: bottom, label:$PDM(epsilon, A, A_1...a_n) "For each" A -> A_1...A_n in P, accent(A, arrow) in V^*$)
  })
)

The stack only stores nonterminals

== PDM to CFG

+ $S -> angle.l Q_0, epsilon q_f angle.r$ (for every $q_f in F$)
+ $angle.l q, A, r angle.r -> a angle.l p, B, r angle.r$ (Use $A$ now for the transition $p attach(->, t: PDM(a, A, B)) q$)
+ $angle.l q, A, r angle.r -> angle.l q, epsilon, p angle.r angle.l p, A, r angle.r$(use $A$ from an intermediate $p$)
+ $angle.l q, epsilon,  angle.r -> epsilon$ (done processing)

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
- A Turing machine may access and modify any memory position, using a sequence of elementary operations
- No limitation on the space/time available for a computation
- A finite state machine equipped with a *tape*, divided into *squares*, which can be written on as a result of a transition
- The *head* of the machine can move to the right or to the left, allowing the TM to read and manipulate the input as desired\

In other words, a transition:
- Changes the state
- Writes a symbol on the square scanned by the head
- Moves the head

A (simple) *Turing machine* $M$ is a quintuple ($Q, Sigma,Gamma, delta, q_0$) where:
- $Q$ is a set of *states*
- $q_0 in Q$ is the *start state*
- $Gamma$ is the *tape alphabet*, a set of symbols disjoint from $Q$. Contains a *blank symbol* $B$, not in $Sigma$
- $Sigma subset.eq Gamma backslash {B}$ is the *input alphabet*
- The transition function $delta$ is a partial function such that
#align(center)[$delta: Q times Gamma -> Q times Gamma times {L,R}$\ If $delta(q,X)$ is undefined then $delta(q,X) = bot$.\ ]

A set of accepting states $F subset.eq Q$ is possible but not indispensable for acceptance.\ \
A TM that reads the input string and interchanges symbols $a$ and $b$:
#align(center, automaton(
    (
    q1: (q1: none,q2: none),
    q2: (q2: none)
    ),
    initial: "q1",
    final: none,
    labels: (
      q1-q1: $TM(a, b, tml) \ TM(b, a, tmr)\ $,
      q1-q2: $TM(blank, blank, tml)$,
      q2-q2: $TM(a, a, tml)\ TM(b, b, tml)\ $,
    ),
  )
)
In state $q_1$, label '$a slash b R$' indicates:
- symbol $a$ is rewritten into $b$
- the head moves right ($R$)
\ \

The global state of the TM is determined by the state $q in Q$, the contents of the tape (a string in $Gamma^*$) and the position of the head
- A *configuration* of the TM is a string $u q v$ in $Gamma^* Q Gamma&*$, in which:
 - $u$ is a string on the tape to the left of the head
 - $q$ is the *current* state
 - $v$ is a string on the tape that begins under the head
- The initial configuration is $q_0 w$, where $w in Sigma^*$ is the input string
- The first symbol of $v B^(infinity)$ is called the *current* symbol

Suppose $X, Y, Z$ are tape symbols (in $Gamma$).\
Moving to the next configuration:
#align(center)[
  $
  delta(q, X)=(r, Y, R) => u Z q X v tack.r u Z Y r v\
  delta(q, X) = (r,Y,L) => u Z q X v tack.r u r Z Y v\
  delta(q, X) = bot => u q X v tack.r bot
  $
]
- A computation is a sequence of steps, as defined by $tack.r$
- A TM *computes a function* $f$
 - If starting in $q_0 w$, the final tape upon termination is always $B^(infinity) u B^(infinity)$, with $u = f(w)$.

#figure(
  image("images/tm1.png"),
  caption: [
    A TM which duplicates the input string $w in {0, 1}^*$
  ]
)

=== Acceptance
The set $L(M)$ can be defined in two different ways.
+ A TM $M$ *accepts by termination* the language of the input strings $w$ for which it terminates: #align(center)[
  $L(M) = {w in Sigma^* | q_0w tack.r""^* bot}$\ No need for accepting states.]
+ $L(M)$ can also be defined by *termination in an accepting state*, extending $M$ with a set $F subset.eq Q$:
#align(center)[$L(M)={w in Sigma^* | exists q_f in F, u ,v in Gamma^* : q_0 w tack.r""^* u q_f v tack.r bot}$]
This definition can be reduced to the first one by letting $F = Q$. In fact, both definitions are equivalent.\
#figure(
  image("images/tm2.png"),
  caption: [
    A TM with accepting state(s)
  ]
)

=== Further Terminology
A TM is *always terminating* if it terminates for every input.\ \
Let $L$ be a language.
- $L$ is *semi-decidable* (or *recursively enumerable, RE*) if there exists a TM $M$ such that $L = L(M)$.
- $L$ is *decidable* (or *recursive*) if there is an always terminating TM that accepts $L$ by termination in an accepting state.
- If $L$ is decidable, then it is also semi-decidable. *The converse doesn't hold!!!*

== Variations of TMs

- two way: the tape extends infinity in both directions
- Multi-track: the equivalent of using tuples in the tape
  #image("images/multi-track.png")
- multi-tape
  #image("images/multi-tape.png")
  #bluebox("Simulating Mulitape with Multitrack", [
    Simulating a two-tape machine using a five-track machine:
    - Tracks 1 and 3 maintain the information on tapes 1 and 2
    - Tracks 2 and 4 use a symbol $X$ to indicate the position of the heads
    - Track 5 uses a symbol \# to control the simulation
    In general, a language accepted by a $k$-tape machine is accepted by a $2k + 1$-track machine
  ])
- nondeterministic (NTMs)
  $
    delta : Q times Gamma -> cal(P)(Q times Gamma times {tml, tmr})
  $
  When more than one transition is possible, the computation chooses arbitrarily one of them

  An NTM may produce several computations for a single input
  string. The string is accepted if there is a computation that
  terminates.

  The reader shows how to represent NTM as a deterministic two tape TM, but it s BS.

- Any conbination is possible, and all TMs are equivalent

== Always Terminating NTMs

there are 3 kinds of computations with accepting NTMs
+ Terminating and accepting
+ Terminating and non-accepting
+ Non terminating

an input is accepted iff it has at least one accepting computation

== Complexity classes

A (non)deterministic TM $M$ has *time complexity* $T(n)$
if $M$ is guaranteed to terminate in at most $T(n)$ steps for every input
string $w$ of length $n$.

Let $L$ be a language and let $T(n)$ be a *polynomial function*:
- $L$ belongs to the class $P$ if there is a deterministic TM
  $M$ with $L = L(M)$ and with time complexity $T(n)$
- $L$ belongs to the class $N P$ if there is an NTM $M$ with $L = L(M)$
  and with time complexity $T(n)$
- Because every deterministic TM can be regarded as an NTM with the same time complexity,
  we have $P subset.eq N P$
- Conjecture: $P != N P$.

== Closure Properties

+ $L$ is decidable $=>$ $L$ is semi-decidable
+ $L$ is decidable $=>$ $overline(L)$ is decidable
+ $L$ and $overline(L)$ are semi-decidable $<=>$ $L$ is decidable
+ $L$ is semi-decidable $<=>$ $L^*$ is semi-decidable
+ $L_1$ and $L_2$ are semi-decidable $=>$ $L_1 L_2, L_1 union L_2, L_1 inter L_2$ are semi-decidable

- Computing a function vs recognizing a language

== Proving decidability, semi-decidability or undecidability
- How to distinguish between the computable and non-computable?
 - Turing machines (TMs)
 - Combinatory logic
 - The $lambda$-calculus (haskell reference!!!)
- All of these are equivalent - they embody the same notion of *effective computation*, from different angles
- Deterministic TMs are arguably closer to actual computers than the other formalisms

=== Church-Turing's Thesis
- While formalisms such as TMs, combinatory logic, $lambda$-calculus,
etc, are vastly dissimilar, they have a striking commonality
- *Effective computability*: they capture our intuition about what
it means to be effectively computable — no more and no less
- *Church-Turing's thesis*: computability is not just TMs, nor
Java, nor the $lambda$-calculus, but the 'common spirit' they embody
- Not a theorem, but an observation (perhaps unsurprising today)
- TMs were a key step towards acceptance of the Church-Turing's
thesis: they were the first readily programmable model
- Of course TMs do not address all possible aspects of
computation (say, interactivity or randomness) but they do
capture a robust notion of effective computability

=== Universality
- *Programs as data*:
 - TMs are powerful enough that programs can be written to read/manipulate other programs (encoded as data)
- TMs can interpret the input strings as descriptions of other TMs
- A *universal machine* $U$ is constructed to take an encoded description of another machine $M$ and a string $x$ as input. $U$ can perform a step-by-step simulation of $M$ on input $x$
- This is computers as we know them today

=== Self-reference
- A consequence of universality, and key to the discovery of uncomputable problems
- Observation: there are uncountably many *decision problems* but countably many TMs
- Extremely powerful: Gödel's incompleteness theorem, whose proof exploits self-reference

=== Terminology

A language $L$ is
- *recursive* if $L = L(M)$ for some #underline("always terminating") TM $M$
- *recursively enumerable* if $L=L(M)$ for some TM $M$\ \
Alternatively, let $P$ be a *property* of strings.
- $P$ is #text(red)[decidable] if the set of all strings having $P$ is recursive: there is a total TM that accepts strings that have $P$ and rejects those that don't
- $P$ is #text(red)[semi-decidable] if the set of strings having $P$ is recursively enumerable: there is a TM that accepts $x$ if $x$ has $P$ and _rejects or loops if not_ \ \

*Recursive* and *recursively enumerable* are best applied to sets, while *decidable* and *semi-decidable* to properties
- Property $P$ is decidable $<=>$ Set ${x | P(x)}$ is recursive
- Set $A$ is recursive $<=>$ $x in A$ is decidable
Similarly:
- Property $P$ is semi-decidable $<=>$ Set ${x | P(x)}$ is recursively enumerable
- Set $A$ is recursively enumerable $<=> x in A$ is semi-decidable

=== Decision problems
A question which expects a 'yes' or 'no' answer, depending on some given *instance* (positive or negative). We would like to have procedures (programs, TMs) to answer correctly this question in all cases.\
Examples:
+ Given a graph, is there a path between two of its nodes?
+ Is $n in NN$ the difference between two prime numbers?
+ Given a CFG $G$ and a string $w$, do we have $w in L(G)$?
+ Given a CFG $G$, does $L(G)$ contain a palindrome?
+ Given a TM $M$ and a string $w$, does it hold that $w in L(M)$?
+ Given a program $P$, does the call of $P$ with input $I$ terminate?

=== Decidable and semi-decidable problems
A problem is:
- *decidable* if there is a procedure (program or TM) able to answer the question correctly in all cases
- *semi-decidable* if there is a procedure that
 - for every positive instance terminates with answer 'yes'
 - for every negative instance terminates with answer 'no' or loops

*Examples*\
+ Given a graph, is there a path between two of its nodes? #text(blue)[(decidable)]
+ Is a natural $n$ the difference between two prime numbers? #text(rgb("#ff2bae"))[(semi-decidable)]
+ Given a CFG $G$ and a string w , do we have $w in L(G)$? #text(blue)[(decidable)]
+ Given a CFG G, does $L(G)$ contain a palindrome? #text(red)[(not decidable)]
+ Given a TM $M$ and a string $w$ , does it hold that $w in L(M)$? #text(red)[(not decidable)]
+ *The halting problem*: Given a program $P$, does the call of $P$ with input $I$ terminate? #text(red)[(not decidable)]

=== Turing Machines, in Plaintext
*From* $M$ *to* $R(M)$
- Define a *numbering function* $n$ that maps each state $q$ into a positive integer $n(q)$. Similarly for symbols in the tape alphabet and for the direction $d in {L,R}$.
- The functions may clash: $n(q_0)=1, n(0)=1$ and $n(L)=1$.
- Let us write $1^k$ to denote $underbrace(11...1, k "times")$. A transition #align(center)[
  $delta(q,X) = [r,Y,d]$ is represented as:\ $001^(n(q))01^(n(X))01^(n(r))01^(n(Y))01^(n(d))$
]
- Given $M$, its string representation $R(M)$ corresponds to a sequence of encoded transitions, followed by '$000$'
- Hence, assuming an input alphabet of bits, the string $R(M)w$ corresponds to the regular expression #align(center)[$underbrace((0(01^+)^5)^*000, R(M))underbrace((0|1)^*, "input" w)$]


=== The halting problem for TMs

*Proof by contradiction*
+ Assume there is a TM $H$ that solves the halting problem. A string is accepted by $H$ if
 - the input consists of two strings, $R(M)$ and $w$
 - the computation of $M$ with input $w$ halts.\ Otherwise, $H$ rejects the input. #image("images/halting1.png")
+ Modify $H$ to build another TM, called $K$: the computations of $K$ are the same as $H$, but $K$ loops indefinitely whenever $H$ terminates in an accepting state (whenever $M$ halts on $w$).
+ Combine $K$ with a "copy machine" to build another TM called $D$, with $D(M) = K(M,M)$ as follows: #figure(
  image("images/halting3.png"),
)
+ The input to $D$ may be the representation of any TM, even $D$ itself. Adapting the diagram: #image("images/halting4.png")\ Thus, $D(D)$ terminates iff $D(D)$ doesn't terminate. A contradiction derived from the assumption that there is a machine $H$ that solves the halting problem.

=== Halting problem without input
A "simpler" problem which is also undecidable.\
Given a program $P$ #underline("without input"), is there a program $Q$ that can decide whether or not $P$ terminates?
+ Assume $Q$ does exist, and is an always terminating program with boolean output.
+ $Q(P)$ terminates iff the call $P$ terminates
+ Define a "linker" $L$: a program that calls program $P_i$ with input $I$
+ $L(P_i, I)$ is a program without input, for any $P_i$ and $I$
+ Thus, $Q(L(P_i,I))$ terminates iff the call $P_i(I)$ terminates
+ Define a program $Q'$ such that $Q'(P_i, I) = Q(L(P_i,I))$
+ $Q'$ would decide the halting problem - a contradiction

== Universal TM

A Universal TM can read TM representations as input and simulate running it

$R(M)$ is the representation of M as a string

$M$ terminates on in $w$ iff UTM terminates on input $R(M)w$

=== Running a binary on a 3-tape UTM

+ check the format of the input, enter an infite loop if invalid
+ move input to tape 2
+ Write 1 on tape 3
+ Simulate by repeating:
  + Find a transition based on
    - the state (tape 3)
    - the current symbol (tape 2)
  + If no transition is found, terminate
  + otherwise, if a transition is found, change the state,
  change the symbol, move the head (tape 2)


== Pumping lemma


