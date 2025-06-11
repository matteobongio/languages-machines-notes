#import "lib.typ"

#let title = [Languages and Machines]
#set page(
   paper: "a4",
   header: align(left, title),
   numbering: "1",
)

#align(center, text(17pt)[
   *#title*
   
   Matteo Bongiovanni
])

= Regualr Languages
== Definitions
== Regualr Expressions
== DFSM, NFSM, N$epsilon$FSM
== FSM to regualr expression
== FSM minimization
== Pumping lemma for regular languages
== Regular expression identities
== regular grammars
== Closure properties

= Context-free languages
== Definitions
== PDM
== CFG to PDM
== Closure properties and proofs
== making a grammar productive

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


