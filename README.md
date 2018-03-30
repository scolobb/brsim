# brsim

`brsim` is a `B`asic `R`eaction `S`ystem `Sim`ulator.  Given the
description of the reactions of a reaction system and a context sequence,
`brsim` can produce the corresponding result sequence, and, if
required, an annotated description of the obtained interactive
process, which includes listing for each step the context, the
result, the full state, as well as the enabled reactions.  `brsim` also
offers an interactive mode in which the context is to be provided by
the user at every step.  Finally, `brsim` can be used to list all the sets which are
conserved in a reaction system.

`brsim` is distributed under the terms of GNU GPLv3 license.

## Reaction Systems

Reaction systems were originally introduced as a formal framework for
investigating processes carried out by biological reactions [0].
Reaction systems are a part of natural computing.  A *reaction system*
is a pair `(S, A)`, where `S` is a *universe* of symbols, while `A` is
a set of reactions over `S`.  A *reaction* over `S` is a triple `(R,
I, P)`, where `R` is the set of reactants which have to be there for
the reaction to run, `I` is the set of inhibitors, which must *not* be
there for the reaction to run, and `P` is the set of products, i.e.,
the symbols which are introduced into the system as a result of
running the reaction.

Reaction systems differ from other rule-based formal frameworks in two
major ways:

 - reaction systems operate on *sets*, and not on strings or multisets
   or whichever different structures allowing repeated elements, and

 - symbols are *not* automatically sustained from state to state,
   i.e., all symbols which make it into the new state are introduced
   by some reaction enabled in the previous state.

When passing from a state to the next one, all enabled reactions run,
so, in a certain sense, reaction systems are fully deterministic.  In
each state, an additional *context* of symbols is supplied to the
system, which can be further used by the reactions.  The contexts are
meant to model the fact that real-world processes (including the
biological ones) are rarely isolated from an environment which
influences (often considerably) the evolution of the system.

For a formal introduction and overview of the reaction systems, the
reader is referred to [0].

## Specifying the Input

The most important thing `brsim` needs to have in its input is the
description of the reaction system it is required to simulate.  This
description should be provided in a file, in the form of a list of
reactions.

There are two different formats to specify reactions.  The one which is
closest to the conventional way of writing reactions as 3-tuples (like
in [0]) is called the *plain format*.  In this format, reactions are
given as three sets of symbols, corresponding to the reactants, the
inhibitors, and the products.  For example, a reaction which consumes
`a` and `b`, produces `c` and `d`, and is inhibited by `e` and `f`
will be written as

```
a b, e f, c d
```

`brsim` also supports a format which may be more intuitive for
newcomers: the *arrow format*, based on the ways chemists normally
write reactions.  In this format, the same reaction will be specified
like this:

```
a + b -> c + d | e f
```

`brsim` allows reactions with empty components, that is, reactions
which have no reactants, inhibitors, or products.  The representation
of empty sets which holds throughout all files read or written by
`brsim` is the dot symbol: **.**.  Thus, a reaction which
unconditionally produces `a` can be written as follows:

```
. , . , a
```

It is possible to just omit the corresponding components and specify
this reaction as `, , a`, but using the dot-syntax is encouraged for
readability and consistency.

Components of the reaction can also be omitted in the arrow format,
and the same reaction could be specified like this:

```
. -> a
```

Note that you don't have to use the vertical bar at all in the arrow
format if the reaction cannot be inhibited.

## Batch Mode

In batch mode, `brsim` should be invoked in the following way:

```
brsim run [options] FILE
```

where `FILE` is the path to the file containing (at least) the
description of the reaction system to simulate.  `FILE` might
optionally include the context sequence, which should be separated
from the description of the system by a line containing three dashes:
`"---"`.  Thus a valid input file `example-plain.rs` may look like
this:

```
a b, e f, c d
. , . , a
. , c , b

---

a b
c d
e f
j
```

The contexts are specified one per line, so the first context is the
set containing `a` and `b`, the second context is the set containing
`c` and `d`, etc.  Empty lines and lines starting with the hash symbol
(comment lines) are ignored.  This is where the dot-notation for empty
sets comes in really handy, since you can specify an empty context by
just putting a `.` on the corresponding line.

Correspondingly, the file `example-arrow.rs` containing the
description of the same reaction system in the arrow format might look
like this:

```
a + b -> c + d | e f
. -> a
-> b | c

---

a b
c d
e f
j
```

Running `example-plain.rs` is as easy as typing

```
brsim run example-plain.rs
```

on your command line.  `brsim` will then write the result sequence
of the interactive process given by the sequence of contexts given in
the input file:

```
a b c d
a c d
a
a b
```

To run `example-arrow.rs`, you should explicitly tell `brsim` that the
reactions are specified in the arrow format:

```
brsim run --format=arrow example-arrow.rs
```

`brsim` can be told to write the result sequence to a file rather then
to the standard output via the `--output` option:

```
brsim run example-plain.rs --output=example.out
```

Finally, to get nice detailed annotations for the interactive process,
you should use the `--annotate` option in the following way:

```
brsim run example-plain.rs --annotate=example.an
```

Context sequences can also be specified in a separate file.  If you
place the following text:

```
a b c
d
d
a b c
```

into `example.ctx` and then run `brsim` like this:

```
brsim run example-plain.rs --context=example.ctx
```

you should get the following output:

```
a c d
a
a b
a c d
```

## Interactive Mode

In interactive `brsim` will ask you for contexts at every new step of
the simulation.  Thus it is not necessary to specify any context
sequence beforehand, but if you do, `brsim` will run the simulation
for the given context sequence and will start prompting you for new
contexts after it has exhausted the input.  Therefore, if you type:

```
brsim interact example-plain.rs
```

`brsim` will produce the following prompt:

```
Context sequence provided.  The description of the last reached state follows.

STEP 3
Context:       j
Last result:   a b c d
State:         a b c d j
Enabled rules:
  ., ., a
  a b, e f, c d
New result: a b c d j

Next context:
```

You can use the same options as for the batch mode to control the
format of the input file and how and which output files are produced.
An option specific to the interactive mode is `--output-context`,
which is used to specify the file into which `brsim` will write the
*context sequence*, so that you can keep a full record of your actions
in an interactive session.

## Retrieving Information about the Reaction System

`brsim` can be told to construct a number of objects providing
structural and dynamic information about a reaction system via the
command line command `show`.

One of the most basic objects describing the dynamics of a reaction
system is the behaviour graph.  To get the adjacency lists of this
graph, use the following syntax:

```
brsim show behaviour-graph [FILE]
```

`brsim` can build the conservation dependency graph for the given
reaction system.  The vertices of this graph are labelled with the
symbols from the alphabet of the reaction system.  Two vertices 'x'
and 'y' are connected if all conserved sets containing 'y' also
contain 'x'.

To have `brsim` build the conservation dependency graph, run the
following command:

```
brsim show cons-dep-graph [FILE]
```

`brsim` is also capable of listing the sets which are conserved by a
given reaction system.  To achieve that goal, it should be invoked in
the following way:

```
brsim show conserved-sets [FILE]
```

The input file need only contain the definition of the reaction system
you want to list the conserved sets for; any context definitions will
be just ignored.

## Getting Help

If you type `brsim help`, you will get a more or less detailed
explanation of the commands and options `brsim` takes.  To learn more
about reaction systems I recommend the reference [0] in the
bibliography.  To find more information about conservation dependency
graphs and the algorithm for listing all conserved sets, check out the
technical report [1].  If your question is not covered by the present
`README` or `brsim help`, feel free to read the code directly or
contact me at `sergiu` dot `ivanov` at something like `u-pec.fr`.

## Web Interface

It is also possible to use the simulator
[online](http://combio.abo.fi/research/reaction-systems/reaction-system-simulator/)
on the Web page of the [Combio laboratory](http://combio.abo.fi) at
the Åbo Akademi University.  This page also lists further examples of
reaction systems.

## References

[0] Robert Brijder, Andrzej Ehrenfeucht, Michael G. Main, Grzegorz
Rozenberg.  *A Tour of Reaction Systems*.
Int. J. Found. Comput. Sci., vol 22 (7), 2011,
pp. 1499--1517. [DOI](http://dx.doi.org/10.1142/S0129054111008842).

[1] Sepinoud Azimi, Cristian Gratie, Sergiu Ivanov, Ion Petre,
*Dependency Graphs and Mass Conservation in Reaction Systems*. TUCS
Technical Reports 1123, 2014.
