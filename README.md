![](https://raw.githubusercontent.com/alexbiehl/hs-fst/master/test.png)

Indices of full text search engines are efficient data structures mapping words to their respective documents. Since an index grows with the number of documents, e.g. words in that documents, efficient organization is
needed to ensure fast lookup times.

This library implements the algorithm introduced in `Incremental Construction of Minimal Acyclic finite-state Automata` by Jan Daciuk and Stoyan Mihov et al. Since simple automaton representations based on maps and sets are not very space efficient this library
compiles those automatons into efficient byte representations on-the-fly. A similar technique is used by Lucene.

Status
------

* Simple construction of minimal automata is working.
* Different backends for compilation is supported (Map based representation, Dot format for graphviz and simple unoptimized binary format)
* No lookup methods specified.
* Api is not stable.

Transducer memory layout
------------------------

## Arc
```
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |S|N|L|O|F|                                                     |
   |I|E|A|U|I|                   Label                             |
   |N|X|S|T|N|                                                     |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Weight                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Target                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Target (contd.)                      |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

* `SIN` Indicates `Arc` is a `State` with a single transition, omit `Weight` field.
* `NEX` Omit `Weight` and `Target`-address since the target state is placed directly after this arc. `NEX` will only be set in combination with `SIN`.
* `LAS` Indicates the last `Arc` in this `State`. (Only used if transducer is compressed)
* `OUT` This is an output transition. This will be described below.
* `FIN` This is a final transition. (Only used if transducer is compressed)

In this context, compression means that all fields are length encoded (Except flags). Since the fields in the compressed fst don't have contant length traversal of such transducers is a linear search instead of binary search.

## Arc with output (only pseudo minimal setting)

```
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |     |1|           Output size                                 |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Output                               |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

Output arcs are considered final arcs. (Needs rethinking)

## State
```
    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |S|       |                                                     |
   |I|       |               Number of transitions                 |
   |N|       |                                                     |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Arcs                                 |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Arcs (contd.)                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                          Arcs (contd.)                        |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

* If `SIN` flag is set treat `State` as single `Arc`

If compressed the the number of transitions will be omitted. Since the `LAS` flag indicates end of arcs.

Plans
-----

* Introduce Typeclass for compilation backends.
* Pseudo minimal finite-state automaton for storing any `Storable`s in an optimized binary format.
* Minimal finite-state automaton using perfect hashing in an optimized binary format.
* Levenshtein automata implementation for fuzzy searching the binary representations.
