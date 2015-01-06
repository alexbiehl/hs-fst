Indices of full text search engines are efficient data structures mapping words to their respective documents. Since an index grows with the number of documents, e.g. words in that documents, efficient organization is
needed to ensure fast lookup times.

This library implements the algorithm introduced in `Incremental Construction of Minimal Acyclic finite-state Automata` by Jan Daciuk and Stoyan Mihov et al. Since simple automaton representations based on maps and sets are not very space efficient this library
compiles those automatons into efficient byte representations on-the-fly. A similar technique is used by Lucene.

![](https://raw.githubusercontent.com/alexbiehl/hs-fst/master/test.png)


Status
------

* Simple construction of minimal automata is working.
* Different backends for compilation is supported (Map based representation, Dot format for graphviz and simple unoptimized binary format)
* No lookup methods specified.
* Api is not stable.

Plans
-----

* Introduce Typeclass for compilation backends.
* Pseudo minimal finite-state automaton for storing any `Storable`s in an optimized binary format.
* Minimal finite-state automaton using perfect hashing in an optimized binary format.
* Levenshtein automata implementation for fuzzy searching the binary representations.
