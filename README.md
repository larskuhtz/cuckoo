[![Build Status](https://travis-ci.org/larskuhtz/cuckoo.svg?branch=master)](https://travis-ci.org/larskuhtz/cuckoo)

Haskell implementation of Cuckoo filters as described in

[B. Fan, D.G. Anderson, M. Kaminsky, M.D. Mitzenmacher. Cuckoo Filter:
Practically Better Than Bloom. In Proc. CoNEXT,
2014.](https://www.cs.cmu.edu/~dga/papers/cuckoo-conext2014.pdf)

Cuckoo filters are a data structure for probabilistic set membership. They
support insertion, deletion, and membership queries for set elements.

Membership queries may return false positive results. But queries don't return
false negative results.

Unlike Bloom filters, Cuckoo filters maintain an upper bound on the false
positive rate that is independent of the load of the filter. However, insertion
of new elements in the filter can fail. For typical configurations, this
probability is very small for load factors smaller than 90 percent.
