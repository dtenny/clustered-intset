# `clustered-intset` - a set-of-integers implementation favoring clustered keys

This is a hash-based set implementation supporting arbitrarily large non-negative integer keys,
which is moderately well suited for large numbers of clustered key values.

The implementation uses Common Lisp's hash tables such that multiple set
members share a hash key and hash value.  This provides us the usual O(logN)
capabilities of a hash table but reduces the required number of hash table
entries by (for reasonably clustered data) by an order of magnitude.
Furthermore, it uses only fixnums as hash
table _values_ (regardless of the size of integers in the set membership),
thereby minimizing memory allocations.  The net result is that if all your integer set
members are fixnums, there are no extraneous allocations for references to
boxed objects from your hash table (related to the intset data, at any rate).
You can also have intset members which are larger than fixnums. In those cases
only the hash table key is boxed, but not the value.

This release was tested in sbcl 2.1.6 and ABCL 1.8.0.

# To load and run the sample tests

Assuming this has been provided by quicklisp and/or you've modified your
quicklisp local-projects configuration to refer to the git repo cloned from
https://github.com/dtenny/clustered-intset.

    (ql:quickload :clustered-intset-test)
    (clustered-intset-test:run-tests)

# More on representation (skip if you don't care).

When an integer 'k' is added to the set, its hash table key is `(/ k
fixnum-bits)`, and its hashtable value is a fixnum used as a bitset for
members in the range `0 - fixnum-bits` relative to the base `(floor k
fixnum-bits)`.

Using SBCL as an example, it has 62 bit fixnums on the machine where this was
developed. Every hash table entry could represent up to 62 integer values in a
single hash table entry.  An abusive example such as a dense value interval
`[0-1,000,000)` would have 16,129 entries in the hash table, all keys and all
values being fixnums. 

This implementation isn't designed for additional dense set techniques such as
compression. It's fairly simple, and
benchmarks were satisfactory w.r.t. the primary use-case that motivated
this implementation. If you need more support for sparse or dense bitsets, see the references
at the end of this note, in particular [CL-ROARING](https://github.com/dtenny/cl-roaring).

Set members exceeding MOST-POSITIVE-FIXNUM will also work, potentially
resulting in non-fixnum keys, but the hash-table values will still be fixnums.

# Motivating use-case

I wanted to do some processing which involved representing many integer primary keys
from a database in memory. In my case, I might want to pull in a million primary keys at a time
and process them in ascending order.  

With respect to Common Lisp tools, a million integers isn't a problem IF THEY
START AT ZERO.  You could use a bit-vector for that.  The problem is when your
integer primary key values have a range starting in the millions or billions.
Billion element bit-vectors would require 125 megabytes of memory unless you
do something smarter, and unfortunately I couldn't find any Common Lisp bit
vector tools that were smarter with the possible exception of FSET (see
below). Plain hash tables can handle large integers just fine, though you'll find
that this package can substantially reduce memory requirements for large
cardinality sets.

And so here we are. It isn't elegant, but it seems to do what I wanted it to do.

# Primary functionality of the initial release.

The focus is on integer set membership. The main things supported in the initial release are:

* Populating the set with integers.
* Testing the set for membership.
* Iterating over the set in ascending or descending order if desired.

Hopefully the above actions are reasonably efficient.

The package doesn't presently support any bitmap-smashing types of operations.
Certainly UNION (OR), INTERSECTION (AND), XOR and similar types of behaviours
could be added, but since I didn't need them I haven't added them.

Feel free so submit a PR for this stuff, or you may be interested in the CL-ROARING
package mentioned above and below, which supports a variety of logical bitmap operations.

# Performance

I ran some very simple benchmarks (see `integer-set-testing.lisp`)
of various approaches to integer sets for the element quantity and key ranges 
that I required in my use-case.  The benchmark may or may not run for you, it isn't
something I plan to keep up to date, was just proof of concept stuff for me.

See the lisp file for full details, here's some highlights:

    ;;; Using 1,000,000 intset entries, 20% holes, ranging from 1,000,000 to 2,250,239
    ;;; Duration in seconds, followed by bytes consed (Kilobytes/Megabytes/Gigabytes etc).
    ;;; Lower numbers are better.
    ;;;
    ;;;                                                    Ascending
    ;;; Implementation     Loading           Membership    Traversal
    ;;; =================  ===============   ===========   =============
    ;;; Simple Bit Vector   0.005s,  ~281K   0.003s,  0K   0.008s,    0K 
    ;;; Hash Table          0.032s,   ~24M   0.032s,  0K   0.299s,   ~8M
    ;;; Clustered Intset    0.046s,  ~490K   0.030s,  0K   0.022s, ~161K
    ;;; CL-Roaring (FFI)    0.014s,    N/A   0.024s, N/A   0.011s,   N/A
    ;;; FSET                1.592s,  ~1.5G   0.429s,  0K   0.006s,    0K  
    ;;; CL-intset          50.102s,  ~406G   0.016s,  0K   0.020s,    0K
    ;;;
    ;;; Using 1,000,000 intset entries, 20% holes, ranging from 1,000,000,000 to 1,001,248,861
    ;;;
    ;;;                                                    Ascending
    ;;; Implementation     Loading           Membership    Traversal
    ;;; =================  ===============   ===========   =============
    ;;; Simple Bit Vector   0.010s,  ~125M   0.004s,  0K   2.542s,    0K 
    ;;; Hash Table          0.032s,   ~24M   0.031s,  0K   0.297s,   ~8M
    ;;; Clustered Intset    0.048s,  ~490K   0.029s,  0K   0.022s, ~161K
    ;;; CL-Roaring (FFI)    0.013s,    N/A   0.023s, N/A   0.010s,   N/A
    ;;; FSET                1.535s,  ~1.5G   0.403s,  0K   0.005s,    0K  
    ;;; CL-intset              <cancelled after a couple of hours>

Key takeaways:

* The fixed memory of bit vectors is bad with large integer values.
* The memory of pure hash tables is non-trivial for large numbers of elements, and is 
  not efficient for ordered traversals (at least using standard lisp interfaces).
* CL-ROARING and CLUSTERED-INTSET are not sensitive to range, and have reasonable memory/time
  tradeoffs for large number of values (though we're taking that on faith for CRoaring)
  and ordered traversal.
* FSET has excellent traversal times, but I could not get it to load efficiently, 
  nor do I know how much fixed memory it was consuming. I was probably using it wrong.
* CL-INTSET or any other integer-as-bitmaps representation is not suitable for large values.


# See also

* [FSET](http://www.cliki.net/FSet)
* [CL-INTSET](https://github.com/tkych/cl-intset)
* [CL-ROARING](https://github.com/dtenny/cl-roaring)

I probably did not give FSET a fair shake. I wasn't really looking for
immutable semantics, and by the time I finally tried to use it I'd already written
this package (CLUSTERED-INTSET) and also the CL-ROARING package. In my quick and dirty
benchmark attempt, I also couldn't get FSET to load members efficiently.  Probably all
bad on my part, if there's something I missed let me know.  There's also a question of how
much memory it retains once it's loaded.

CL-INTSET uses single integers as bit sets, but that doesn't scale well to
large keys such as database primary keys, e.g. set member 4,000,000 would
require a four million bit bignum or bit-vector to represent it.  It may have
other scalability problems too, in benchmarking I had to kill the test where I
ran set membership having range starting at one billion.  There are a couple
of packages that use integers as bitmaps, I'm not sure what the attraction of
these implementations is, they are very impractical for large numbers.

In Clojure I've had very good experiences with
[data.int-map](https://github.com/clojure/data.int-map) and its int-set
capabilities.  However aside from being written in a combination of java and
clojure, the packaging is solving about 5 problems at once, most of which are
more useful for Clojure than Common Lisp.  Still, worth a look if you're
seeking inspiration while writing another int-set or related class.

Finally, someone on reddit also suggested using FFI to access the
[roaringbitmap](https://roaringbitmap.org/) compressing bitmap
implementations. In a weird development path, I wrote some of this
CLUSTERED-INTSET package, then decided I didn't have anything to compare it to
for benchmarking, so I wrote the CL-ROARING package, then I returned to this one.

I have to say using FFI for CL-ROARING was really nice and easy. Give it a
look, it provides much more functionality than this lisp-only CLUSTERED-INTSET
implementation, but it requires that you have the CRoaring shared library in
your path, and it's limited to 32-bit unsigned integers, depending on
where/how it was built.

Meanwhile, this package, CLUSTERED-INTSET, is pure lisp, easy to use, works
well for large volumes and large values, and both the memory and CPU behavior
of the package scale well enough with number of values and range of integers,
subject to the limits of CL's hash-table implementation. It certainly
works for my limited use case.
