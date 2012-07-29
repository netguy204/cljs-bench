cljs-bench
==========

Track how the performance of Clojurescript changes with each commit
to the repository.

[See the plots](http://50ply.com/cljs-bench/)

Usage
-----

Make sure you have at least one of these environment variables
defined:

* V8_HOME (location of the d8 binary)
* SPIDERMONKEY_HOME (location of the js binary)
* JSC_HOME (location of the jsc binary)

Also, make sure you have a recent version of Gnuplot on your PATH.

This assumes you already have a working Clojurescript that was cloned
from git. Make sure you've run the clojurescript bootstrap script
(script/bootstrap) to download Clojure and the Closure
compiler. Create a file named last-head inside your copy of cljs-bench
and put the SHA1 of the earliest version you care about in this file.

Then, if using leiningen:

lein run ~/src/clojurescript/ report

This will check out and run the benchmarks in every version of
clojurescript between the version you mentioned and origin/master of
the local clojurescript repository you pointed cljs-bench at. When its
done it will update the last-head file to point at the current
location of origin/master. To perform incremental updates, just fetch
into your local clojurescript repo and run the above command again.

License
-------

Copyright (C) 2012 Brian Taylor

Distributed under the Eclipse Public License, the same as Clojure.
