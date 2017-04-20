---
title: Homework #4, Due Sunday, Dec 11, 2016 (23:59:59 PST)
---



\begin{code} 
module Hw4 where
\end{code}

Instructions
------------

To complete this homework,

1. **Download** the [virtual machine](http://goto.ucsd.edu/~gridaphobe/cse-230.ova)
   The VM contains everything you need to complete HW4.

2. **Open** the `hw4` folder on the desktop (or `tar -zxvf hw4.tar.gz`) 
   or download [Hw4.tar.gz](static/Hw4.tar.gz) 
   to see files for each part:

    + [Inference](Inference.html)
    + [Assertions](Assert.html)
    + [Refined Lists](List.html)
    + [MapReduce](MapReduce.html)
    + [KMeans Clustering](KMeans.html)
    + [Interpreter](Interpreter.html)

3. **Fill in** the code where noted.

4. **Package** the files into `Hw4.tar.gz` by running `make package`.

5. **Submit** by emailing `Hw4.tar.gz` to `cse230@goto.ucsd.edu` with the subject "Hw4"; 
   you will receive a confirmation email after submitting.

**Notes**

+ **Your code must typecheck against the given type signatures.**

+ Please post questions to Piazza.

**Snapshots**

The VM has a custom build of Liquid Haskell that snapshots each file
you verify. We will use these snapshots to *improve* Liquid Haskell;
the submission and snapshots *will not affect your grade*; the grade
depends *only* on the final code submitted.

**Emacs**

We have also configured emacs to automatically run Liquid Haskell as you
work. If you're not already comfortable using emacs, feel free to run
Liquid Haskell from the command-line, by:

~~~~~{.sh}
$ liquid file.lhs
~~~~~

Problem 1: Type Inference 
--------------------------

This problem is simply about extending the type inference
algorithm we saw in lecture; there are no refinement types
involved.

+ [code](final/Inference.lhs)
+ [html](final/Inference.html)


Problem 2: Refined Lists 
------------------------

+ [code](List.lhs)
+ [html](List.html)

Problem 3: MapReduce
--------------------

+ [code](MapReduce.lhs)
+ [html](MapReduce.html)

Problem 4: KMeans Clustering
----------------------------

+ [code](KMeans.lhs)
+ [html](KMeans.html)

Problem 5: Interpreter
----------------------

+ [code](Interpreter.lhs)
+ [html](Interpreter.html)




