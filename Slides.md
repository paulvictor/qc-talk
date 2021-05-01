%title: Level Up with Property Tests
%author: paul.victor@juspay.in
%date: 01-05-2021

-> Tests <-
=====

Essential for any piece of software

_Kinds:_

\- _*Unit*_            written by the developer to make sure code works as expected
\- Integration     how the code/function/module interacts with its peers
\- Regression      to make sure any code or environment change doesn't break the system

--------------------------------------------------------------------------------------

-> Purpose <-
=======

-> To make software reliable and robust

--------------------------------------------------------------------------------------

-> Purpose <-
=======

-> .. also to `express the way the domain is modelled`
-> and serve as a codified form of software specifications

--------------------------------------------------------------------------------------
-> Unit Tests <-
=======

Unit under test
--------------

~~~ {#sort .haskell .numberLines startFrom="100"}
sort :: [ Int ] -> [ Int ]
sort [] = []
sort [x] = [x]
sort (x:xs) = filter (<x) xs ++ [x] ++ filter (>x) xs
~~~

Test cases
-----------

~~~ {.numberLines}
sort [1,2,3,4,5] == [1,2,3,4,5]
sort [5,4,3,2,1] == [1,2,3,4,5]
sort [2,1]       == [1,2]
sort [1]         == [1]
sort []          == []
~~~

--------------------------------------------------------------------------------------
-> Traditional unit tests <-
=======

Not expressive enough
May not cover all cases
Hard to mention each one
Prone to get unreliable over time (by new developers adding, removing, modifying tests)

---------------------------------------------------------------------------------------
-> Back to our sort example <-
===================

~~~
∀x : x is a list of integers
  ∀(i, j) : 0 < i < j < length(n)
   sort(x)[i] <= sort(x)[j]
~~~
   
~~~
∀x : x is a list of integers
  ∀i : 0 < i < length(n)
    ∃j : 0 < j < length(n)
      sort(x)[i] == x[j]
~~~

~~~
∀x : x is a list of integers
  ∀i : 0 < i < length(n)
    ∃j : 0 < j < length(n)
      sort(x)[j] == x[i]
~~~

~~~
∀x : x is a list of integers
  length sort(x) == length x
~~~

--------------------------------------------------------------------------------------

> _Program testing can be used to show the presence of bugs, but never to show their absence!_

-> _* Edsger W. Dijkstra *_

--------------------------------------------------------------------------------------

-> so what can be the alternative ? <-
=================================

1. Proofs                 Needs a more powerful language to express invariants
2. Equational reasoning   Needs a language which is pure by design.
3. Property tests         Specify properties of a function and ask the system to generate enough test cases *_

----------------------------------------------------------------------------------------

-> _* Property Tests In Action *_

