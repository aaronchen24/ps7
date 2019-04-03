(*
                          CS51 Problem Set 7
                       Refs, Streams, and Music
                       Part 2: Lazy Evaluation
 *)

(*======================================================================
Section 2.1: Series acceleration with infinite streams

In nativeLazyStreams.ml, we provide the definitions of lazy streams
using OCaml's native Lazy module as presented in lecture, up to and
including code for approximating pi through partial sums of the terms
in a Taylor series. In the next problem, you'll use streams to find
faster approximations for pi.

Recall from the reading the use of streams to generate approximations
of pi of whatever accuracy. Try it. You should be able to reproduce
the following:

   # within 0.01 pi_sums ;;
   - : int * float = (199, 3.13659268483881615)
   # within 0.001 pi_sums ;;
   - : int * float = (1999, 3.14109265362104129)
   # within 0.0001 pi_sums ;;
   - : int * float = (19999, 3.14154265358982476)

Notice that it takes about 2000 terms in the Taylor series to get
within .001 of the value of pi. This method converges quite
slowly. But we can increase the speed dramatically by averaging
adjacent elements in the approximation stream.
......................................................................*)

open NativeLazyStreams ;;

(*......................................................................
Problem 5: Implementing average on streams

Write a function average that takes a float stream and returns a
stream of floats each of which is the average of adjacent values in
the input stream. For example:

# first 5 (average (to_float nats)) ;;
- : float list = [0.5; 1.5; 2.5; 3.5; 4.5]
......................................................................*)
  
let average (s : float stream) : float stream =
  failwith "average not implemented" ;;

(* Now instead of using the stream of approximations in pi_sums, you
can instead use the stream of averaged pi_sums, which converges much
more quickly. Test that it requires far fewer steps to get within,
say, 0.001 of pi. You'll want to record your results below for Problem
7. *)
   
(*......................................................................
Problem 6: Implementing Aitken's method

An even better accelerator of convergence for series of this sort
is Aitken's method. The formula is given in the problem set
writeup. Write a function to apply this accelerator to a stream, and
use it to generate approximations of pi.
......................................................................*)
   
let aitken (s: float stream) : float stream =
  failwith "aitken not implemented" ;;

(*......................................................................
Problem 7: Testing the acceleration

Fill out the following table, recording how many steps are needed to
get within different epsilons of pi.

    ---------------------------------------------------------
    epsilon  |  pi_sums  |  averaged method  |  aitken method 
    ---------------------------------------------------------
    0.1      |           |                   |
    ---------------------------------------------------------
    0.01     |           |                   |
    ---------------------------------------------------------
    0.001    |           |                   |
    ---------------------------------------------------------
    0.0001   |           |                   |
    ---------------------------------------------------------
......................................................................*)

(*======================================================================
Section 2.2 : Infinite trees

Just as streams are a lazy form of list, we can have a lazy form of
trees. In the definition below, each node in a lazy tree of type 'a
tree holds a value of some type 'a, and a (conventional, finite) list
of one or more (lazy) child trees. Complete the implementation by
writing print_depth, tmap, tmap2, and bfenumerate.  We recommend
implementing them in that order.
......................................................................*)
   
type 'a treeval = Node of 'a * 'a tree list
 and 'a tree = 'a treeval Lazy.t ;;

(* Infinite trees shouldn't have zero children. This exception is
available to raise in case that eventuality comes up. *)

exception Finite_tree ;;

(*......................................................................
Problem 8: Implement the following functions for manipulating
infinite trees. *)

(*......................................................................
node t -- Returns the element of type 'a stored at the root node of
tree t of type 'a tree.
......................................................................*)
  
let node (t : 'a tree) : 'a =
  failwith "node not implemented" ;;

(*......................................................................
children t -- Returns the list of children of the root node of tree t.
......................................................................*)
   
let children (t : 'a tree) : 'a tree list =
  failwith "children not implemented" ;;

(*......................................................................
print_depth n indent t -- Prints a representation of the first n
levels of the tree t indented indent spaces. You can see some examples
of the intended output of print_depth below.
......................................................................*)
   
let print_depth (n : int) (indent : int) (t : int tree) : unit =
  failwith "print_depth not implemented" ;;

(*......................................................................
tmap f t -- Returns a tree obtained by mapping the function f over
each node in t.
......................................................................*)
   
let tmap (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  failwith "tmap not implemented" ;;

(*......................................................................
tmap2 f t1 t2 -- Returns the tree obtained by applying the function f
to corresponding nodes in t1 and t2, which must have the same
"shape". If they don't, an Invalid_argument exception is raised.
......................................................................*)
   
let tmap2 (f : 'a -> 'b -> 'c)
          (t1 : 'a tree) (t2 : 'b tree)
        : 'c tree =
  failwith "tmap2 not implemented" ;;

(*......................................................................
bfenumerate tslist -- Returns a LazyStreams.stream of the nodes in the
list of trees tslist enumerated in breadth-first order, that is, the
root nodes of each of the trees, then the level one nodes, and so
forth. There is an example of bfenumerate being applied below. If
there isn't an infinite set of nodes in the list of trees (think about
how that could come about), raise a Finite_tree exception.
......................................................................*)
   
let bfenumerate (tslist : 'a tree list) : 'a stream =
  failwith "bfenumerate not implemented" ;;

(* Now use your implementation to generate some interesting infinite
trees.  Hint: Drawing a tree considering how the values change along
each branch will yield helpful intuition for the next problems. *)

(*......................................................................
onest -- An infinite binary tree all of whose nodes hold the integer 1.
......................................................................*)
   
let onest : int tree =
  lazy (failwith "onest not implemented") ;;

(*......................................................................
levels n -- Returns an infinite binary tree where the value of each
node in the tree is its level or depth in the tree, starting with the
argument n. For example:

# print_depth 2 0 (levels 0) ;;
0...
 1...
  2...
  2...
 1...
  2...
  2...
- : unit = ()
......................................................................*)
   
let levels (n : int) : int tree =
  failwith "levels not implemented" ;;

(*......................................................................
tree_nats -- An infinite binary tree where the value of each
node in the tree is consecutively numbered in breadth-first order
starting with 0. For example:

# print_depth 2 0 tree_nats ;;
0...
 1...
  3...
  4...
 2...
  5...
  6...
- : unit = ()
# first 10 (bfenumerate [tree_nats]) ;;
- : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
......................................................................*)
   
let tree_nats : int tree =
  lazy (failwith "tree_nats not implemented") ;;
                                                 
(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
