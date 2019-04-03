(*
                          CS51 Problem Set 7
                       Refs, Streams, and Music
                   Part 1: Mutable Lists and Cycles
 *)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref ;;

                                          
(*......................................................................
Problem 1: Write a function has_cycle that returns whether a mutable
list has a cycle. You may want a recursive helper function. Don't
worry about space usage.
......................................................................*)
                                      
let has_cycle (lst : 'a mlist) : bool =
  failwith "has_cycle not implemented"

(*......................................................................
Problem 2: Write a function flatten that flattens a list (removes its
cycles if it has any) destructively. Again, you may want a recursive
helper function and you shouldn't worry about space.
......................................................................*)

let flatten (lst : 'a mlist) : unit =
  failwith "flatten not implemented"

(*......................................................................
Problem 3: Write mlength, which nondestructively finds the number of
nodes in a mutable list that may have cycles.
......................................................................*)
let mlength (lst : 'a mlist) : int =
  failwith "mlength not implemented"

(*====================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignment
......................................................................*)

let minutes_spent_on_part () : int = failwith "not provided" ;;
