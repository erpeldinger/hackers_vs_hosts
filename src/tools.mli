(*------------------------- tools.mli----------------------------------*)

open Graph
open Gfile

(* Values of an arc : flow and capacity *)
type tarc = {
  flow : int;
  capacity : int;
}

(* Return a new graph having the same nodes than gr, but no arc *)
val clone_nodes: 'a graph -> 'b graph

(* Map all arcs of gr by function f *)
val gmap: 'a graph -> ('a -> 'b) -> 'b graph

(* Add n to the value of the arc between id1 and id2. If the arc does not exist, it is created *)
val add_arc: int graph -> id -> id -> int -> int graph

(* Add n to the value of a flow of a tarc between id1 and id2. If the arc does not exist, it is created *)
val add_tarc: (int * 'a) graph -> id -> id -> int -> (int * 'a) graph

(* Add n to the flow of a tecart arc between id1 and id2. If the arc does not exist, it is created. 
 * A tecart arc is an arc used in a residual graph, it contains a flow and the direction of the arc. 
 * 0 represents an incoming arc
 * 1 represents an outgoing arc
 *)
val add_tecart_arc: (int * 'a) graph -> id -> id -> int -> (int * 'a) graph

(* Same as add_tecart except for it substracts n*)
val sub_tecart_arc: (int * 'a) graph -> id -> id -> int -> (int * 'a) graph

(* Add n to the flow of an arc between id2 and id2, used in the max flow/min cost algorithm.
 * This kind of arc is caracterized by a flow, a capcity and a cost.
 *)
val add_mfmc_arc: (int * int * 'a) graph -> id -> id -> int -> (int * int * 'a) graph

(* Same as add_mfmc_arc except for it substracts n to the flow *)
val sub_mfmc_arc: (int * int * 'a) graph -> id -> id -> int -> (int * int * 'a) graph