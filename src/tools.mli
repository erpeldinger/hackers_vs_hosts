(*------------------------- tools.mli----------------------------------*)

open Graph
open Gfile

(* Values of an arc : flow and capacity *)
type tarc = {
  flow : int;
  capacity : int;
}

val clone_nodes: 'a graph -> 'b graph

val gmap: 'a graph -> ('a -> 'b) -> 'b graph

val add_arc: int graph -> id -> id -> int -> int graph

val add_tarc: (int * 'a) graph -> id -> id -> int -> (int * 'a) graph

val add_tecart_arc: (int * 'a) graph -> id -> id -> int -> (int * 'a) graph

val sub_tecart_arc: (int * 'a) graph -> id -> id -> int -> (int * 'a) graph