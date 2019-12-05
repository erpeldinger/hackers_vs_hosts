open Graph

(* Values of an arc : flow and capacity *)
type tarc = {
  flow : int;
  capacity : int;
}

type tecart = {
  flow : int;
  sens : int; (*1 si aller, 0 si retour *)
}

type state = White | Grey | Black


(* Seek a path between two nodes.
    If the path exists, it returns a list of arcs.
    Otherwise, an exception is raised.
*)
val graphe_ecart: (int * int) Graph.graph -> (int * int) Graph.graph
val find_path: (int*int) graph -> id -> id -> (id * id * int) list 

(*
val find_bellman: (int*int) graph -> id -> id -> (id * id * int) list 
val flow_max_cout_min: (int * int) graph -> id -> id -> int
*)

(* Return the flow variation of a path *)
val get_vflow: ('a * 'a * int) list -> int

(* Create a new graph with the given list (cf get_path_dir) and its flow *)
val update_graph: (int*int) graph -> (id * id * int) list -> int -> (int*int)  graph

(* Apply the Ford Fulkerson algorithm to get the max flow between two nodes*)
val get_max_flow: (int * int) graph -> id -> id -> int

