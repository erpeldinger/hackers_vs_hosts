open Graph

(* Values of an arc : flow and capacity *)
type tarc = {
  flow : int;
  capacity : int;
}

type tecart = {
  flow : int;
  sens : int; (*1 forward arc, 0 backward arc *)
}

type state = White | Grey | Black


(* Use of the Bellman-Ford algorithm to get the max flow with a minimal cost*)
val flow_max_cout_min: (int * int * int) graph -> id -> id -> int*int* (int * int * int) graph

(* Apply the Ford Fulkerson algorithm to get the max flow one between two nodes*)
val get_max_flow: (int * int) graph -> id -> id -> int * (int * int) graph 

