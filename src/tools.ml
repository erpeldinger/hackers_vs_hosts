(*---------------------- tools.ml ------------------------------------*)
open Graph
open Gfile

(* Values of an arc : flow and capacity *)
type tarc = {
  flow : int;
  capacity : int;
}
(* Returns a new graph having the same nodes than gr, but no arc *)
(* 'a graph -> 'b graph *)
let clone_nodes gr = n_fold gr (new_node) empty_graph;;

(* Maps all arcs of gr by function f *)
(* 'a graph -> ('a -> 'b) -> 'b graph *)
let gmap gr f = 
  let modif_arc gr id1 id2 a = new_arc gr id1 id2 (f a) in
  let new_gr = clone_nodes gr in
  e_fold gr modif_arc new_gr ;;

(* Adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created *)
(* int graph -> id -> id -> int -> int graph *)
let add_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> new_arc gr id1 id2 n
    | Some x -> new_arc gr id1 id2 (n+x)
  with
    e -> raise e;;


let add_tarc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "t_arc inexistant"
    | Some (flow, capa) -> new_arc gr id1 id2 (flow+n,capa)
  with
    e -> raise e;;

let add_tecart_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "t_earc inexistant"
    | Some (flow, sens) -> new_arc gr id1 id2 (flow+n,sens)
  with
    e -> raise e;;

let sub_tecart_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "t_earc inexistant"
    | Some (flow, sens) -> new_arc gr id1 id2 (flow-n,sens)
  with
    e -> raise e;;

(* max flow min cost *)
let add_mfmc_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "[add_arc] mfcm_arc inexistant"
    | Some (flow, capa, cout) -> new_arc gr id1 id2 (flow+n,capa,cout)
  with
    e -> raise e;;
    
(* max flow min cost *)
let sub_mfmc_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "[sub arc] mfcm_arc inexistant"
    | Some (flow, capa, cout) -> new_arc gr id1 id2 (flow-n,capa,cout)
  with
    e -> raise e;;