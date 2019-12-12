
open Graph
open Gfile

(* Values of a arc : flow and capacity *)
type tarc = {
  flow : int;
  capacity : int;
}
(* Return a new graph having the same nodes than gr, but no arc *)
let clone_nodes gr = n_fold gr (new_node) empty_graph;;

(* Map all arcs of gr by function f *)
let gmap gr f = 
  let modif_arc gr id1 id2 a = new_arc gr id1 id2 (f a) in
  let new_gr = clone_nodes gr in
  e_fold gr modif_arc new_gr ;;

(* Add n to the value of the arc between id1 and id2. If the arc does not exist, it is created *)
let add_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> new_arc gr id1 id2 n
    | Some x -> new_arc gr id1 id2 (n+x)
  with
    e -> raise e;;

(* Add n to the value of a flow of a tarc between id1 and id2. If the arc does not exist, it is created *)
let add_tarc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "t_arc inexistant"
    | Some (flow, capa) -> new_arc gr id1 id2 (flow+n,capa)
  with
    e -> raise e;;

(* Add n to the flow of a tecart arc between id1 and id2. If the arc does not exist, it is created. 
 * A tecart arc is an arc used in a residual graph, it contains a flow and the direction of the arc. 
 * 0 represents an incoming arc
 * 1 represents an outgoing arc
 *)
let add_tecart_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "t_earc inexistant"
    | Some (flow, sens) -> new_arc gr id1 id2 (flow+n,sens)
  with
    e -> raise e;;

(* Same as add_tecart except for it substracts n*)
let sub_tecart_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "t_earc inexistant"
    | Some (flow, sens) -> new_arc gr id1 id2 (flow-n,sens)
  with
    e -> raise e;;

(* Add n to the flow of an arc between id2 and id2, used in the max flow/min cost algorithm.
 * This kind of arc is caracterized by a flow, a capcity and a cost.
 *)
let add_mfmc_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "[add_arc] mfcm_arc inexistant"
    | Some (flow, capa, cout) -> new_arc gr id1 id2 (flow+n,capa,cout)
  with
    e -> raise e;;
    
(* Same as add_mfmc_arc except for it substracts n to the flow *)
let sub_mfmc_arc gr id1 id2 n =
  try 
    let res = find_arc gr id1 id2 in
    match res with
    | None -> failwith "[sub arc] mfcm_arc inexistant"
    | Some (flow, capa, cout) -> new_arc gr id1 id2 (flow-n,capa,cout)
  with
    e -> raise e;;