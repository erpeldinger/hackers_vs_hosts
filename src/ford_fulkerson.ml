open Tools
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

(* Initialization of states with White *)
let nodes_state gr =
  let rec recup_node acu id = match (node_exists gr id) with
    | false -> acu
    | true -> recup_node ( (id, White) :: acu) (id+1)
  in 
  List.rev (recup_node [] 0)

(* Copy the given list and modify the state of s with the given color *)
let change_color l s color = 
  let rec loop acu l s = match l with
    | [] -> acu
    | (id, col) :: rest-> if (id==s) then loop ((id, color) :: acu) rest s
      else loop ((id, col) :: acu) rest s 
  in 
  List.rev (loop [] l s)

(* ----------------------- Functions related to the residual graph ----------------------------------*)

(* Create a graph containing forward arcs *)
let arcs_1 gr = gmap gr (fun (flow, capa) -> (capa-flow,1))
(* Create a graph containing backward arcs *)
let arcs_2 gr = 
  let modif_arc gr id1 id2 (x,y) = new_arc gr id2 id1 ((x,0)) in
  let new_gr = clone_nodes gr in
  e_fold gr modif_arc new_gr ;;

(* Return the residual graph *)
let graphe_ecart gr = 
  let gr_1 = arcs_1 gr and gr_2 = arcs_2 gr in
  e_fold gr_1 new_arc gr_2

(* ----------------------- [END of functions related to the residual graph] ----------------------- *)

(* Return a path (list of id) without the state of its nodes *)
let recup_chemin l s p =
  let rec aux acu l = match l with
    | [] -> acu
    | (id, Grey) :: rest -> aux (id :: acu) rest
    | (id, _) :: rest -> aux acu rest
  in 
  List.rev (aux [] l)

(* Return a path (list of id1*id2*flow) *)
let recup_flow_chemin gr l =
  let rec recup acu l = match l with
    | [] -> acu
    | [id] -> acu
    | [id1; id2] -> (match (find_arc gr id1 id2) with
        | None -> failwith "arc not found"
        | Some (flow,_) -> (id1, id2, flow) :: acu)
    | id1 :: id2 :: rest -> match (find_arc gr id1 id2) with
      | None -> failwith "arc not found"
      | Some (flow,_) -> recup ( (id1, id2, flow) :: acu) (id2 ::rest)
  in
  List.rev (recup [] l)

(* Return the id of the last grey node in the given list (list of previous nodes) *)
let recup_antecedant l1 l2 = 
  let rec pas_noir acu l_ant = match l_ant with
    | [] -> acu
    | id :: rest -> if not((List.assoc id l2) == Black) then pas_noir (id :: acu) rest else pas_noir acu rest
  in
  let rec aux l = match l with
    | [] -> (-1)
    | id :: [] -> id
    | id :: rest -> aux rest
  in
  aux (List.rev (pas_noir [] l1) )

(* Return an orderly path from two list (states list and list of previous nodes) *)
let ordre_chemin l_states l_ant p = 
  let rec aux l acu = match l with
    | [] -> acu
    | id :: rest -> if ((List.assoc id l_states)==Grey) && not(List.mem id acu) then aux rest (id::acu) else aux rest acu
  in
  List.rev (p :: (aux l_ant []))

(* --------------------------------- Print functions ---------------------------------------*)

let mon_match color = match color with
  |Black -> "Black"
  |Grey->  "Grey"
  |White -> "White"

let print_state states = 
  let rec aux l = match l with
    | [] -> Printf.printf"\n%!"
    | (id1, color) :: rest -> let () = Printf.printf" %d %s" id1 (mon_match color) in aux rest
  in
  aux states

let print_idid_l chemin =
  let rec aux l = match l with
    | [] -> Printf.printf"\n %!"
    | (id , id2) :: rest -> let () = Printf.printf" cout %d id %d \n%!" id id2 in aux rest
  in
  aux chemin

let print_nodes chemin =
  let rec aux l = match l with
    | [] -> Printf.printf"\n %!"
    | id :: rest -> let () = Printf.printf" %d " id in aux rest
  in
  aux chemin

let print_res_find_path chemin =
  let rec aux l = match l with
    | [] -> Printf.printf" \n%!"
    | (id1,id2,a) :: rest -> let () =Printf.printf" id1 : %d | id2 : %d | cout : %d \n%!" id1 id2 a in aux rest
  in
  aux chemin

(*---------------------------- FORD FULKERSON ALGORITHM ------------------------------------------- *)

(* Find a path from n1 to n2 in the given residual graph, return [(-1,-1,0)] if there is no path *)
let find_path gr n1 n2 =
  let l_state = nodes_state gr in
  (* Function that returns two lists : the final states' list and the list of previous nodes *)
  let rec voisins s vois les_states l_antecedant = 
    let states = change_color les_states s Grey in
    match vois with
    | [] -> if s==n2 then (change_color states s Grey, List.rev (s::(List.rev l_antecedant))) else (
        let l_state_modif=change_color states s Black in
        let antecedant=recup_antecedant l_antecedant l_state_modif in
        if (antecedant >=0) then voisins antecedant (out_arcs gr antecedant) l_state_modif (List.rev (s :: (List.rev l_antecedant)))
        else ([(-1,Black)], []) (* No path found *)
      )
    | (id,(flow,1)) :: rest -> if (flow>0) && (id==n2) then ((change_color states n2 Grey), List.rev (s::(List.rev l_antecedant))) else  (* verification de la terminaison *)
      (if (flow>0) && (List.assoc id states)==White then voisins id (out_arcs gr id) states (List.rev (s :: (List.rev l_antecedant)))
      else voisins s rest states l_antecedant)
    | (id,(flow,0)) :: rest -> if flow>0 then
                              (match (find_arc gr id s) with
                              | None -> failwith "erreur graphe ecart : arc dans un seul sens"
                              | Some (flow, sens) -> if (flow>0) && (id==n2) then (change_color states n2 Grey, List.rev (s::(List.rev l_antecedant))) else  (* verification de la terminaison *)
                                    (if (flow>0) && (List.assoc id states)==White then voisins id (out_arcs gr id) states (List.rev (s :: (List.rev l_antecedant)))
                                    else voisins s rest states l_antecedant)
                              ) 
                              else voisins s rest states l_antecedant
    | (id,(flow,a)) :: rest -> failwith ("erreur lbl find path"^(string_of_int a))
  in
  let (state_final, l_ant) = voisins n1 (out_arcs (gr) n1) l_state [] in
  if state_final==[(-1,Black)] then [(-1,-1,0)] (* No path found *)
  else 
    let chemin_ordonne = ordre_chemin state_final l_ant n2 in
    recup_flow_chemin gr chemin_ordonne

(* Get the min flow of a path *)
let get_vflow path = 
  let find_min v1 v2 = if v1 < v2 then v1 else v2 in
  let rec aux chemin flow = match chemin with
    | [] -> flow
    | (_, _,n) :: rest -> aux rest (find_min n flow)
  in 
  aux path max_int;;

(* Modify two arcs in the given residual graph : add v to the id1->id2 arc and substract v to the id2->id1 arc *)
let modifier_2arcs gr id1 id2 v = sub_tecart_arc (add_tecart_arc gr id1 id2 v) id2 id1 (v)

(* Update the given residual graph according to the given residual flow *)
let update_graph gr path v = 
  let rec update_path gr1 path = match path with
    | [] -> gr1
    | (id1, id2, flow) :: rest -> 
      match (find_arc gr id1 id2) with
      | None -> failwith "Probleme l'arc n'existe pas"
      | Some (flow, sens) -> match (flow,sens) with
        | (flow, 0) -> if (flow-v >=0)
          then update_path (modifier_2arcs gr1 id1 id2 v) rest
          else failwith "erreur valeur negative si soustraction (retour)"
        | (_, 1) -> if (flow-v)>=0 then update_path (modifier_2arcs gr1 id2 id1 v) rest
        else failwith "erreur valeur negative si soustraction (aller) flow et v "
        | (_,a) -> failwith ("erreur format lbl gr ecart "^(string_of_int a))
  in 
  update_path gr path;;

(* Ford Fulkerson Algorithm *)
let get_max_flow gr s p = 
  (* Initialize all flow in the flow graph with 0 *)
  let init_flow = gmap gr (fun (flow,capacity) -> (0,capacity)) in
  let gr_ecart = graphe_ecart init_flow
  in 
  (* While a path exists *)
  let rec loop gr2 debit = match (find_path gr2 s p) with
    | [(-1,-1,0)] -> (debit, gr2)
    | res -> let var_flow = get_vflow res in  
      loop (update_graph gr2 res var_flow) (debit + var_flow) 
  in
  loop gr_ecart 0


(* ---------------------- MAX FLOW/MIN COST - MOORE-BELLMAND-FORD ALGORITHM -------------------------*)


(* Update a flow graph *)
let update_graph_BF gr path v = 
  let rec update_path gr1 path = match path with
    | [] -> gr1
    | (id1, id2, _) :: rest -> 
      match (find_arc gr id1 id2) with
      | None -> update_path (sub_mfmc_arc gr1 id2 id1 v) rest
      | Some (_, _, _) -> update_path (add_mfmc_arc gr1 id1 id2 v) rest
  in 
  update_path gr path

(* -------------------------- Functions related to the residual graph -------------------------- *)

(*  Forward arcs, if flow /= capacity, its value is "+cost", otherwise its value is max int *)
let arcs1_MFCM gr = gmap gr (fun (flow, capa, cout) -> if not(flow==capa) then (cout,1) else (max_int,1) )

(* Backward arcs, if flow /=0, its value is "-cost ", otherwise its value is max_int *)
let arcs2_MFCM gr = 
  let modif_arc gr id1 id2 (flow,capa,cout) = new_arc gr id2 id1 ( if not(flow==0) then ((-cout), 0) else (max_int,0)) in
  let new_gr = clone_nodes gr in
  e_fold gr modif_arc new_gr ;;

(* Return the residual graph *)
let gr_ecart_MFCM gr = 
  let gr_1 = arcs1_MFCM gr and gr_2 = arcs2_MFCM gr in
  e_fold gr_1 new_arc gr_2 

(* --------------------------------- [ END of functions related to the residual graph ] ---------------------------------------*)

(* Return a list filled with the predecessors of the given node *)
let get_pred gr id = 
  let rec aux iter acu = match (node_exists gr iter) with
    | false -> acu
    | true -> if not((find_arc gr iter id)==None) then (aux (iter + 1) (iter::acu))else (aux (iter +1) acu)
  in 
  aux 0 []

(* Initialization of the Bellman-Ford algorithm : return a list of costs *)
let init_bellman gr s =
  let rec loop iter acu = match (node_exists gr iter) with
    | false -> acu
    | true -> if iter==s then loop (iter+1) ((0,-1)::acu) else loop (iter+1) ((max_int,0)::acu)
  in 
  List.rev (loop 0 [])

(* Return the cost in a list of tuple (cost,id) *)
let get_cout l id = 
  let rec aux l iter = match l with 
    | [] -> failwith "[get_cout] Sommet inexistant dans la liste"
    | (c, _) :: rest -> if id==iter then c else aux rest (iter+1)
    in 
  aux l 0

(* Return the value of an arc *)
let get_val_arc gr x y = match find_arc gr x y with
  | None -> failwith "[get_val_arc] Arc inexistant"
  | Some (cout, sens) -> cout

(* Copy the given list of costs and modify the column x with (pred, new_cout) *)
let change_cost lcout x pred new_cout =
  let rec aux l iter acu = match l with
    | [] -> acu
    | (cost, id) :: rest -> if iter==x then aux rest (iter+1) ((new_cout, pred)::acu)
                            else aux rest (iter + 1) ((cost,id)::acu)
  in 
  List.rev (aux lcout 0 [])

(* For all predecessors y of x, update the cost of x
and return a boolean (cont) that is true if there has been at least a change in the list of costs 
and the list of costs *)
let maj_all_pred gr lcout x lpred =
  let rec aux l lcost cont= match l with
    | [] -> (lcost, cont)
    | y :: rest -> let cout_x = get_cout lcost x in 
                  let cout_y = get_cout lcost y in
                  let cout_arc_xy = get_val_arc gr y x in
                  if not(cout_arc_xy=max_int) then
                    if not(cout_y ==max_int) then
                      let new_cout = cout_y + cout_arc_xy in 
                      if cout_x > new_cout then 
                        let new_lcout = change_cost lcost x y new_cout in 
                        aux rest new_lcout true
                    else aux rest lcost cont
                  else aux rest lcost cont
                  else aux rest lcost cont
  in 
  let (res,cont) = aux lpred lcout false in
  (res,cont)

(* For all nodes x (except the source s), apply the "maj_all_pred" function 
and return a boolean (cont) and the list of costs *)
let for_all_sommets gr lcout s = 
  let rec aux iter l continuer = 
    if node_exists gr iter then
        if iter==s then aux (iter+1) l continuer
        else let (new_lcout,cont)=maj_all_pred gr l iter (get_pred gr iter) in 
              if cont then aux (iter+1) new_lcout true
              else aux (iter+1) new_lcout continuer
    else (l, continuer)
  in
  aux 0 lcout false

(* Return the id of the given node in a list of tuple (cost,id) *)
let get_id lcout id = 
  let rec aux l iter = match l with
    | [] -> failwith ("[get_id] error"^(string_of_int iter))
    | (cout, id1) :: rest -> if (iter==id) then id1 else aux rest (iter+1)

  in 
  aux lcout 0

(* Return the path (list of nodes) *)
let recup_chemin_bf l s p = 
  let rec aux id_courant acu = 
    if id_courant==s then acu else let next_id = get_id l id_courant in aux next_id (next_id :: acu)
  in 
  aux p [p]

(* Return true if the given path is valid (the cost is not max_int or more) *)
let chemin_valide lcout path = 
  let rec aux validite l = match l with
    | [] -> validite
    | id :: rest -> match List.nth lcout id with 
        | (cout, _) -> if cout == max_int then false else aux validite rest
  in 
  aux true path 

(* Application of the Bellman-Ford algorithm to find a path, based on the flow graph *)
let find_bellman gr s p = 
  let gr_ecart = gr_ecart_MFCM gr in
  let liste_cout = init_bellman gr_ecart s in
  let rec aux l = match for_all_sommets gr_ecart l s with
    | (res, false) -> res
    | (res, true) -> aux res
  in 
  let lcost_final = aux liste_cout in 
  let chemin_id = recup_chemin_bf lcost_final s p in

  if not(chemin_valide lcost_final chemin_id) then [(-1,-1,0)] 
  else 
  let flow_chemin = recup_flow_chemin gr_ecart chemin_id in
  flow_chemin

(* Return the cost of a path *)
let get_cout_chemin gr res = 
  let rec aux acu l = match l with 
  | [] -> acu
  | (id1, id2, cout) :: rest -> match (find_arc gr id1 id2) with
        | None -> begin
           match (find_arc gr id2 id1) with 
            | None -> failwith "[get_vflow_bf] error arc inexistant"
            | Some (_, _, cout) -> aux (acu+cout) rest
          end
    | Some (_, _, cout) -> aux (acu+cout) rest 
  in 
  aux 0 res

(* Find the min element inside a list *)
let find_min liste = 
  let rec aux min l = match l with
    | [] -> min
    | flow :: rest -> if min>flow then aux flow rest else aux min rest
  in 
  aux max_int liste

(* Get the flow variation *)
let get_vflow_bf gr chemin = 
  let rec aux acu l = match l with
    | [] -> acu
    | (id1, id2, cout) :: rest -> match (find_arc gr id1 id2) with
        | None -> begin
           match (find_arc gr id2 id1) with 
            | None -> failwith "[get_vflow_bf] error arc inexistant"
            | Some (flow, capa, cout) -> aux (flow::acu) rest
          end
        | Some (flow, capa, cout) -> aux ((capa-flow)::acu) rest
  in 
  let liste_flow = aux [] chemin in 
  find_min liste_flow

(* convert flow, capa and cost to string *)
let string_of_mfcm (flow, capa, cout) = (string_of_int flow) ^ "/" ^ (string_of_int capa) ^ "/" ^ (string_of_int cout)

(* Find the max flow/min cost*)
let flow_max_cout_min gr s p = 
  let rec loop index gr2 debit cout = 
  (*Gfile.export ("test"^(string_of_int index)) (gmap gr2 string_of_mfcm);*)
  match (find_bellman gr2 s p) with
      | [(-1,-1,0)] -> (debit, cout, gr2)
      | res -> let var_flow = get_vflow_bf gr2 res in 
      let new_cost = (get_cout_chemin gr2 res) *var_flow in
      loop (index+1) (update_graph_BF gr2 res var_flow) (debit + var_flow) (cout+new_cost)
  in
  loop 0 gr 0 0