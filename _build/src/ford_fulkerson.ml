
(* Note : s correpond au noeud source et p au noeud puits *)
open Tools
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

(* Initialise tous les etats à White, renvoie la liste des etats *)
let nodes_state gr =
  let rec recup_node acu id = match (node_exists gr id) with
    | false -> acu
    | true -> recup_node ( (id, White) :: acu) (id+1)
  in 
  List.rev (recup_node [] 0)

(* Change l'état d'un id dans la liste des etats, renvoie la nouvelle liste*)
let change_color l s color = 
  let rec loop acu l s = match l with
    | [] -> acu
    | (id, col) :: rest-> if (id==s) then loop ((id, color) :: acu) rest s
      else loop ((id, col) :: acu) rest s 
  in 
  List.rev (loop [] l s)

(* Fonctions auxiliaires de Graphe Ecart *)
(*arcs_1 représente les arcs qui sont modifié pour créer le graphe d'écart *)
let arcs_1 gr = gmap gr (fun (flow, capa) -> (capa-flow,1))
(* arcs_2 représente les arcs qui sont ajouté pour créer le graphe d'écart *)(* (flow, capa) -> flow) *)
let arcs_2 gr = 
  let modif_arc gr id1 id2 (x,y) = new_arc gr id2 id1 ((x,0)) in
  let new_gr = clone_nodes gr in
  e_fold gr modif_arc new_gr ;;

(*renvoie le graphe d'écart de gr*)
let graphe_ecart gr = 
  let gr_1 = arcs_1 gr and gr_2 = arcs_2 gr in
  e_fold gr_1 new_arc gr_2

(* recuperation d'un chemin (enleve les couleurs)*)
let recup_chemin l s p =
  let rec aux acu l = match l with
    | [] -> acu
    | (id, Grey) :: rest -> aux (id :: acu) rest
    | (id, _) :: rest -> aux acu rest
  in 
  List.rev (aux [] l)

let print_res_find_path chemin =
  let rec aux l = match l with
    | [] -> Printf.printf" \n%!"
    | (id1,id2,a) :: rest -> let a =Printf.printf" id1 : %d | id2 : %d | flow : %d \n%!" id1 id2 a in aux rest
  in
  aux chemin

(* prend un chemin composé d'id et renvoie le chemin compose de (id id flow) *)
let recup_flow_chemin gr l =
  let rec recup acu l = match l with
    | [] -> acu
    | [id] -> acu
    | [id1; id2] -> (match (find_arc gr id1 id2) with
        | None -> failwith "arc not found"
        | Some (flow,_) -> (id1, id2, flow) :: acu)
    | id1 :: id2 :: rest -> match (find_arc gr id1 id2) with
      | None -> let a = print_res_find_path acu in failwith "arc not found"
      | Some (flow,_) -> recup ( (id1, id2, flow) :: acu) (id2 ::rest)
  in
  List.rev (recup [] l)

(* recupere le dernier id antecedant dans la liste des antecedants *)
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

(* PB DANS CETTE FONCTION *)
let ordre_chemin l_states l_ant p = 
  let rec aux l acu = match l with
    | [] -> acu
    | id :: rest -> if ((List.assoc id l_states)==Grey) && not(List.mem id acu) then aux rest (id::acu) else aux rest acu
  in
  List.rev (p :: (aux l_ant []))

let mon_match color = match color with
  |Black -> "Black"
  |Grey->  "Grey"
  |White -> "White"

let print_state states = 
  let rec aux l = match l with
    | [] -> Printf.printf"\n%!"
    | (id1, color) :: rest -> let a = Printf.printf" %d %s" id1 (mon_match color) in aux rest
  in
  aux states

let print_nodes chemin =
  let rec aux l = match l with
    | [] -> Printf.printf"\n %!"
    | id :: rest -> let a = Printf.printf" %d " id in aux rest
  in
  aux chemin

(* recherche d'un chemin dans un graphe -- parcours en profondeur 
à refaire : verifier que les arcs n'ait pas flow==capa (arc retour ==0)
*)
let find_path gr n1 n2 =
  let init_gr_ecart = (* graphe_ecart*) gr in  (* ON TRAVAILLE DEJA AVEC LE GRAPHE D'ECART  donc init inutile*)
  let l_state = nodes_state gr in
  let rec voisins s vois les_states l_antecedant = 
  (* let print_node_courant = Printf.printf" %d %!" s in *)
    let states = change_color les_states s Grey in
    match vois with
    | [] -> if s==n2 then (change_color states s Grey, List.rev (s::(List.rev l_antecedant))) else (
        let l_state_modif=change_color states s Black in
        let antecedant=recup_antecedant l_antecedant l_state_modif in
        if (antecedant >=0) then voisins antecedant (out_arcs init_gr_ecart antecedant) l_state_modif (List.rev (s :: (List.rev l_antecedant)))
        else ([(-1,Black)], []) (*Pas de chemin trouvé *)
      )
    | (id,(flow,1)) :: rest -> if (flow>0) && (id==n2) then ((change_color states n2 Grey), List.rev (s::(List.rev l_antecedant))) else  (* verification de la terminaison *)
      (if (flow>0) && (List.assoc id states)==White then voisins id (out_arcs init_gr_ecart id) states (List.rev (s :: (List.rev l_antecedant)))
      else voisins s rest states l_antecedant)
    | (id,(flow,0)) :: rest -> if flow>0 then
                              (match (find_arc init_gr_ecart id s) with
                              | None -> failwith "erreur graphe ecart : arc dans un seul sens"
                              | Some (flow, sens) -> if (flow>0) && (id==n2) then (change_color states n2 Grey, List.rev (s::(List.rev l_antecedant))) else  (* verification de la terminaison *)
                                    (if (flow>0) && (List.assoc id states)==White then voisins id (out_arcs init_gr_ecart id) states (List.rev (s :: (List.rev l_antecedant)))
                                    else voisins s rest states l_antecedant)
                              )
                              else voisins s rest states l_antecedant
    | (id,(flow,a)) :: rest -> failwith ("erreur lbl find path"^(string_of_int a))
  in
  let (state_final, l_ant) = voisins n1 (out_arcs (init_gr_ecart) n1) l_state [] in
  if state_final==[(-1,Black)] then [(-1,-1,0)] (*Pas de chemin trouvé *)
  else 
    (*let bb = Printf.printf"state final : " in
    let b = print_state state_final in 
    let cc = Printf.printf"liste antecedant : " in
    let c = print_nodes l_ant in *)
    let chemin_ordonne = ordre_chemin state_final l_ant n2 in
    let aaa = Printf.printf" chemin ordonnee : " in
    let a = print_nodes chemin_ordonne in
    recup_flow_chemin init_gr_ecart chemin_ordonne

(* Recupere le flow minimal d'un chemin (calcule de la variation de flow) *)
let get_vflow path = 
  let find_min v1 v2 = if v1 < v2 then v1 else v2 in
  let rec aux chemin flow = match chemin with
    | [] -> flow
    | (_, _,n) :: rest -> aux rest (find_min n flow)
  in 
  aux path max_int;;

let modifier_2arcs gr id1 id2 v = sub_tecart_arc (add_tecart_arc gr id1 id2 v) id2 id1 (v)

(* Met à jour le graphe d'écart -> modification en fonction de la variation de flow *)
let update_graph gr path v = 
  let rec update_path gr1 path = match path with
    | [] -> gr1
    | (id1, id2, flow) :: rest -> 
      match (find_arc gr id1 id2) with
      | None -> failwith "Probleme l'arc n'existe pas"
      | Some (flow, sens) -> match (flow,sens) with
        | (flow, 0) -> if (flow-v >=0) (* Retour donc pas besoin de verifier la valeur de l'arc aller*)
          then update_path (modifier_2arcs gr1 id1 id2 v) rest
          else failwith "erreur valeur negative si soustraction (retour)" (*on ne peut pas enlever de flow, on est deja a zero *)
        | (_, 1) -> if (flow-v)>=0 then update_path (modifier_2arcs gr1 id2 id1 v) rest
        else failwith "erreur valeur negative si soustraction (aller) flow et v "
        | (_,a) -> failwith ("erreur format lbl gr ecart "^(string_of_int a))
  in 
  update_path gr path;;

(* Algorithme de Ford Fulkerson*)
let get_max_flow gr s p = 
  (* Mettre les flots du graphe de flots à 0 *)
  let init_flow = gmap gr (fun (flow,capacity) -> (0,capacity)) in
  (* mfaire le graphe d'ecart*)
  let gr_ecart = graphe_ecart init_flow
  in 
  (* les iterations tant qu'on trouve un chemin *)
  let rec loop gr2 debit = match (find_path gr2 s p) with
    | [(-1,-1,0)] -> debit
    | res -> let var_flow = get_vflow res in 
    (* let print_res = print_res_find_path res in *)
     let iter = Printf.printf"une iter  de var_flow=%d \n%!" var_flow in 
      loop (update_graph gr2 res var_flow) (debit + var_flow) 
  in
  (* l'appel de la fonction recursive*)
  loop gr_ecart 0;;

(*On doit update le graphe d'ecart pas le graphe initial ! 
  Solution -> calculer graphe_ecart dans cette fonction et pas dans find_path *)

(* ---------- ETAPES --------------
   Init : Flot F = O et  Débit D = 0
   Itérations : répéter justqu'à ce qu'il n'y ait plus de chemin entre les deux nodes ----> res == [(-1,-1,0)]
    Chercher un chemin d'un node1 vers un node2 (source -> puits) grâce au graphe d'écarts --->find path
    Calculer la variation v de flot de ce chemin  ----> get_vflow
    Mettre à jour le graphe de flot --->update_graph
    Mettre à jour D = D + v
*)


;;





