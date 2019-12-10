
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

(* prend un chemin composé d'id et renvoie le chemin compose de (id id flow) *)
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
    let () = Printf.printf" chemin ordonnee : " in
    let () = print_nodes chemin_ordonne in
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
     let () = Printf.printf"une iter  de var_flow=%d \n%!" var_flow in 
      loop (update_graph gr2 res var_flow) (debit + var_flow) 
  in
  (* l'appel de la fonction recursive*)
  loop gr_ecart 0


(* ----------------------- FLOT MAX / COUT MIN AVEC MOORE-BELLMAND-FORD -----------------------------*)

(* Met à jour le graphe de flots *)
let update_graph_BF gr path v = 
  let rec update_path gr1 path = match path with
    | [] -> gr1
    | (id1, id2, flow) :: rest -> 
      match (find_arc gr id1 id2) with
      | None -> (* arc inverse donc soustraction *) update_path (sub_mfmc_arc gr1 id2 id1 v) rest
      | Some (flow, capa, _) -> update_path (add_mfmc_arc gr1 id1 id2 v) rest
  in 
  update_path gr path

(* les arcs "aller" : si le flot est pas égal à la capa, il vaut le cout *)
let arcs1_MFCM gr = gmap gr (fun (flow, capa, cout) -> if not(flow==capa) then (cout,1) else (0,1) )

(* les arcs retour, si le flto est pas nul, il vaut -cout *)
let arcs2_MFCM gr = 
  let modif_arc gr id1 id2 (flow,capa,cout) = new_arc gr id2 id1 ( if not(flow==0) then ((-cout), 0) else (0,0)) in
  let new_gr = clone_nodes gr in
  e_fold gr modif_arc new_gr ;;

let gr_ecart_MFCM gr = 
  let gr_1 = arcs1_MFCM gr and gr_2 = arcs2_MFCM gr in
  e_fold gr_1 new_arc gr_2 


let get_pred gr id = 
(* Pour tous les sommets du graphe, on vérifie si l'arc existe entre le sommet et l'id d'entrée *)
  let rec aux iter acu = match (node_exists gr iter) with
    | false -> acu
    | true -> if not((find_arc gr iter id)==None) then (aux (iter + 1) (iter::acu))else (aux (iter +1) acu)
  in 
  aux 0 []

let init_bellman gr s =
  let rec loop iter acu = match (node_exists gr iter) with
    | false -> acu
    | true -> if iter==s then loop (iter+1) ((0,-1)::acu) else loop (iter+1) ((max_int,0)::acu)
  in 
  List.rev (loop 0 [])

let get_cout l x = 
  let rec aux l iter = match l with 
    | [] -> failwith "[get_cout] Sommet inexistant dans la liste"
    | (c, id) :: rest -> if x==iter then c else aux rest (iter+1)
    in 
  aux l 0

let get_val_arc gr x y = match find_arc gr x y with
  | None -> failwith "[get_val_arc] Arc inexistant"
  | Some (cout, sens) -> cout

(* mise à jour du cout dans la liste des couts ----------DEPART A ZERO ------------ *)

(* prend une liste de cout et renvoie une nouvelle liste en modifiant la colonne x par (pred, new_cout) *)
let change_cost lcout x pred new_cout =
  let rec aux l iter acu = match l with
    | [] -> acu
    | (cost, id) :: rest -> if iter==x then aux rest (iter+1) ((new_cout, pred)::acu)
                            else aux rest (iter + 1) ((cost,id)::acu)
  in 
  List.rev (aux lcout 0 [])


(* le "for tous les predecesseurs" avec lpred la liste des y  + RENVOIE LE CONTINUER *)
let maj_all_pred gr lcout x lpred =
  let rec aux l lcost cont= match l with
    | [] -> (* let () = Printf.printf"FIN MAJ \n%!" in *) (lcost, cont)
    | y :: rest -> let cout_x = get_cout lcost x in 
                  let cout_y = get_cout lcost y in
                  let cout_arc_xy = get_val_arc gr y x in
                  if not(cout_arc_xy=0) then
                    if not(cout_y ==max_int) then
                      let new_cout = cout_y + cout_arc_xy in 
                      (*
                      let () = Printf.printf"x est %d et y est %d \n%!" x y in
                      let () = Printf.printf" cout y : %d et cout_arc_xy : %d \n%!" cout_y cout_arc_xy in
                      let () = Printf.printf"cout %d %d et new_cout %d \n%!" x cout_x new_cout in
                      let () = Printf.printf"liste de cout \n%!" in 
                      let () = print_idid_l lcost in
                      *)
                      if cout_x > new_cout then 
                        let new_lcout = change_cost lcost x y new_cout in 
                        (* let () = Printf.printf"Liste new cout : \n%!" in
                        let () = print_idid_l new_lcout in
                        *)
                        aux rest new_lcout true
                    else aux rest lcost cont
                  else aux rest lcost cont
                  else aux rest lcost cont
  in 
  let (res,cont) = aux lpred lcout false in
  (*
  let () = Printf.printf"Resultat maj_all_pred : \n%!" in 
  let () = print_idid_l res in
  let () = Printf.printf"cont : %b \n%!" cont in *)
  (res,cont)

(* le for "tous les sommets x!=s" + LE CONTINUER*)
let for_all_sommets gr lcout s = 
  let rec aux iter l continuer = 
  (* let () = Printf.printf"iter : %d \n%!" iter in *)
    if node_exists gr iter then (* pour tous les nodes du graphe*)
        if iter==s then aux (iter+1) l continuer (* x=s donc on ne parcours pas les precesseurs*)
        else let (new_lcout,cont)=maj_all_pred gr l iter (get_pred gr iter) in 
              (* let () = Printf.printf"liste cout %d : \n%!" iter in 
              let () = print_idid_l new_lcout in 
              *)
              if cont then aux (iter+1) new_lcout true
              else (* (new_lcout, continuer) *) aux (iter+1) new_lcout continuer
    else (l, continuer)
  in
  aux 0 lcout false

let get_id lcout id = 
  let rec aux l iter = match l with
    | [] -> failwith ("[get_id] error"^(string_of_int iter))
    | (cout, id1) :: rest -> if (iter==id) then id1 else aux rest (iter+1)

  in 
  aux lcout 0

let recup_chemin_bf l s p = 
  let rec aux id_courant acu = 
    if id_courant==s then acu else let next_id = get_id l id_courant in aux next_id (next_id :: acu)
  in 
  aux p [p]

let chemin_valide lcout path = 
  let rec aux validite l = match l with
    | [] -> validite
    | id :: rest -> match List.nth lcout id with 
        | (cout, _) -> if cout == max_int then false else aux validite rest
  in 
  aux true path 

(* a partie d'un graph de flow (calcul lui-même le graphe d'écart) *)
let find_bellman gr s p = 
  let gr_ecart = gr_ecart_MFCM gr in
  let liste_cout = init_bellman gr_ecart s in
  let () = Printf.printf" Liste de cout - Debut : \n%!" in 
  let () = print_idid_l liste_cout in 
  let rec aux l = match for_all_sommets gr_ecart l s with
    | (res, false) -> let () = Printf.printf"false\n%!" in res
    | (res, true) -> let () = Printf.printf"continuer\n%!" in aux res
  in 
  let lcost_final = aux liste_cout in 
  let () = Printf.printf"s : %d et p %d \n%!" s p in 
  let () = Printf.printf" Liste de cout - Fin : \n%!" in 
  let () = print_idid_l lcost_final in 

  let chemin_id = recup_chemin_bf lcost_final s p in
  let () = Printf.printf"chemin id trouve : \n%!" in 
  let () = print_nodes chemin_id in

  if not(chemin_valide lcost_final chemin_id) then [(-1,-1,0)] 
  else 
  let flow_chemin = recup_flow_chemin gr_ecart chemin_id in
  let () = Printf.printf" flow chemin : \n%!" in 
  let () = print_res_find_path flow_chemin in
  flow_chemin

let get_cout_chemin gr res =
  let rec aux acu l = match l with 
  | [] -> acu
  | (id1, id2, cout) :: rest -> match (find_arc gr id1 id2) with
        | None -> begin (* on a pris un arc retour *)
           match (find_arc gr id2 id1) with 
            | None -> failwith "[get_vflow_bf] error arc inexistant"
            | Some (flow, capa, cout) -> aux (acu+(cout*capa)) rest
          end
    | Some (flow, capa, cout) -> aux (acu+(cout*capa)) rest
  in 
  aux 0 res

let find_min liste = let () = print_nodes liste in
  let rec aux min l = match l with
    | [] -> min
    | flow :: rest -> if min>flow then aux flow rest else aux min rest
  in 
  aux max_int liste

let get_vflow_bf gr chemin = 
  let rec aux acu l = match l with
    | [] -> acu
    | (id1, id2, cout) :: rest -> match (find_arc gr id1 id2) with
        | None -> begin (* on a pris un arc retour *)
           match (find_arc gr id2 id1) with 
            | None -> failwith "[get_vflow_bf] error arc inexistant"
            | Some (flow, capa, cout) -> aux (flow::acu) rest
          end
        | Some (flow, capa, cout) -> aux ((capa-flow)::acu) rest
  in 
  let liste_flow = aux [] chemin in 
  find_min liste_flow


let string_of_mfcm (flow, capa, cout) = (string_of_int flow) ^ "/" ^ (string_of_int capa) ^ "/" ^ (string_of_int cout)

let flow_max_cout_min gr s p = 
(* let gr_ecart = gr_ecart_MFCM gr in *)
  let rec loop index gr2 debit cout = 
  Gfile.export ("test"^(string_of_int index)) (gmap gr2 string_of_mfcm);
  match (find_bellman gr2 s p) with
      | [(-1,-1,0)] -> (debit, cout)
      | res -> let var_flow = get_vflow_bf gr2 res in 
      let new_cost = get_cout_chemin gr2 res in
      let () = Printf.printf"une iter : var_flow=%d et cout=%d \n%!" var_flow new_cost in 
      loop (index+1) (update_graph_BF gr2 res var_flow) (debit + var_flow) (cout+new_cost)
  in
  loop 0 gr 0 0

(* ------------- ANCIEN maj cout et find bellman---------
let maj_cout gr l x y = 
let () = Printf.printf"maj cout debut \n%!" in
  let rec aux iter l acu = match l with 
  | [] -> acu
  | (c,id) :: rest -> if iter==x then (* on va à la bonne colonne de la liste des couts *)
      let new_c = (get_cout l y) + (get_val_arc gr x y) in
      let () = Printf.printf"cout : %d et new cout : %d \n%!" c new_c in
      if (c > new_c) then
        aux (iter+1) rest ((new_c,id)::acu)
      else aux (iter+1) rest ((c,id)::acu) 
    else let () = Printf.printf"iter diff de x : iter %d et x %d \n%!" iter x in aux (iter+1) rest acu
  in
  aux 0 l []

let rec forall_pred gr l iter acu =  match l with 
  | [] -> acu
  | id :: rest -> let () = Printf.printf"forall pred de iter : %d \n%!" iter in forall_pred gr rest (iter+1) (maj_cout gr acu iter id)

let find_bellman gr s p =
let () = Printf.printf"debut Bellman \n%!" in
  let liste_cout = init_bellman gr s in
  let rec parcours_sommets iter l lancien = match (node_exists gr iter) with 
        | false -> l
        | true -> 
          (* let lpred = get_pred gr iter in
          let maj_cout =  forall_pred gr lpred iter lancien in *)
          let maj_cout = forall_pred gr l s lancien in 
          if maj_cout == lancien then lancien
          else parcours_sommets (iter+1) maj_cout lancien
  in
    let lcost_final = parcours_sommets 0 liste_cout liste_cout in
    let () = Printf.printf"liste cout final : \n%!" in
    let () = print_idid_l lcost_final in
    let chemin_id = recup_chemin_id lcost_final s p in
    let () = Printf.printf"chemin de nodes : \n%!" in
    let () = print_nodes chemin_id in
    recup_flow_chemin gr chemin_id
*)

(*
  let rec recherche iter l = 
    let mutable cont = true in 
    match (node_exists gr iter) with 
      | None -> l
      | Some lbl -> if iter==s then recherche (iter+1) l else
        let rec aux_pred l acu cont =
          match l with
            | [] -> acu
            | x :: rest -> 
            
              let current_cout = get_cout l x in 
              let lc_modif = maj_cout l x iter in
              if not(current_cout == get_cout lc_modif x) then aux_pred rest ( :: acu)
              else recherche (iter+1) l

        in 
        aux_pred (get_pred gr iter)
         
  in
  recherche 0 liste_cout
*)
  
 
(*
let flow_max_cout_min gr s p = 
  (* Mettre les flots du graphe de flots à 0 *)
  let init_flow = gmap gr (fun (flow,capacity) -> (0,capacity)) in
  (* mfaire le graphe d'ecart*)
  let gr_ecart = graphe_ecart init_flow
  in 
  (* les iterations tant qu'on trouve un chemin *)
  let rec loop gr2 debit = match (find_bellman gr2 s p) with
    | [(-1,-1,0)] -> debit
    | res -> let var_flow = get_vflow res in 
    (* let print_res = print_res_find_path res in *)
     let iter = Printf.printf"une iter  de var_flow=%d \n%!" var_flow in 
      loop (update_graph gr2 res var_flow) (debit + var_flow) 
  in
  (* l'appel de la fonction recursive*)
  loop gr_ecart 0
*)
