open Ford_fulkerson
open Graph

type tdortoir = {
    nb_dortoirs : int;
    nb_lits : int list; (* [nb_lits_dortoir1, nb_lits_dortoir2, etc.] *)
}

type thost = {
    id_host : int;
    nb_places : int;
    nb_nuits : int;
    nb_dortoir : tdortoir;
    nb_chb_indiv : int;
    animaux : int;
    fumeur : int;
    tolere_cig : int;
}

type thacker = {
    id_hacker : int;
    nb_nuits : int;
    animaux : int;
    fumeur : int;
    tolere_cig : int; 
    mixte : int;
    }

(*
type tl_host = {
    mutable courant : thost;
    mutable suivant : tl_host;
}
type tl_hacker = {
    mutable h_courant : thacker;
    mutable h_suivant : tl_hacker;
}
*)

type host_cell = {mutable courant : thost; mutable suivant : tl_host}
and tl_host = Host of thost | Cell_host of host_cell;;

type tl_hacker = Hacker of thacker | Cell_hacker of hacker_cell
and hacker_cell = {mutable h_courant : thacker; mutable suivant : tl_hacker};;

let dortoir_dernier = {nb_dortoirs=(-1);nb_lits=[]};;
let host_dernier = {id_host=(-1); nb_places=(-1); nb_nuits=(-1); nb_dortoir=dortoir_dernier; nb_chb_indiv=(-1); animaux=(-1);fumeur=(-1);tolere_cig=(-1)};;

let hacker_dernier = {id_hacker=(-1); nb_nuits=(-1); animaux=(-1);fumeur=(-1);tolere_cig=(-1); mixte=(-1)};;

(* Fonctions permettant l'ajout d'un host/hacker dans un type tl_host ou tl_hacker *)
let add_tlhost host lhost =
  let rec aux l = match l with
  | Host h-> if h.id_host==(-1) then failwith "erreur add_tlhost" else ()
  | Cell_host x -> if x.courant.id_host==(-1) then x.courant<-host else aux x.suivant
  in
  aux lhost

let add_tlhacker hacker lhacker =
  let rec aux l = match l with
  | Hacker h-> if h.id_hacker==(-1) then failwith "erreur add_tlhacker" else ()
  | Cell_hacker x -> if x.h_courant.id_hacker==(-1) then x.h_courant<-hacker else aux x.suivant
  in
  aux lhacker

(* FONCTIONS DE COMPATIBILITES *)
let cp_fumer host hacker = if not(host.id_host==(-1)) then 
  match (hacker.fumeur, hacker.tolere_cig) with
    | (1,_) -> if host.tolere_cig==1 then true else false
    | (0,1) -> true
    | (0,0) -> if host.tolere_cig==1 then false else true
    | _ -> failwith"ERREUR de compatibilité fumer"
  else false
;;

let cp_animaux host hacker = if not(host.id_host==(-1)) then 
  match (host.animaux, hacker.animaux) with
    | (1,1) -> false
    | _ -> true
    else false
;;

let cp_mixte host hacker = 
    if hacker.mixte == 1 then true 
    else
        if host.nb_chb_indiv > 0 then true else false
        (* On part du principe que les femmes et les hommes sont mélangés dans les drotoirs *)
;;

(* Indicate wether an arc can be created between a host and a hacker*)
let compatible host hacker = (cp_animaux host hacker) && (cp_mixte host hacker) && (cp_fumer host hacker) ;;


(* CREATION DU GRAPHE EN X FONCTIONS *)

(* création d'un hôte, ajout de l'hôte dans le graph et dans la liste tlhosts *)
let add_host gr list id p n d c a f t = 
let h = {
    id_host = id;
    nb_places = p;
    nb_nuits = n;
    nb_dortoir = d;
    nb_chb_indiv = c;
    animaux = a;
    fumeur = f;
    tolere_cig = t;
    }
    in
    new_node gr h.id_host ;
    add_tlhost h list

let add_hacker gr list id n m a f t = 
let h = {
    id_hacker = id;
    nb_nuits = n;
    animaux = a;
    fumeur = f;
    tolere_cig = t;
    mixte = m;
    }
    in
    new_node gr h.id_hacker ;
    add_tlhacker h list

let creer_host gr line list id =
  try Scanf.sscanf line "%dp%dn%dd%da%df%dt" (add_host gr list id) 
  with e ->
    Printf.printf "Cannot read node in line - %dp%dn%dd%da%df%dt\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let creer_hacker gr line list id =
  try Scanf.sscanf line "%dn%dm%da%df%dt" (add_hacker gr list id) 
  with e ->
    Printf.printf "Cannot read node in line - %dn%dm%da%df%dt\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let gr_hosts phosts = 

  let infile = open_in phosts in

  let list = Host host_dernier in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph id =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

        let (n2, graph2) =
            (* Ignore empty lines *)
            if line = "" then (n, graph)

            (* The first character of a line determines its content *)
            else match line.[0] with
            (*| 'h' ->  inutile ?*)
            | 'p' -> creer_host graph line list id (* Créer host*)
            | 'd' -> ()(* Compléter type host + dortoir + ajouter fonction traitement "/" *)
            (*| '.' -> creer_node graph line créer nodes -> deja fait dans creer host ???*)
            | _ -> failwith"ERREUR lecture ligne infile host"
        in 
        loop n2 graph2 (id+1)

        with End_of_file -> graph (* Done *)
    in

    let final_graph = loop 0 empty_graph 0 in

    close_in infile ;
    final_graph
;;

let creer_arc gr line = assert false;;

let gr_hackers phackers = 

  let infile = open_in phackers in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph id =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

        let (n2, graph2) =
            (* Ignore empty lines *)
            if line = "" then (n, graph)

            (* The first character of a line determines its content *)
            else match line.[0] with
            (*| 'h' ->  inutile ?*)
            | 'n' -> creer_hacker graph line(* créer et ajouter un hacker*)
            | '.' -> creer_arc graph line (* créer node + créer arc*)
            | _ -> failwith"ERREUR lecture ligne infile host"
        in

        loop n2 graph2 (id+1)

        with End_of_file -> graph (* Done *) 
    in

    let final_graph = loop 0 empty_graph 0 in (* a changer *)

    close_in infile ;
    final_graph
;;

let creer_gr = 
(* on utiliser gr_hosts et gr_hackers cf fonction gr_ecart dans FF *)
;;

(* Etapes pour la création graphe
    1 - lecture du ficher host
        -> pour chaque host, on crée un node et un type host
        -> ajouter host à la liste l_host
    2 - Lecture du fichier 
        -> pour chaque hacker, on crée un type hacker
        -> évaluer compatibilité entre chaque host (création d'un arc en fonction)
        -> ajouter host à la liste l_hacker
**)


let affectation = assert false

(* lancement de l'algo *)
(* On récupère le dernier graphe de FF et création de listes avec l'host et les hackers conviés chez lui *)
;;





