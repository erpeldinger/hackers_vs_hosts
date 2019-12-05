open Ford_fulkerson

type tdortoir = {
    nb_dortoirs : int;
    nb_lits : int list; (* [nb_lits_dortoir1, nb_lits_dortoir2, etc.] *)
}

type thost = {
    id_host : int;
    nb_places : int;
    nb_nuits : int;
    nb_dortoir : t_dortoir;
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

type tl_host {
    mutable courant : thost;
    mutable suivant : tl_host;
}

let l_thost;;
let l_thacker;;

let add_tlhost host =
let rec aux l = match (l.courant) with
| {0000} as dernier -> l <- {h; dernier}
| {} -> aux l.suivant

let cp_fumer host hacker = match (hacker.fumeur, hacker.tolere_cig) with
    | (1,_) -> host.tolere_cig
    | (0,1) -> true
    | (0,0) -> not(host.tolere_cig)
    | _ -> failwith"ERREUR de compatibilité fumer"
;;

let cp_animaux host hacker = match (host.animaux, hacker.animaux) with
    | (1,1) -> false
    | _ -> true
;;

let cp_mixte host hacker = 
    if hacker.mixte == true then true 
    else
        if host.nb_chb_indiv > 0 then true else false
        (* On part du principe que les femmes et les hommes sont mélangés *)
;;

(* Indicate wether an arc can be created between a host and a hacker*)
let compatible host hacker = (cp_animaux host hacker) && (cp_mixte host hacker) (cp_fumer host hacker) ;;
    

(* CREATION DU GRAPHE EN X FONCTIONS *)

(* création d'un hôte, ajout de l'hôte dans le graph et dans la liste tlhosts *)
let add_host gr id p n d c a f t = 
let h = {
    id_host = id;
    nb_places : p;
    nb_nuits = n;
    nb_dortoir = d;
    nb_chb_indiv = c;
    animaux = a;
    fumeur = f;
    tolere_cig = t;
    }
    in
    new_node gr id_host ;
    add_tlhost(h) (* ne modifie pas l_host, en crée un autre! *)

let add_host gr id n m a f t = 
let h = {
    id_hacker = id;
    nb_nuits = n;
    animaux = a;
    fumeur = f;
    tolere_cig = t;
    mixte = m;
    }
    in
    new_node gr id_hacker ;
    add_tlhacker(h) (* ne modifie pas l_host, en crée un autre! *)

let creer_host id graph line =
  try Scanf.sscanf line "%dp%dn%dd%da%df%dt" (add_host gr id) 
  with e ->
    Printf.printf "Cannot read node in line - %dp%dn%dd%da%df%dt\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let creer_hacker id graph line =
  try Scanf.sscanf line "%dn%dm%da%df%dt" (add_hacker gr id) 
  with e ->
    Printf.printf "Cannot read node in line - %dn%dm%da%df%dt\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


let gr_hosts phosts = 

  let infile = open_in phosts in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

        let (n2, graph2) =
            (* Ignore empty lines *)
            if line = "" then (n, graph)

            (* The first character of a line determines its content *)
            else match line.[0] with
            | 'h' -> (* inutile ?*)
            | 'p' -> creer_host graph line(* Créer host*)
            | 'd' -> (* Compléter type host + dortoir + ajouter fonction traitement "/" *)
            | '.' -> creer_node graph line (*créer nodes*)
            | _ -> failwith"ERREUR lecture ligne infile host"
        in 
        loop n2 graph2

        with End_of_file -> graph (* Done *)
    in

    let final_graph = loop 0 empty_graph in

    close_in infile ;
    final_graph
;;

let gr_hackers phackers = 

  let infile = open_in phackers in

  (* Read all lines until end of file. 
   * n is the current node counter. *)
  let rec loop n graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

        let (n2, graph2) =
            (* Ignore empty lines *)
            if line = "" then (n, graph)

            (* The first character of a line determines its content *)
            else match line.[0] with
            | 'h' -> (* inutile ?*)
            | 'n' -> creer_hacker graph line(* créer et ajouter un hacker*)
            | '.' -> (* créer node + créer arc*)
            | _ -> failwith"ERREUR lecture ligne infile host"
        in

        loop n2 graph2

        with End_of_file -> graph (* Done *)
    in

    let final_graph = loop 0 empty_graph in

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





