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

type host_cell = {mutable courant : thost; mutable suivant : tl_host}
and tl_host = Host of thost | Cell_host of host_cell;;

type tl_hacker = Hacker of thacker | Cell_hacker of hacker_cell
and hacker_cell = {mutable h_courant : thacker; mutable suivant : tl_hacker};;

(* Last host and hacker in a list (equivalent to null in other languages) *)
let dortoir_dernier = {nb_dortoirs=(-1);nb_lits=[]};;
let host_dernier = {id_host=(-1); nb_places=(-1); nb_nuits=(-1); nb_dortoir=dortoir_dernier; nb_chb_indiv=(-1); animaux=(-1);fumeur=(-1);tolere_cig=(-1)};;
let hacker_dernier = {id_hacker=(-1); nb_nuits=(-1); animaux=(-1);fumeur=(-1);tolere_cig=(-1); mixte=(-1)};;

(* Add a host in the given list (type host_cell) *)
let add_tlhost host lhost =
  let rec aux l = match l with
  | Host h-> if h.id_host==(-1) then failwith "erreur add_tlhost" else ()
  | Cell_host x -> if x.courant.id_host==(-1) then (x.courant<-host ; let lastH = Host host_dernier in x.suivant<-lastH) else aux x.suivant
  in
  aux lhost

(* Add a hacker in the given list (type hacker_cell) *)
let add_tlhacker hacker lhacker =
  let rec aux l = match l with
  | Hacker h-> if h.id_hacker==(-1) then failwith "erreur add_tlhacker" else ()
  | Cell_hacker x -> if x.h_courant.id_hacker==(-1) then (x.h_courant<-hacker ; let lastH = Hacker hacker_dernier in x.suivant<-lastH) else aux x.suivant
  in
  aux lhacker

(* ---------------------------- Functions related to compatibility ----------------------------------- *)

(* Return the "smoke compatibility" between a host and a hacker *)
let cp_fumer host hacker = if not(host.id_host==(-1)) then 
  match (hacker.fumeur, hacker.tolere_cig) with
    | (1,_) -> if host.tolere_cig==1 then true else false
    | (0,1) -> true
    | (0,0) -> if host.tolere_cig==1 then false else true
    | _ -> failwith"ERREUR de compatibilité fumer"
  else false

(* Return the "animal compatibility" between a host and a hacker *)
let cp_animaux host hacker = if not(host.id_host==(-1)) then 
  match (host.animaux, hacker.animaux) with
    | (1,1) -> false
    | _ -> true
    else false

(* Return the "gender diversity compatibility" between a host and a hacker *)
let cp_mixte host hacker = 
    if hacker.mixte == 1 then true 
    else
        if host.nb_chb_indiv > 0 then true else false
        (* We consider that there is always women and men in a dormitory *)

(* Indicate wether an arc can be created between a host and a hacker*)
let compatible host hacker = (cp_animaux host hacker) && (cp_mixte host hacker) && (cp_fumer host hacker) ;;

(* ---------------------------- Functions related to arcs'/nodes' creation ----------------------------------- *)

(* Create an host, add the host in the given list and create a node in the given graph *)
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
    add_tlhost h list ;
    new_node gr h.id_host

(* Create an hacker, add the hacker in the given list and create a node in the given graph *)
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
    add_tlhacker h list ;
    new_node gr h.id_hacker

(* Read a line and call a function to create the host/node *)
let creer_host gr line list id =
  try Scanf.sscanf line "%dp%dn%dd%da%df%dt" (add_host gr list id)
  with e ->
    Printf.printf "Cannot read node in line - %dp%dn%dd%da%df%dt\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Read a line and call a function to create the hacker/node *)
let creer_hacker gr line list id =
  try Scanf.sscanf line "%dn%dm%da%df%dt" (add_hacker gr list id) 
  with e ->
    Printf.printf "Cannot read node in line - %dn%dm%da%df%dt\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Match the hacker with each host to see if an arc need to be created, and create it if needed*)
(* We still need to change the label of the arcs *)
let creer_arc gr line list hacker =
  let rec loop gr l = match l with
    | Host h -> if compatible h hacker then new_arc gr h.id_host hacker.id_hacker "0/1" else gr
    | Cell_host cl -> if compatible cl.courant hacker then loop (new_arc gr cl.courant.id_host hacker.id_hacker "0/1") cl.suivant else loop gr cl.suivant
  in 
  loop gr list

(* Match each hacker with each host to see if an arc need to be created, and create it if needed *)
let creer_arcs graph line list lh =
  let rec loop gr l = match l with
    | [] -> gr
    | h :: rest -> loop (creer_arc gr line list h) rest
  in 
  loop graph lh

(* ---------------------------- Functions that create the graph corresponding to the given files ----------------------------------- *)

(* Create a graph containing the nodes corresponding to the given file (Hosts.txt) *)
let gr_hosts phosts = 

  let infile = open_in phosts in

  (* A list that will contain all the created hosts *)
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
            (*| 'h' ->  useless *)
            | 'p' -> creer_host graph line list id (* create the host, add it to the list and create the corresponding node *)
            (*| 'd' -> We need to create a function to exploit the "t_dortoir" structure *)
            (*| '.' -> useless, everything is done above *)
            | _ -> failwith"ERREUR lecture ligne infile host"
        in 
        loop n2 graph2 (id+1)

        with End_of_file -> (graph, list)
    in

    let final_graph = loop 0 empty_graph 0 in

    close_in infile ;
    final_graph

(* Create a graph containing the nodes and arcs corresponding to the given file (Hackers.txt) *)
let gr_hackers phackers gr_host list_host = 

  let infile = open_in phackers in

  (* A list that will contain all the created hackers *)
  let list_hackers = Hacker hacker_dernier in

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
            (*| 'h' ->  useless *)
            | 'n' -> creer_hacker graph line list id (* create the host, add it to the list and create the corresponding node *)
            | '.' -> creer_arc graph line list_host list_hackers(* create the arcs that need to be created *)
            | _ -> failwith"ERREUR lecture ligne infile host"
        in

        loop n2 graph2 (id+1)

        with End_of_file -> graph
    in

    let final_graph = loop 0 gr_host 0 in

    close_in infile ;
    final_graph

(* Function that create the final graph *)
let creer_gr phosts phackers = 
  let (gr_host, list_hosts) = gr_hosts phosts in 
  gr_hackers phackers gr_host list_hosts

(* Run the fold fulkerson algorithm and create the solution (who is going to sleep where ?) *)
let affectation = assert false