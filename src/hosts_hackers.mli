
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
    animaux : bool;
    fumeur : bool;
    tolere_cig : bool;
}

type thacker = {
     id_hacker : int;
    nb_nuits : int;
    animaux : int;
    fumeur : bool;
    tolere_cig : bool; 
    mixte : bool;
    }

(* Indicate wether an arc can be created between a host and a hacker*)
val compatible : t_host -> t_hacker -> bool