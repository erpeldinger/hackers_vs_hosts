
open Gfile
open Tools
open Ford_fulkerson

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* Rewrite the graph that has been read. *)


  (* On devrait mettre toutes ces lignes de test dans un fichier à part,
     avec les commandes utilisées pour les lancer *)

  (* let () = write_file outfile (clone_nodes graph) in *)
  (* let () = write_file outfile (gmap graph (fun x -> "test" ^ x)) in *)
  (*let () = write_file outfile (gmap ((add_arc (gmap graph int_of_string) 0 2 10)) string_of_int) in*)
  
  
  (* -------TEST GRAPHE ECART ET FIND PATH--------- *)
  (*let string_to_tarc str = Scanf.sscanf str "%d/%d" (fun flow capa -> (flow,capa)) in

  let string_of_tecart (flow, sens) = match (flow, sens) with
    | (flow, 0) -> (string_of_int flow) ^ "/retour"
    | (flow, 1) -> (string_of_int flow) ^ "/aller"
    | (flow,_) -> (string_of_int flow) ^ "/ERROR" in
*)
  let tarc_of_string str = Scanf.sscanf str "%d/%d" (fun flow capa -> (flow,capa)) in
(*
   let trans flow sens = match (flow, sens) with
      | (flow, "aller") -> (flow, 1)
      | (flow, "retour") -> (flow, 0)
      | (_, y) -> (flow, -111)
   in

  let tecart_of_string str = Scanf.sscanf str "%d/%s" trans in
*)
  (*let gr_ecart = gmap graph tecart_of_string in
  let gr_string = gmap gr_ecart string_of_tecart in
  let () = export outfile gr_string in*)

(* ----------------- TEST FIND BELLMAN ----------------

let int_graph = gmap graph tarc_of_string in
let init_graphe = gmap int_graph (fun (flow,capacity) -> (0,capacity)) in
  let gr_ecart = graphe_ecart init_graphe in
  let mon_chemin = find_bellman gr_ecart 0 8 in
  let () =
     let rec aux l = match l with
       | [] -> Printf.printf"\n%!"
       | (id1, id2, flow) :: rest -> let () = Printf.printf" %d %d %d " id1 id2 flow in aux rest
     in
     aux mon_chemin in
*)

(* ---------------------TEST TEST BELLMAN--------------------*)
let int_graph = gmap graph tarc_of_string in
let init_graphe = gmap int_graph (fun (flow,capacity) -> (0,capacity)) in
  let gr_ecart = graphe_ecart init_graphe in
  let mon_chemin = test_bellman gr_ecart 0 8 in
  let () =
     let rec aux l = match l with
       | [] -> Printf.printf"\n%!"
       | (id1, id2, flow) :: rest -> let () = Printf.printf" %d %d %d " id1 id2 flow in aux rest
     in
     aux mon_chemin in


  (* ------- TEST INITIALISATION FORD FULK --------- 
     let init_flow = gmap (gmap graph tarc_of_string) (fun (flow,capacity) -> (0,capacity)) in
     (* faire le graphe d'ecart*)
     let gr_ecart = graphe_ecart init_flow in
     let () = export outfile (gmap gr_ecart string_of_tecart) in
*)

  (* ---TEST FORD FULKERSON ------- 
     let int_graph = gmap graph tarc_of_string in
     let debit_final = get_max_flow int_graph 0 8 in
     let () =Printf.printf"debit final : %d \n%!" debit_final in 
*)

  (* ------ TEST FIND PATH ------ 
  (*
  let int_graph = gmap graph tarc_of_string in
  let init_graphe = gmap int_graph (fun (flow,capacity) -> (0,capacity)) in
  let gr_ecart = graphe_ecart init_graphe in*)
  let mon_chemin = find_path (gmap graph tecart_of_string) 0 8 in
  let a =
     let rec aux l = match l with
       | [] -> Printf.printf"\n%!"
       | (id1, id2, flow) :: rest -> let a = Printf.printf" %d %d %d " id1 id2 flow in aux rest
     in
     aux mon_chemin in*)

   (*let () = export outfile graph in *)
   (*

  let var_flow = get_vflow mon_chemin in
  let a = Printf.printf" var = %d \n%!" var_flow in
  let update_gr = update_graph (gmap graph tecart_of_string) mon_chemin var_flow in
  let () = write_file outfile (gmap update_gr string_of_tecart) in 
  *)
     (*
     let gr_ecart_tecart = graphe_ecart int_graph in 
     let () = export outfile (gmap gr_ecart_tecart string_of_tecart) in 
   *)
  (*let mon_gr_ecart = graphe_ecart int_graph in *) 


  (*let () = export outfile (gmap gr_ecart_tecart string_of_tecart) in *)
     (*
     let () = 
     let rec aux l = match l with
     | [] -> Printf.printf"\n%!"
     | (id1, id2, flow) :: rest -> let a = Printf.printf" %d %d %d " id1 id2 flow in aux rest
     in
     aux mon_chemin in

     let () =
     let rec aux l = match l with
     | [] -> Printf.printf" \n%!"
     | (id1, id2, flow) :: rest -> let a = Printf.printf"n1 : %d | n2 : %d | flow : %d \n%!" id1 id2 flow in aux rest
     in
     aux mon_chemin
     in*)
  (*let mon_chemin = find_path (graphe_ecart int_graph) 0 8 in 
    let flow_min = get_vflow mon_chemin in
    (*let p = Printf.printf"flow min : %d \n%!" flow_min in *)
    (* ---- PRINT LE GRAPHE ECART EN SORTIE DE FIND PATH----
    let gr_ecart_tecart = graphe_ecart int_graph in 
    let () = export outfile (gmap gr_ecart_tecart string_of_tecart) in *)

    let update_gr_ecart = update_graph (graphe_ecart int_graph) mon_chemin flow_min in *)
     (*
     let string_of_tecart (flow, sens) = match (flow, sens) with
     | (flow, 0) -> (string_of_int flow) ^ "/retour"
     | (flow, 1) -> (string_of_int flow) ^ "/aller"
     | (flow,_) -> (string_of_int flow) ^ "/ERROR" in
    *)




  (*-----------------------------------TESTS RECUP LAST GREY, CHANGE COLOR------------------------------------------
    let ma_liste = [(1, Black);(2, Grey);(3,White);(4,Grey);(5,Black)] in
    let mon_grey = recup_last_grey ma_liste in
    let mon_change_color = change_color ma_liste 2 Black in
    let mon_match color = match color with
    |Black -> "Black"
    |Grey->  "Grey"
    |White -> "White"
    in
    let () = 
    let rec aux l = match l with
    | [] -> Printf.printf"\n%!"
    | (id1, color) :: rest -> let a = Printf.printf" %d %s" id1 (mon_match color) in aux rest
    in
    aux mon_change_color in
  *)
  (* -------------------------------------TEST NODES_STATE------------------------------------------------------------------
     let l_state = nodes_state mon_gr_ecart in
     let mon_match color = match color with
     |Black -> "Black"
     |Grey->  "Grey"
     |White -> "White"
     in
     let () = 
     let rec aux l = match l with
     | [] -> Printf.printf"\n%!"
     | (id1, color) :: rest -> let a = Printf.printf" %d %s" id1 (mon_match color) in aux rest
     in
     aux l_state in
  *)

  (* let gr_ecart_str = write_file outfile (gmap mon_gr_ecart string_of_int) in 
     let () = export outfile (gmap mon_gr_ecart string_of_int) in *)
  (*let mon_chemin = find_path mon_gr_ecart _source _sink in

    let () = 
    let rec aux l = match l with
    | [] -> ()
    | (id1, id2, flow) :: rest -> let p = Printf.printf"%d %d %d \n%!" id1 id2 flow in aux rest
    in
    aux mon_chemin in *)
  (* Pour tester export : 
     dot -Tsvg graphs/graph2_export.dot > graphs/graph2_export.svg
  *)
  (* --------------------------------------TEST RECUP FLOW CHEMIN-----------------------------------------------------------
     let mon_vrai_chemin = [(0,1,2);(1,3,2);(3,4,1);(4,7,1);(7,8,3)] in
     let mon_chemin = [0;1;3;4;7;8] in
     let recup_flow = recup_flow_chemin mon_gr_ecart mon_chemin in
     let () = 
     let rec aux l = match l with
     | [] -> Printf.printf"\n%!"
     | (id1, id2, lbl) :: rest -> let a = Printf.printf" %d %d %d \n%!" id1 id2 lbl in aux rest
     in
     aux recup_flow in
  *)
  (* ----------------------------------------TEST RECUP CHEMIN -------------------------------------------------------------
     let ma_liste = [(0, Grey);(1, Grey);(2,White);(3,Grey);(4,Grey);(5,White);(6,Black);(7,Grey);(8,Grey)] in
     let ma_liste2 = [(0, Black);(1, Grey);(2,White);(3,Grey);(4,White);(5,Black);(6,Grey);(7,White);(8,White)] in
     let mon_chemin_sans_couleur = recup_chemin ma_liste 0 8 in
     let () =
     let rec aux l = match l with
     | [] -> Printf.printf"\n%!"
     | id :: rest -> let a = Printf.printf" %d " id in aux rest
     in
     aux mon_chemin_sans_couleur 
     in
  *)
  (* LES TEST : 
     ./ftest.native graphs/graph3_ok.txt 0 4 graphs/test.dot
     dot -Tsvg graphs/test.dot > graphs/test.svg
  *)
  ()