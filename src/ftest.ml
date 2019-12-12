
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


  (* -------------------------- FORD FULKERSON ------------------------------- *)
  (*
    let tarc_of_string str = Scanf.sscanf str "%d/%d" (fun flow capa -> (flow,capa)) in  
    let string_of_tecart (flow, sens) = match (flow, sens) with
    | (flow, 0) -> (string_of_int flow) ^ "/retour"
    | (flow, 1) -> (string_of_int flow) ^ "/aller"
    | (flow,_) -> (string_of_int flow) ^ "/ERROR" in
  
    let int_graph = gmap graph tarc_of_string in
    let (debit_final, gr_final) = get_max_flow int_graph _source _sink in
    let () =Printf.printf"debit final : %d \n%!" debit_final in 
    let () = export outfile (gmap gr_final string_of_tecart) in 
  *)


  (* -------------- FORD FULKERSON MAX FLOX MIN COST ------------------------- *)
  
  let mfcm_of_string str = Scanf.sscanf str "%d/%d/%d" (fun flow capa cout -> (flow,capa, cout)) in
  let string_of_mfcm (flow, capa, cout) = (string_of_int flow) ^ "/" ^ (string_of_int capa) ^ "/" ^ (string_of_int cout) in

  let mfcm_graph = gmap graph mfcm_of_string in 
  let (debit_final, cout_final, gr_final) = flow_max_cout_min mfcm_graph _source _sink in
  let () = Printf.printf"debit final : %d \n%!" debit_final in
  let () = Printf.printf"cout final : %d \n%!" cout_final in
  let () = export outfile (gmap gr_final string_of_mfcm) in
  

  (* ------------------ Testing --------------------------------------------*)
  (*
     ./ftest.native graphs/<monchemin>.txt source sink graphs/<monchemin>.dot
     dot -Tsvg graphs/<monchemin>.dot > graphs/<monchemin>.svg
  *)
 
  ()