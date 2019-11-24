open ftest

let test () =
  (* Open file *)
  let graph = from_file infile in


  (* Clone nodes of a graph *)
  (* let () = write_file outfile (clone_nodes graph) in *)

  (* Map a function to all arcs of a graph *)
  (* let () = write_file outfile (gmap graph (fun x -> "test" ^ x)) in *)

  (* Modify the value of an arc if it exists, otherwise a new arc is created *)
  (*let () = write_file outfile (gmap ((add_arc (gmap graph int_of_string) 0 2 10)) string_of_int) in*)

  (* Create a dot graph out of a string graph *)
  let () = export outfile graph in
  (* The following line creates an svg file : 
  dot -Tsvg /graphs/graph2_export.dot > graphs/graph2_export.svg
  *)
  ()