open Gfile
open Tools
open Graph
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (*Printf.printf "=== Début de l'exécution ===\n";
  Printf.printf "Arguments reçus :\n";
  Printf.printf "  - Fichier d'entrée : %s\n" infile;
  Printf.printf "  - Source : %d\n" _source;
  Printf.printf "  - Sink : %d\n" _sink;

  Printf.printf "  - Fichier de sortie : %s\n\n" outfile;*)

  (* Open file *)
  let graph = from_file infile in

  (* récupérer graph et faire les tests des fonctions ici *)
  let int_graph = gmap graph (int_of_string) in

  (* Test de la fonction find_min_lbl*)
  Printf.printf "\n- Test de la fonction find_min_lbl : id arc list-> id (trouver la capacité min d'une liste d'arc)\n
  Résultat appliqué aux arcs sortant du sommet 0 du graphe 1 \n -> doit trouver 7 (capacité de l'arc 0->1)
  : %s\n\n" (string_of_int (find_min_lbl (out_arcs int_graph 0)));
  
  (* Test de la fonction add_changes*)
  Printf.printf "\n- Test de la fonction app_changes : id graph -> id arc list -> id graph (modifier le graphe résiduel en foncion d'un chemin trouvé)\n
  Résultat appliqué au graphe 1, avec la liste d'arcs [0->3,3->4,4->5]:  \n -> doit créer/changer les arcs : 
  \ne 0 3 _ 5 \ne 3 0 _ 5\ne 3 4 _ 0 \ne 5 4 _ 5\ne 4 3 _ 5\ne 4 5 _ 9 \n 
  : *voir graphe ci-dessous \n\n" ;
  (*let changed_graph = apply_changes (int_graph) [{src = 0 ; tgt = 3 ; lbl = 10}; {src = 3 ; tgt = 4 ; lbl = 5}; {src =4; tgt = 5 ; lbl = 14} ] in*)

  (* Test de la fonction find_edge_path*)
  let edge_path = find_edge_path (int_graph) (Some [0;3;4;5]) [] in
  Printf.printf "\n- Test de la fonction find_edge_path : 'a graph -> id list -> 'a arc option list -> 'a arc option list
 (donner la liste d'arcs en fonction d'un chemin de sommets)\n
  Résultat appliqué à une liste de sommets [0,3,4,5] :  \n -> doit renvoyer la liste d'arcs:[0->3,3->4,4->5] \n : %s\n\n" (string_of_list_of_arcs edge_path) ;

  (* Test de la fonction find_path*)
  Printf.printf "\n- Test de la fonction find_path : id graph -> id list -> id -> id -> id list option (trouver un chemin entre deux sommets dans le graphe résiduel)
 \nRésultat appliqué au graphe 1 (trouver chemin entre 0 et 4) \n-> doit renvoyer la liste des sommets du chemin : [0;3;4] \n
  : %s\n" (string_of_list_of_nodes (let aux = function | Some l -> l | None -> [] in aux (find_path int_graph [] 0 2))) ;
  
  (* Test de la fonction algo*)
  Printf.printf "\n- Test de la fonction algo : id graph -> id graph (appliquer les changements de flots au graphe résiduel)
 \nRésultat appliqué au graphe 1 (trouver chemin entre 0 et 3) \n-> doit changer/créer les arcs : \ne 0 3 _ 10 -> e 0 3 _ 5 \nnouvel arc : e 3 0 _ 5\n
  : *voir graphe ci-dessous\n\n" ;
  let new_graph = int_graph in

  let string_graph = gmap new_graph (string_of_int) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile string_graph in

  ()


