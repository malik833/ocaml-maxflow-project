open Graph

(* FONCTIONS AUXILIAIRES *)
val string_of_list_of_arcs: id arc list -> string
val string_of_list_of_nodes: id list -> string
val string_of_flow_capacity: id * id -> string
val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph

(* FONCTIONS BRIQUES DE L'ALGORITHME FORD-FULKERSON *)
val find_min_lbl: id arc list -> id
val apply_changes: id graph -> id arc list -> id graph
val find_edge_path: 'a graph -> id list option -> 'a arc list
val find_path: id graph -> id list -> id -> id -> id list option

(* FONCTIONS SUPPLEMENTAIRES : CALCULS DE COUPE ST *)
val st_coupe: id graph -> id -> id list
val t_coupe: id graph -> id list -> id list

(* FONCTIONS DE TRANSFORMATION DE GRAPHE *)
type flow_capacity = id * id 
val to_flow_form : id graph -> id graph -> flow_capacity graph

(* FONCTIONS PRINCIPALES : FOLD-FULKERSON*)
val ford_fulkerson : id graph -> id -> id -> id graph 
val flot_max: id graph -> id graph  -> id -> id
val flot_min: id graph -> id graph -> id -> id
