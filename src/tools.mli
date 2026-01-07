open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val find_min_lbl: id arc list -> id
val apply_changes: id graph -> id arc list -> id graph
val find_edge_path: 'a graph -> id list option -> 'a arc list -> 'a arc list
val algo: id graph -> id graph
val find_path: id graph -> id list -> id -> id -> id list option
val string_of_list_of_arcs: id arc list -> string
val string_of_list_of_nodes: id list -> string
