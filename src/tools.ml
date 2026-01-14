open Graph 

(* FONCTIONS AUXILIAIRES *)
let clone_nodes (gr: 'a graph) = n_fold gr new_node empty_graph 

let gmap (gr: 'a graph) f = 
    let g gr arc = new_arc gr {arc with lbl = f arc.lbl} in
    e_fold gr g (clone_nodes gr)

let add_arc (gr: 'a graph) id1 id2 n =  
    if (node_exists gr id1) && (node_exists gr id2) then
        match (find_arc gr id1 id2)  with 
            | None -> new_arc gr {src = id1; tgt = id2; lbl = n} 
            | Some arc ->  new_arc gr {arc with lbl = n + arc.lbl} 
    else gr

(* Fonctions de conversions d'arc en string *)
let string_of_arc arc = "{"^ string_of_int arc.src ^"->"^ string_of_int arc.tgt ^" : "^ string_of_int arc.lbl ^"}"

(* Fonctions de conversions d'une liste d'arcs en string *)
let string_of_list_of_arcs l = 
        let rec aux l acc = 
            match l with 
                | [] -> acc ^ "]"
                | [arc] -> acc ^ (string_of_arc arc) ^ "]"
                | arc :: rest -> aux rest (acc ^ (string_of_arc arc) ^ "; ")
        in
            aux l "["

(* Fonctions de conversions d'une liste de sommets en string *)
let string_of_list_of_nodes l = 
    let rec aux l acc = 
        match l with 
            | [] -> acc ^ "]"
            | [id] -> acc ^ (string_of_int id) ^ "]"
            | id :: rest -> aux rest (acc ^ (string_of_int id) ^ "; ")
    in
        aux l "["

(* Fonction pour transformer un label (flot,capacity) en string*)
let string_of_flow_capacity (f,c) = (string_of_int f)^"/"^(string_of_int c)


(*****************************************************************************)
(* FONCTIONS BRIQUES DE L'ALGORITHME FORD-FULKERSON *)

(* trouver la capacité (label) min d'une liste d'arcs : retourne la capacité min trouvée *)
let find_min_lbl l = 
    let rec find_min acc = function
        | [] -> acc 
        | arc :: next -> if arc.lbl < acc then find_min arc.lbl next else find_min acc next
    in
        find_min 10000 l 
  
(* appliquer les changements de flots au graphe résiduel : retourne le nouveau graphe modifié -> utiliser gmap*)
let apply_changes gr_res edges_path = 
    Printf.printf "Applying changes" ;
    let min_lbl = find_min_lbl edges_path in
        (* fonction auxiliaire : pour tous les arcs de edge_path, ajouter un arc de tgt vers src avec min_lbl et ajouter un arc de src vers tgt avec lbl-min_lbl (en parcourant edge_path ?) *)
        let g new_gr_res arc = 
            (*si l'arc est dans la liste des arcs du chemin, appliquer la transformation*)
            if List.mem arc edges_path then
                add_arc (add_arc new_gr_res arc.tgt arc.src min_lbl) arc.src arc.tgt (- min_lbl)
            else new_gr_res 
        in
            e_fold gr_res g gr_res 

(* Trouver la liste d'arcs correspondant à un chemin de sommets *)
let find_edge_path gr path =
    let rec aux gr path acc = 
        match path with 
            | None -> []
            | Some path ->
                match path with 
                    | _ :: [] | []  -> acc
                    | id1 :: id2 :: rest -> match (find_arc gr id1 id2) with 
                                            | Some arc -> aux gr (Some (id2 :: rest)) (arc :: acc)
                                            | None -> [] 
    in
        aux gr path [] 

(* Fonction qui recherche les sommets voisins du sommet en entrée*)
let list_of_neighbors gr id = 
    match (out_arcs gr id) with 
        | [] -> []
        | arcs -> List.filter_map (fun arc -> if arc.lbl > 0 then Some arc.tgt else None) arcs


(* Fonction de recherche d'un chemin dans le graphe résiduel*)
let rec find_path gr forbidden id1 id2 = 
    Printf.printf "Forbidden : %s" (string_of_list_of_nodes forbidden) ;
    if List.mem id1 forbidden then None
    else 
        let find_arc_ff gr id1 id2 = 
            match (find_arc gr id1 id2 ) with 
                | Some arc -> if arc.lbl > 0 then Some arc else None
                | None -> None 
        in 
       match (find_arc_ff gr id1 id2 ) with 
       | Some arc -> if arc.lbl > 0 then Some [id1;id2] else None
       | None -> let neighbors = list_of_neighbors gr id1 in
                    Printf.printf "Exploring neighbors of %d: %s\n" id1 (string_of_list_of_nodes neighbors);
                    let rec aux = function 
                        | [] -> None
                        | neighbor :: rest -> 
                            Printf.printf "\tTrying neighbor %d from %d to %d\n" neighbor id1 id2;
                            match (find_path gr (id1 :: forbidden) neighbor id2) with 
                                                | Some path -> 
                                                    Printf.printf "\t\tPath found from %d to %d via %d: %s\n" id1 id2 neighbor (string_of_list_of_nodes path);
                                                    Some (id1 :: path)
                                                | None ->
                                                    Printf.printf "\t\t\tNo path found from %d to %d\n" neighbor id2;
                                                    Printf.printf "\t\t\tTrying rest of neighbors\n";
                                                    Printf.printf "\t\t\tRest of neighbors: %s\n" (string_of_list_of_nodes rest);
                                                    Printf.printf "\t\t\tNeighbors: %s\n" (string_of_list_of_nodes neighbors);
                                                    Printf.printf "\t\t\tForbidden: %s\n" (string_of_list_of_nodes (id1 :: forbidden));
                                                    aux rest
                    in
                        aux neighbors




                    
(*****************************************************************************)

(* FONCTIONS SUPPLEMENTAIRES : CALCULS DE COUPE ST *)

(* Fonction de calcul de la somme des capacités d'une liste d'arcs*)
let rec sum acc = function
    | [] -> acc
    | arc :: rest -> sum (acc + arc.lbl) rest

(* Fonction qui vérifie que le puit d'un graphe a ou non des arcs sortants *)
let weird_sink gr id_sink = 
    match (out_arcs gr id_sink) with 
        | [] ->  None
        | arcs -> Some (sum 0 arcs)

(* Fonction qui réalise la st coupe et renvoie les sommets accessibles depuis la source *)
let st_coupe gr id_source = 
    let rec explore gr visited to_visit = 
        match to_visit with 
            | [] -> visited
            | id :: rest -> 
                if List.mem id visited then
                    explore gr visited rest
                else
                    let neighbors = list_of_neighbors gr id in
                    let new_to_visit = rest @ neighbors in
                    explore gr (id :: visited) new_to_visit
    in
        explore gr [] [id_source]
        
(* Fonction qui calcule la somme des capacité d'une liste d'arc pour la S-T coupe (ne prend pas en compte les arc allant vers S)*)
let rec sum_coupe acc s = function
    | [] -> acc
    | arc :: rest -> if List.mem arc.tgt s then sum_coupe acc s rest else sum_coupe (acc + arc.lbl) s rest

(* Fonction qui donne la valeur de la capacité d'une coupe S-T*)
let rec coupe_min gr s visited acc = 
    match s with 
        | [] -> acc 
        | id :: rest -> 
            let out_arcs = out_arcs gr id in
            coupe_min gr rest (id :: visited) (sum_coupe acc (s @ visited) out_arcs)

(* Fonction qui retourne une liste de tous les sommets qui ne sont pas dans la liste d'entrée*) 
let t_coupe gr s = n_fold gr (fun acc node -> if List.mem node s then acc else (node :: acc)) []



(*****************************************************************************)

(* FONCTIONS DE TRANSFORMATION DE GRAPHE *)

type flow_capacity = id * id 

(** Fonction qui donne la capacité d'un arc entre deux sommets, si elle existe et si elle est positive *)
let capacity gr id1 id2 = 
    match (find_arc gr id1 id2) with 
        | None -> None
        | Some arc -> if arc.lbl > 0 then Some arc.lbl else None

(*Fonction our transformer un graphe à double sens à un graphe au capacités en forme f(u,v)/c(u,v)*) 
let to_flow_form gr_initial final_graph =  
    let new_graph = clone_nodes gr_initial in
    let aux new_gr node = 
        let rec transform new_gr node out_arcs = 
            Printf.printf "\nOut arcs of node %d: %s\n" node (string_of_list_of_arcs out_arcs);
            match out_arcs with 
                | [] -> new_gr
                | arc :: rest -> 
                    match (capacity gr_initial arc.tgt arc.src) with
                        | Some capacity -> 
                            Printf.printf "\nCapacity found for arc from %d to %d: %d\n, changes applied with label %d" arc.tgt arc.src capacity arc.lbl;
                            let flot = arc.lbl in
                            if capacity < flot then 
                            transform (new_arc new_gr {src = arc.tgt; tgt = arc.src; lbl = (capacity,capacity)}) node rest
                        else transform (new_arc new_gr {src = arc.tgt; tgt = arc.src; lbl = (flot,capacity)}) node rest
                        | None -> match (capacity gr_initial arc.src arc.tgt) with
                                | None -> 
                                    Printf.printf "\nNo capacity found for arc from %d to %d, no changes applied\n" arc.src arc.tgt;
                                    transform new_gr node rest
                                | Some capacity -> 
                                    Printf.printf "\nCapacity found for arc from %d to %d: %d\n, changes applied" arc.src arc.tgt capacity;
                                    match (find_arc new_gr arc.src arc.tgt) with 
                                        | None ->  transform (new_arc new_gr {src = arc.src; tgt = arc.tgt; lbl = (0,capacity)}) node rest
                                        | Some _ -> transform new_gr node rest
        in
        transform new_gr node (out_arcs final_graph node)
    in 
    n_fold gr_initial aux new_graph



(*****************************************************************************)

(* FONCTIONS PRINCIPALES : FOLD-FULKERSON*)

(* Algorithme de Ford-Fulkerson : renvoie un graphe en doubles flèches*)
let ford_fulkerson gr_source id_source id_sink =
  let rec aux gr_res =
    match find_path gr_res [] id_source id_sink with
    | None -> gr_res
    | Some path ->
        let edges_path = find_edge_path gr_res (Some path) in
        let gr_res_updated = apply_changes gr_res edges_path in
        aux gr_res_updated
  in
  aux gr_source  


(* Fonction de calcul du flot maximum d'un graphe orienté*)
let flot_max gr_initial gr_final id_sink =
    match (weird_sink gr_initial id_sink) with 
        | None -> sum 0 (out_arcs gr_final id_sink)
        | Some flot_initial -> (sum 0 (out_arcs gr_final id_sink)) - flot_initial

(* Fonction qui renvoie la capacité de la ST coupe *)
let flot_min gr_initial gr_final id_source = coupe_min gr_initial (st_coupe gr_final id_source) [] 0
