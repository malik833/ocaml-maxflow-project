open Graph 

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


(* trouver la capacité (label) min d'une liste d'arcs : retourne la capacité min trouvée *)
let find_min_lbl l = 
    let rec find_min acc = function
        | [] -> acc 
        | arc :: next -> if arc.lbl < acc then find_min arc.lbl next else find_min acc next
    in
        find_min 10000 l 
  
(* appliquer les changements de flots au graphe résiduel : retourne le nouveau graphe modifié -> utiliser gmap*)
let apply_changes gr_res edges_path = 
    let min_lbl = find_min_lbl edges_path in
        (* fonction auxiliaire : pour tous les arcs de edge_path, ajouter un arc de tgt vers src avec min_lbl et ajouter un arc de src vers tgt avec lbl-min_lbl (en parcourant edge_path ?) *)
        let g new_gr_res arc = 
            (*si l'arc est dans la liste des arcs du chemin, appliquer la transformation*)
            if List.mem arc edges_path then
                add_arc (add_arc new_gr_res arc.tgt arc.src min_lbl) arc.src arc.tgt (- min_lbl)
            else new_gr_res 
        in
            e_fold gr_res g gr_res 

let rec find_edge_path gr path acc = 
    match path with 
        | None -> []
        | Some path ->
            match path with 
                | _ :: [] | []  -> acc
                | id1 :: id2 :: rest -> match (find_arc gr id1 id2) with 
                                        | Some arc -> find_edge_path gr (Some (id2 :: rest)) (arc :: acc)
                                        | None -> [] 
let string_of_arc arc = "{"^ string_of_int arc.src ^"->"^ string_of_int arc.tgt ^" : "^ string_of_int arc.lbl ^"}"
let string_of_list_of_arcs l = 
        let rec aux l acc = 
            match l with 
                | [] -> acc ^ "]"
                | [arc] -> acc ^ (string_of_arc arc) ^ "]"
                | arc :: rest -> aux rest (acc ^ (string_of_arc arc) ^ "; ")
        in
            aux l "["

let string_of_list_of_nodes l = 
    let rec aux l acc = 
        match l with 
            | [] -> acc ^ "]"
            | [id] -> acc ^ (string_of_int id) ^ "]"
            | id :: rest -> aux rest (acc ^ (string_of_int id) ^ "; ")
    in
        aux l "["

(*type path = id list*)
let list_of_neighbors gr id = 
    match (out_arcs gr id) with 
        | [] -> []
        | arcs -> List.map (fun arc -> arc.tgt) arcs

let rec find_path gr forbidden id1 id2 = 
    if List.mem id2 forbidden then None
    else match (find_arc gr id1 id2 ) with 
       | Some _ -> Some [id1;id2]
       | None -> let neighbors = list_of_neighbors gr id1 in
                    let rec aux = function 
                        | [] -> None
                        | neighbor :: rest -> match (find_path gr (id1 :: forbidden) neighbor id2) with 
                                                | Some path -> Some (id1 :: path)
                                                | None -> aux rest
                    in
                        aux neighbors


let algo gr_originel = apply_changes (gr_originel) (find_edge_path gr_originel (find_path gr_originel [] 0 3) [])


(*let find_path (gr: 'a graph) s t = 
(* algo qui permet de trouver un chemin vers le puit et qui doit renvoyer la liste des arcs empruntés*-> ne pas repasser vers les mêmes noeuds avec un mem*)

    (* renvoie None si l'arc ne va pas au puit et some arc si oui*)
    let is_t_here edge = edge.tgt = t && edge.lbl != 0 in

    (* algo récursif qui renvoie un arc en option comportant un chemin vers t sinon None*)
    let find_t out_edges = (List.find_opt (is_t_here) out_edges)
        (* match (List.find_opt (is_t_here) out_edges) with*)
        (* match (List.fold_left is_t_here None out_edges) with *)        
    in 
    let rec find_path_to_t gr node edge_path = 
        match (find_t (out_arcs gr node)) with 
            | Some edge -> Some (edge :: edge_path)
            | None -> match (out_arcs gr node) with 
               | edge :: rest -> essayer de penser autrement on tourne en rond la*)


    (* !!! commencer avec un graphe residuel de flots = capacités (avec réseau de flots f(u,v)=0)*)
    (* on travaille surtout sur le graph d'écart *)
    (* être capable de trouver une chemin (avec une fonction) entre source et puit dans le graphe résiduel: 

    étapes de construction de l'algo FF : 
    - fonction qui trouve un chemin de s vers t dans n'importe quel graphe (gr ou rzo flot) et qui renvoie la capacité min des tuyaux empruntés
    - un nouveau type ou structure ? qui permette de représenter le flot (initié à 0 dans rzo) -> ou  alors créer un graphe nul résiduel avec les même sommets 
    - algo qui cherche un chemin depuis le puit jusqu'a la source en prenant en compte les capacités des arcs ( -> fonction pour trouver le min de capacité pendant le chemin)
    - graphe résiduel : 
        1) dès qu'un chemin entre u et v trouvé avec flot f(u,v) -> on rajoute un arc de v vers u de f(u,v) et on rajoute un arc de u vers v avec c(u,v)-f(u,v)
        2) à partir du graphe résiduel, trouver un chemin de s à t avec minc = min{c(u,v)} (pour tous les u v par lesquels on passe)
        [3) appliquer le +minc à tous les flots du réseau de flot f(u,v) concernés par le chemin (et -minc si le flot va de v vers u)] -> pas nécessaire de travailler sur le rzo
        4) réitérer jusquèa trouver le flot max (en changeant gr puis rzo) sur UN AUTRE CHEMIN (comment mémoriser les chemins emprunté ? avec une liste)
    - une manière de s'assurer que l'algo s'arrête (qu'on a atteint le flot max : tant qu'il existe un chemin dans le graphe résiduel -> avec une capacité de st coupe 
    (apparaît quand plus de chemins de s vers t dans gr) avec goulot d'étranglement = au flot sur lequel on s'arrête -> à faire si on a le temps pour plus de points)

   *)