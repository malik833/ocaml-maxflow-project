open Graph 

let clone_nodes (gr: 'a graph) = n_fold gr new_node empty_graph 

let gmap (gr: 'a graph) f = 
    let g gr arc = new_arc gr {arc with lbl = f arc.lbl} in
    e_fold gr g (clone_nodes gr)

let add_arc g id1 id2 n =  
    if (node_exists g id1) && (node_exists g id2) then
        match (find_arc g id1 id2)  with 
            | None -> new_arc g {src = id1; tgt = id2; lbl = n} 
            | Some arc ->  new_arc g {arc with lbl = n + arc.lbl} 
    else g

    (* on travaille surtout sur le graph d'écart *)
    (* être capable de trouver une chemin (avec une fonction) entre source et puit dans le graphe résiduel: 

    étapes de construction de l'algo FF : 
    - fonction qui repère/instancie la source et puit ?
    - fonction qui trouve un chemin de s vers t dans n'importe quel graphe (gr ou rzo flot) et qui renvoie la capacité min des tuyaux empruntés
    - un nouveau type ou structure ? qui permette de représenter le flot (initié à 0 dans rzo) -> ou  alors créer un graphe nul résiduel avec les même sommets 
    - algo qui cherche un chemin depuis le puit jusqu'a la source en prenant en compte les capacités des arcs ( -> fonction pour trouver le min de capacité pendant le chemin)
    - graphe résiduel : 
        1) dès qu'un chemin entre u et v trouvé avec flot f(u,v) -> on rajoute un arc de v vers u de f(u,v) et on rajoute un arc de u vers v avec c(u,v)-f(u,v)
        2) à partir du graphe résiduel, trouver un chemin de s à t avec minc = min{c(u,v)} (pour tous les u v par lesquels on passe)
        3) appliquer le +minc à tous les flots du réseau de flot f(u,v) concernés par le chemin (et -minc si le flot va de v vers u)
        4) réitérer jusquèa trouver le flot max (en changeant gr puis rzo) sur UN AUTRE CHEMIN (comment mémoriser les chemins emprunté ? avec une liste)
    - une manière de s'assurer que l'algo s'arrête (qu'on a atteint le flot max : tant qu'il existe un chemin dans le graphe résiduel -> avec une capacité de st coupe 
    (apparaît quand plus de chemins de s vers t dans gr) avec goulot d'étranglement = au flot sur lequel on s'arrête ?)

   *)
