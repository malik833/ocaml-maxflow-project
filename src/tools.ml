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