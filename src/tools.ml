open Graph 

let clone_nodes (gr: 'a graph) = function 
    | [] -> []
    | (id, arc) :: next -> (id, []) :: clones_nodes next

let gmap (gr: 'a graph) f = function 
    | [] -> []
    | (id, arc) :: next -> (id, f arc) :: clones_nodes next

let add_arc g id1 id2 n = function 
| (id,_) :: []  -> (id,_) :: (id,{id1; id2; n}) :: [] 
| (id,{src; tgt; lbl}) :: next -> 
    if ((src == id1 && tgt ==id2 ) || (src == id2 && tgt ==id1))
    then  (id,{src;tgt: lbl + n}) ::  next
    else (id,{src; tgt; lbl}) :: add_arc next id1 id2 n ;;


(* il faut les tester : *)
