(* entrees: 
   - un DAG
   sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique 
   specifs: 
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1)
   *)
let rec appartient_aux x l =
    match l with
    | [] -> false
    | t::q -> (t==x) || appartient_aux x q;;
let rec appartient l1 l2 =
    match l1 with
    | [] -> true
    | t1::q1 -> (appartient_aux t1 l2) && (appartient q1 l2);;

let rec tri_aux dag y z=
    match y with
    | [] -> z
    | t::q -> tri_aux dag (fold_succ (fun v laux -> if( appartient (pred dag v) (z@[t]))
						    then (laux@[v]) else laux) dag t q) (z@[t]);;
let tri_topologique dag = 
    let y = fold_vertex (fun v l -> if ( (in_degree dag v)==0) 
				     then l@[v] else l) dag [] in
    let z = [] in
    tri_aux dag y z;;

 


