open Graph.Pack.Digraph;;
(* print_sommets v list -> label list *)
let rec print_sommets l =
  match l with
  | [] -> []
  | t::q -> (V.label t)::(print_sommets q);;

(* affiche la trace *)
let rec print_trace trace =
    match trace with
    | [] -> []
    | t::q -> (print_sommets t)::(print_trace q);;

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

 
(* entrees: 
   - un nombre entier de ressources r
   - un DAG
   sorties:
   - une trace d'execution du DAG 
   specifs: 
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous n'utiliserez pas d'heuristique
   *)
let rec ordonnanceur_aux r dag lv_tp ll lc = 
  match lv_tp with 
  | [] -> ll@[lc]
  | t::q -> if ((appartient (pred dag t) (List.flatten ll)) && ((List.length lc)<r)) then
                ordonnanceur_aux r dag q ll (lc@[t])
            else
                ordonnanceur_aux r dag q (ll@[lc]) [t];;

let ordonnanceur_sans_heuristique r dag = 
  let lv_tp = tri_topologique dag in
    ordonnanceur_aux r dag lv_tp [] [];;
