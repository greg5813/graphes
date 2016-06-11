
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

let print_marquage dag = fold_vertex (fun v l -> (Mark.get v, V.label v)::l) dag [];;

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
let rec ordonnanceur_sans_heuristique_aux r dag lv_tp ll lc = 
  match lv_tp with 
  | [] -> ll@[lc]
  | t::q -> if ((appartient (pred dag t) (List.flatten ll)) && ((List.length lc)<r)) then
                ordonnanceur_sans_heuristique_aux r dag q ll (lc@[t])
            else
                ordonnanceur_sans_heuristique_aux r dag q (ll@[lc]) [t];;

let ordonnanceur_sans_heuristique r dag = 
  let lv_tp = tri_topologique dag in
    ordonnanceur_sans_heuristique_aux r dag lv_tp [] [];;


let rec inserer_trie x l =
  match l with
  | [] -> [x]
  | t::q -> if ((Mark.get t) < (Mark.get x))
	    then x::l
            else t::(inserer_trie x q);;

let rec extraction dag l l_exec l_res =
    match l with
    | [] -> l_res
    | t::q -> if(appartient (pred dag t) (List.flatten l_exec))
              then extraction dag q l_exec (inserer_trie t l_res)
              else l_res;;

let rec retirer_aux x l =
    match l with
    | [] -> []
    | t::q -> if (t=x) then q else t::(retirer_aux x q);;

let rec retirer l1 l2 =
    match l1 with 
    | [] -> l2
    | t1::q1 -> retirer q1 (retirer_aux t1 l2);;



let maxi_marquage dag vertex = fold_succ (fun v maxi -> max (Mark.get v) maxi) dag vertex 0;;

let rec marquage_aux dag l1 l2 =
    let m = ref 0 in 
    match l1 with
    | [] -> ()
    | t::q -> begin
                m:=maxi_marquage dag t;
		Mark.set t (!m+1);
                marquage_aux dag (fold_pred (fun v laux -> if( appartient (succ dag v) (l2@[t]))
						    then (laux@[v]) 
                                                    else laux) dag t q) (l2@[t])
              end;; 
let marquage dag =
    Mark.clear dag;
    let puits = fold_vertex (fun v l -> if ( (out_degree dag v)==0) 
				     then l@[v] else l) dag [] in
    marquage_aux dag puits [];;
    

let rec ordonnanceur_contrainte_memoire_aux r dag lv_tp ll lc = 
  match lv_tp with 
  | [] -> ll@[lc]
  | t::q -> if ((appartient (pred dag t) (List.flatten ll)) && ((List.length lc)<r)) then
                ordonnanceur_contrainte_memoire_aux r dag q ll (lc@[t])
            else
                ordonnanceur_contrainte_memoire_aux r dag q (ll@[lc]) [t];;

let ordonnanceur_contrainte_memoire r dag = 
  let lv_tp = tri_topologique dag in
    ordonnanceur_contrainte_memoire_    aux r dag lv_tp [] [];;


