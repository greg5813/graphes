#use "utile.ml";;

(* tri_topologique_aux DAG.t ->DAG.vertex list -> DAG.vertex list -> DAG.vertex list
   effectue l'algorithme de tri topologique a l aide d une liste utilisee comme file
   et ajoute le prochain sommet a numeroter dans z a chaque appel.
   Retourne la liste z, une fois qu il n y a plus de sommet a numeroter dans y 
   parametres :
     - dag : le DAG sur lequel on applique l algorithme
     - y : liste de sommets qui peuvent etre numerotes
     - z : liste des sommets qui sont deja numerotes (par leur emplacement dans z)
   retour : liste de tous les sommets du DAG qui sont numerotes (par leur emplacement dans z)
*)
let rec tri_topologique_aux dag y z=
    match y with
    | [] -> z
    | t::q -> tri_topologique_aux dag 
                   (fold_succ 
		       (fun v laux -> if( appartient (pred dag v) (z@[t]))
			              then (laux@[v]) 
                                      else laux
                       ) dag t q
                   ) (z@[t]);;

(* tri_topologique DAG.t -> DAG.vertex list
   initialise les listes, y avec tous les sommets qui n ont pas de predecesseurs
   et z a la liste vide. Puis appelle la fonction tri_topologique_aux. 
   parametre :
     - dag : le DAG sur lequel on applique l algorithme
   retour : liste de tous les sommets du DAG qui sont numerotes (par leur emplacement dans z)
*)
let tri_topologique dag = 
    let y = fold_vertex (fun v l -> if ( (in_degree dag v)==0) 
				     then l@[v] else l) dag [] in
    let z = [] in
    tri_topologique_aux dag y z;;

(* ordonnanceur_sans_heuristique_aux int -> DAG.t -> DAG.vertex list -> DAG.trace -> DAG.vertex list -> DAG.trace
   cree la trace d execution du DAG avec r ressources
   parametres :
     - r : le nombre de ressources disponibles
     - dag : le DAG dont on veut connaitre la trace d execution
     - lv_tp : liste des sommets a executer
     - ll : trace deja executee
     - lc : liste des taches qui seront executees a la prochaine etape
   retour : la trace d execution de tout le DAG
*)
let rec ordonnanceur_sans_heuristique_aux r dag lv_tp ll lc = 
  match lv_tp with 
  | [] -> ll@[lc]
  | t::q -> if ((appartient (pred dag t) (List.flatten ll)) && ((List.length lc)<r)) then
                ordonnanceur_sans_heuristique_aux r dag q ll (lc@[t])
            else
                ordonnanceur_sans_heuristique_aux r dag q (ll@[lc]) [t];;

(* ordonnanceur_sans_heuristique int -> DAG.t -> DAG.trace
   initialise la liste des sommets a executer avec la liste des sommets du DAG tries topologiquement,
   les deux autres listes sont initialisees a vide car rien n est encore construit.
   Puis appelle la fonction ordonnanceur_sans_heuristique_aux.
   parametres :
     - r : le nombre de ressources disponibles
     - dag : le DAG dont on veut connaitre la trace d execution
   retour : la trace d execution de tout le DAG
*)
let ordonnanceur_sans_heuristique r dag = 
  let lv_tp = tri_topologique dag in
  ordonnanceur_sans_heuristique_aux r dag lv_tp [] [];;

(* ordonnanceur_avec_heuristique_aux int -> DAG.t -> DAG.vertex list -> trace -> trace
   cree la trace d execution du DAG avec r ressources en executant des que possible, les taches 
   les plus eloignees des puits
   parametres :
     - r : le nombre de ressources disponibles
     - dag : le DAG dont on veut connaitre la trace d execution
     - l_aux : liste de sommets a executer
     - ll : trace deja executee
   retour : la trace d execution de tout le DAG avec l heuristique choisie
*)
let rec ordonnanceur_avec_heuristique_aux r dag l_aux ll =
    if ((List.length l_aux) = 0) 
    then ll
    else
      let l = k_elem (extraction dag l_aux ll []) r [] in 
      ordonnanceur_avec_heuristique_aux r dag (retirer l l_aux) (ll@[l]);;

(* ordonnanceur_avec_heuristique int -> DAG.t -> trace
   marque le DAG avec la distance aux puits, puis initialise la liste de sommets a executer
   avec la liste des sommets du DAG tries topologiquement.
   Puis appelle la fonction ordonnanceur_avec_heuristique_aux 
   parametres :
     - r : le nombre de ressources disponibles
     - dag : le DAG dont on veut connaitre la trace d execution
   retour : la trace d execution de tout le DAG avec l heuristique choisie
*)
let ordonnanceur_avec_heuristique r dag =
    marquage dag;
    let l_topo = tri_topologique dag in
    ordonnanceur_avec_heuristique_aux r dag l_topo [];; 
    
(* ordonnanceur_contrainte_memoire_aux int -> int -> DAG.t -> DAG.vertex list -> trace -> trace
   cree la trace d execution du DAG avec r ressources et une memoire de taille m
   en executant des que possible, les taches les plus eloignees des puits
   parametres :
     - r : le nombre de ressources disponibles
     - m : le nombre de memoire disponible
     - dag : le DAG dont on veut connaitre la trace d execution
     - l_aux : liste de sommets a executer
     - ll : trace deja executee
   retour : la trace d execution de tout le DAG avec l heuristique choisie 
	    en tenant compte de la memoire

*)
let rec ordonnanceur_contrainte_memoire_aux r m dag l_aux ll =
    if ((List.length l_aux) = 0) 
    then ll
    else
      let l = retirer_elem_memoire (extraction dag l_aux ll []) r m [] in 
      ordonnanceur_contrainte_memoire_aux r m dag (retirer l l_aux) (ll@[l]);;

(* ordonnanceur_contrainte_memoire int -> int -> DAG.t -> trace
   marque le DAG avec la distance aux puits, puis initialise la liste de sommets a executer
   avec la liste des sommets du DAG tries topologiquement.
   Puis appelle la fonction ordonnanceur_avec_heuristique_aux 
   parametres :
     - r : le nombre de ressources disponibles
     - m : le nombre de memoire disponible
     - dag : le DAG dont on veut connaitre la trace d execution
   retour : la trace d execution de tout le DAG avec l heuristique choisie 
            en tenant compte de la memoire
*)
let ordonnanceur_contrainte_memoire r m dag =
    iter_vertex (fun v -> if ((Vertex.memory (V.label v)) > m) then failwith ("Memoire insuffisante") else ()) dag;
    marquage dag;
    let l_topo = tri_topologique dag in
    ordonnanceur_contrainte_memoire_aux r m dag l_topo [];;

(* ordonnanceur_contrainte_memoire_bonus int -> int -> DAG.t -> trace
   marque le DAG avec le nombre de dependances, puis initialise la liste de sommets a executer
   avec la liste des sommets du DAG tries topologiquement.
   Puis appelle la fonction ordonnanceur_contrainte_memoire_aux
   parametres :
     - r : le nombre de ressources disponibles
     - m : le nombre de memoire disponible
     - dag : le DAG dont on veut connaitre la trace d execution
   retour : la trace d execution de tout le DAG avec la nouvelle heuristique choisie 
            en tenant compte de la memoire
*)
let ordonnanceur_contrainte_memoire_bonus r m dag =
    iter_vertex (fun v -> if ((Vertex.memory (V.label v)) > m) then failwith ("Memoire insuffisante") else ()) dag;
    marquage_bonus dag;
    let l_topo = tri_topologique dag in
    ordonnanceur_contrainte_memoire_aux r m dag l_topo [];;

