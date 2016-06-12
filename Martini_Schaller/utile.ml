(********************** fonctions sur les listes  *****************)

(* appartient_aux 'a -> 'a list -> bool
   verifie si x appartient a l
   parametres :
     - x : element dont on veut verifier l appartenance
     - l : liste dans laquelle on verifie l appartenance
   retour : booleen signifiant l appartenance de x a la liste l
*)
let rec appartient_aux x l =
    match l with
    | [] -> false
    | t::q -> (t==x) || appartient_aux x q;;

(* appartient 'a list -> 'a list -> bool
   verifie si tous les elements de l1 appartiennent a l2
   parametres :
     - l1 : liste des elements dont on veut verifier l appartenance
     - l2 : liste dans laquelle on verifie l appartenance
   retour :
     - true si tous les elements de la liste l1 appartiennent a l2
     - false sinon 
*)
let rec appartient l1 l2 =
    match l1 with
    | [] -> true
    | t1::q1 -> (appartient_aux t1 l2) && (appartient q1 l2);;

(* retirer_aux 'a -> 'a list -> 'a list
   retire un element d une liste
   parametres :
     - x : element a retirer
     - l : liste dans laquelle on retire
   retour : liste a laquelle on a retire l element x 
*)
let rec retirer_aux x l =
    match l with
    | [] -> []
    | t::q -> if (t=x) then q else t::(retirer_aux x q);;

(* retirer_aux 'a list -> 'a list -> 'a list
   retire tous les elements d une liste a une autre liste
   parametres :
     - l1 : liste des elements a retirer
     - l2 : liste dans laquelle on retire
   retour : liste a laquelle on a retire tous les elements de l1
*)
let rec retirer l1 l2 =
    match l1 with 
    | [] -> l2
    | t1::q1 -> retirer q1 (retirer_aux t1 l2);;

(* ajouter 'a -> 'a list -> 'a list
   ajoute un element a une liste que s il n est pas deja dans la liste
   parametres :
     - x : element que l on veut ajouter
     - l : liste dans laquelle on veut ajouter l element
   retour : liste dans laquelle on a ajoute l element s il n y etait pas deja
*)
let rec ajouter x l =
    match l with
    | [] -> [x]
    | t::q -> if(t=x) then l else t::(ajouter x q);;

(* k_elem 'a list -> int -> 'a list -> 'a list
   recupere les k premiers elements d'une liste. Garde l ordre de
   la liste dont on extrait les elements
   parametres :
     - l : liste dont on veut recuperer les elements
     - k : le nombre d elements qu on souhaite recuperer
     - l_res : liste des elements qui ont deja ete recupere
   retour : liste des k premiers elements 
*)
let rec k_elem l k l_res =
    if k = 0 then l_res
    else
      match l with
      | [] -> l_res
      | t::q -> k_elem q (k-1) (l_res@[t]);;

(***************** fonction d affichage **************************)

(* print_sommets DAG.vertex list -> V.label list 
   affiche une liste de labels correspondant a la liste de sommets
   parametre :
     - l : liste de sommets dont on veut afficher les labels
   retour : liste de labels correspondant aux sommets passes en parametre
*)
let rec print_sommets l =
  match l with
  | [] -> []
  | t::q -> (V.label t)::(print_sommets q);;

(* print_trace DAG.vertex list list -> V.label list list
   affiche la trace a l aide des labels 
   parametre : 
     - trace : trace que l on souhaite afficher
   retour : la trace sous forme de labels et non de vertex 
*)
let rec print_trace trace =
    match trace with
    | [] -> []
    | t::q -> (print_sommets t)::(print_trace q);;

(* print_marquage DAG.t -> (int * V.label) list
   affiche pour chaque sommet d un DAG le marquage et le label du sommet 
   parametre :
     - dag : DAG dont on veut afficher le marquage de chaque sommet
   retour : liste de marquages associes a leur label
*)
let print_marquage dag = 
    fold_vertex (fun v l -> (Mark.get v, V.label v)::l) dag [];;

(**************** fonctions auxiliaire sur les DAG  ***********************************)

(* inserer_trie DAG.vertex -> DAG.vertex list -> DAG.vertex list
   insere un sommet dans une liste pour que les sommets soient ranges dans cette liste
   par ordre decroissant de leur marquage
   parametres :
     - x : sommet que l on veut ajouter
     - l : liste de sommets dans laquelle on ajoute le sommet
   retour : liste dans laquelle x a ete ajoute de facon a respecter l ordre sur la liste 
*) 
let rec inserer_trie x l =
  match l with
  | [] -> [x]
  | t::q -> if ((Mark.get t) < (Mark.get x))
	    then x::l
            else t::(inserer_trie x q);;

(* extraction DAG.t -> DAG.vertex list -> trace -> DAG.vertex list -> DAG.vertex list
   extrait la liste des taches executables a une etape en fonction de celle qui sont deja executees 
   (contenu dans la trace)
   parametres :
     - dag : le DAG sur lequel on travaille
     - l : liste de sommets de laquelle on veut extraire les taches executables
     - trace : trace contenant tous les taches deja executees
     - l_res : liste dans laquelle on range les taches extraites qui seront rangees 
               par ordre decroissant de marquage.
   retour : liste de toutes taches extraites de l
*)
let rec extraction dag l trace l_res =
    match l with
    | [] -> l_res
    | t::q -> if(appartient (pred dag t) (List.flatten trace))
              then extraction dag q trace (inserer_trie t l_res)
              else extraction dag q trace l_res;;

(* retirer_elem_memoire DAG.vertex list -> int -> int -> DAG.vertex list -> DAG.vertex list
   extrait les k premiers sommets d'une liste respectant la contrainte memoire
   parametres :
     - l : liste de sommets a traiter
     - k : nombre d elements a extraire
     - m : nombre de memoire disponible
     - l_res : liste des sommets qui sont deja extraits
   retour : liste des k premiers sommets respectant la contrainte memoire. 
            l ordre de la liste passe en parametre est conserve
*) 
let rec retirer_elem_memoire l k m l_res =
    if ((k=0) || (m=0)) then l_res
    else
      match l with
      | [] -> l_res
      | t::q -> let memory = Vertex.memory (V.label t) in
                if (memory<=m) then retirer_elem_memoire q (k-1) (m-memory) (l_res@[t])
                else retirer_elem_memoire q k m l_res;;

(********************** fonctions de marquage sur les DAG ********************************)

(* maxi_marquage DAG.t -> DAG.vertex -> int
   donne le maximum des marquages de tous les successeurs d un sommet
   parametres :
     - dag : DAG sur lequel on travaille
     - vertex : sommet dont on va regarder les successeurs
   retour : le maximum des marquages de successeurs du sommet 
*)
let maxi_marquage dag vertex = 
	fold_succ (fun v maxi -> max (Mark.get v) maxi) dag vertex 0;;

(* marquage_aux DAG.t -> DAG.vertex list -> DAG.vertex list -> unit
   marque tous les sommets d un DAG. Chaque sommet est marque avec la plus grande
   distance de lui meme a un puits
   parametres :
     - dag : le DAG dont on veut numeroter tous les sommets
     - l1 : liste des sommets a qui peuvent etre numerotes
     - l2 : liste des sommets deja numerotes 
*)
let rec marquage_aux dag l1 l2 =
    let m = ref 0 in 
    match l1 with
    | [] -> ()
    | t::q -> begin
		m:=maxi_marquage dag t;
                if ((out_degree dag t)=0) 
		then ()
                else
                   Mark.set t (!m+1);
                marquage_aux dag (fold_pred (fun v laux -> if( appartient (succ dag v) (l2@[t]))
						    then (laux@[v]) 
                                                    else laux) dag t q) (l2@[t])
              end;;

(* marquage DAG.t -> unit
   marque tous les sommets d un DAG selon leur plus grande distance a un puits
   Initialise une liste de puits puis appelle la fonction marquage_aux
   parametre :
     - dag : le DAG dont on veut marquer tous les sommets
*)
let marquage dag =
    Mark.clear dag;
    let puits = fold_vertex (fun v l -> if ( (out_degree dag v)==0) 
				     then l@[v] else l) dag [] in
    marquage_aux dag puits [];;

(* dependances DAG.t -> DAG.vertex -> DAG.vertex list -> DAG.vertex list
   construit la liste des sommets qui dependent du sommet passe en parametre
   parametres :
     - dag : le DAG sur lequelle on travaille
     - v : sommet dont on veut les sommets qui en dependent
     - l_res : liste des sommets qui sont deja consideres comme dependant du sommet v
   retour : liste des sommets qui dependent du sommet passe en parametre
*)
let rec dependances dag v l_res =
  if ((out_degree dag v)=0) then l_res
  else fold_succ (fun v_aux l -> dependances dag v_aux (ajouter v_aux l)) dag v l_res;;
    
(* marquage_bonus DAG.t -> unit
   marque tous les sommets d un dag selon le nombre de sommets qui dependent de lui
   parametre :
     - dag : le DAG dont on veut marquer tous les sommets
*)
let marquage_bonus dag =
    Mark.clear dag;
    iter_vertex (fun v -> let l = dependances dag v [] in
                          Mark.set v (List.length l)) dag;;


