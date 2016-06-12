open DAG
#use "dag_test.ml";;
#use "utile.ml";;

(* creation de liste pour les tests *)
let v = [];;
let l1 = [1;5;4;7;3];;
let l2 = [1;5;9;8;3;7;4];;

(* tests de la fonction appartient_aux *)
(appartient_aux 8 v) = false;;
(appartient_aux 8 l1) = false;;
(appartient_aux 4 l1) = true;;

(* tests de la fonction appartient *)
(appartient [] l1) = true;;
(appartient l1 l2) = true;;
(appartient l2 l1) = false;;

(* tests de la fonction retirer_aux *)
(retirer_aux 8 v) = [];;
(retirer_aux 8 l1) = l1;;
(retirer_aux 4 l1) = [1;5;7;3];;

(* tests de la fonction retirer *)
(retirer v l1) = l1;;
(retirer l1 l2) = [9;8];;
(retirer l2 l1) =v;;

(* tests de la fonction ajouter *)
(ajouter 2 v) = [2];;
(ajouter 2 l1) = [1;5;4;7;3;2];;
(ajouter 5 l1) = l1;;

(* tests de la fonction k_elem *)
(k_elem v 3 []) = v;;
(k_elem l1 0 []) = v;;
(k_elem l2 3 []) = [1;5;9];;

(* creation de vertex + marquage + creation de liste de vertex pour les tests *)
let va = V.create("a",2);;
let vb = V.create("b",1);;
let vc = V.create("c",2);;
let vd = V.create("d",1);;
let ve = V.create("e",2);;

Mark.set va 2;;
Mark.set vb 0;;
Mark.set vc 1;;
Mark.set vd 0;;
Mark.set ve 0;;

let lv1 = [va;vc;vd];;
let lv2 = [va;vd;vb;ve];;

(* creation d un petit DAG de test *)
let dtest = create();;
add_vertex dtest va;;
add_vertex dtest vb;;
add_vertex dtest vc;;
add_vertex dtest vd;;
add_vertex dtest ve;;
add_edge dtest va vc;;
add_edge dtest va vd;;
add_edge dtest vc vb;;
add_edge dtest vc ve;;
dot_output dtest "dtest.dot";;

(* tests de la fonction inserer_triee *)
(inserer_trie va v) = [va];;
(inserer_trie ve lv1) = [va;vc;vd;ve];;
(inserer_trie vc lv2) = [va;vc;vd;vb;ve];;

(* tests de la fonction extraction *)
(extraction dtest [vd;vc;vb;ve] [[va]] []) = [vc;vd];;
(extraction dtest [vd;vb;ve] [[va];[vc]] []) = [vd;vb;ve];;
 
(* tests de la fonction retirer_elem_memoire *)
(retirer_elem_memoire lv1 2 3 []) = [va;vd];;
(retirer_elem_memoire lv2 3 5 []) = [va;vd;vb];;

(* tests de la fonction maxi_marquage *)
(maxi_marquage dtest vc) = 0;;
(maxi_marquage dtest va) = 1;;

(* tests de la fonction dependances *)
(dependances dtest vc []) = [vb;ve];;
(dependances dtest va []) = [vc;vb;ve;vd];;

(* tests de la fonction marquage *)
marquage dag1;;
print_marquage dag1;;
marquage dag2;;
print_marquage dag2;;
marquage dag3;;
print_marquage dag3;;
marquage dag4;;
print_marquage dag4;;

(*tests de la fonction marquage_bonus *)
marquage dag1;;
print_marquage dag1;;
marquage dag2;;
print_marquage dag2;;
marquage dag3;;
print_marquage dag3;;
marquage dag4;;
print_marquage dag4;;

