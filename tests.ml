print_endline "ordonnanceur sans heuristique";

print_endline "	dag1";
print_string "	  r=1"; let t = List.length (ordonnanceur_sans_heuristique 1 dag1);;
print_string "	  r=2";; let t = List.length (ordonnanceur_sans_heuristique 2 dag1);;
print_string "	  r=3";; let t = List.length (ordonnanceur_sans_heuristique 3 dag1);;
print_string "	  r=4";; let t = List.length (ordonnanceur_sans_heuristique 4 dag1);;
print_string "	  r=5";; let t = List.length (ordonnanceur_sans_heuristique 5 dag1);;

print_endline "	dag2";;
print_string "	  r=1";; let t = List.length (ordonnanceur_sans_heuristique 1 dag2);;
print_string "	  r=2";; let t = List.length (ordonnanceur_sans_heuristique 2 dag2);;
print_string "	  r=3";; let t = List.length (ordonnanceur_sans_heuristique 3 dag2);;
print_string "	  r=4";; let t = List.length (ordonnanceur_sans_heuristique 4 dag2);;
print_string "	  r=5";; let t = List.length (ordonnanceur_sans_heuristique 5 dag2);;

print_endline " dag3";;
print_string "	  r=1";; let t = List.length (ordonnanceur_sans_heuristique 1 dag3);;
print_string "	  r=2";; let t = List.length (ordonnanceur_sans_heuristique 2 dag3);;
print_string "	  r=3";; let t = List.length (ordonnanceur_sans_heuristique 3 dag3);;
print_string "	  r=4";; let t = List.length (ordonnanceur_sans_heuristique 4 dag3);;
print_string "	  r=5";; let t = List.length (ordonnanceur_sans_heuristique 5 dag3);;

print_endline " dag4";;
print_string "	  r=1";; let t = List.length (ordonnanceur_sans_heuristique 1 dag4);;
print_string "	  r=2";; let t = List.length (ordonnanceur_sans_heuristique 2 dag4);;
print_string "	  r=3";; let t = List.length (ordonnanceur_sans_heuristique 3 dag4);;
print_string "	  r=4";; let t = List.length (ordonnanceur_sans_heuristique 4 dag4);;
print_string "	  r=5";; let t = List.length (ordonnanceur_sans_heuristique 5 dag4);;

print_endline "";;
print_endline "ordonnanceur avec heuristique";;

print_endline "	dag1";;
print_string "	  r=1";; let t = List.length (ordonnanceur_avec_heuristique 1 dag1);;
print_string "	  r=2";; let t = List.length (ordonnanceur_avec_heuristique 2 dag1);;
print_string "	  r=3";; let t = List.length (ordonnanceur_avec_heuristique 3 dag1);;
print_string "	  r=4";; let t = List.length (ordonnanceur_avec_heuristique 4 dag1);;
print_string "	  r=5";; let t = List.length (ordonnanceur_avec_heuristique 5 dag1);;

print_endline "	dag2";;
print_string "	  r=1";; let t = List.length (ordonnanceur_avec_heuristique 1 dag2);;
print_string "	  r=2";; let t = List.length (ordonnanceur_avec_heuristique 2 dag2);;
print_string "	  r=3";; let t = List.length (ordonnanceur_avec_heuristique 3 dag2);;
print_string "	  r=4";; let t = List.length (ordonnanceur_avec_heuristique 4 dag2);;
print_string "	  r=5";; let t = List.length (ordonnanceur_avec_heuristique 5 dag2);;

print_endline " dag3";;
print_string "	  r=1";; let t = List.length (ordonnanceur_avec_heuristique 1 dag3);;
print_string "	  r=2";; let t = List.length (ordonnanceur_avec_heuristique 2 dag3);;
print_string "	  r=3";; let t = List.length (ordonnanceur_avec_heuristique 3 dag3);;
print_string "	  r=4";; let t = List.length (ordonnanceur_avec_heuristique 4 dag3);;
print_string "	  r=5";; let t = List.length (ordonnanceur_avec_heuristique 5 dag3);;

print_endline " dag4";;
print_string "	  r=1";; let t = List.length (ordonnanceur_avec_heuristique 1 dag4);;
print_string "	  r=2";; let t = List.length (ordonnanceur_avec_heuristique 2 dag4);;
print_string "	  r=3";; let t = List.length (ordonnanceur_avec_heuristique 3 dag4);;
print_string "	  r=4";; let t = List.length (ordonnanceur_avec_heuristique 4 dag4);;
print_string "	  r=5";; let t = List.length (ordonnanceur_avec_heuristique 5 dag4);;


print_endline "ordonnanceur avec contrainte mémoire avec r de 1 à 5";;

