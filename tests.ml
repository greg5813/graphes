print_endline "ordonnanceur sans heuristique";
print_newline();
for i=1 to 5 do 
	print_string "  dag1 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_sans_heuristique i dag1));
	print_newline()
done;
print_newline();
for i=1 to 5 do 
	print_string "  dag2 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_sans_heuristique i dag2));
	print_newline()
done;
print_newline();
for i=1 to 5 do 
	print_string "  dag3 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_sans_heuristique i dag3));
	print_newline()
done;
print_newline();
for i=1 to 5 do 
	print_string "  dag4 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_sans_heuristique i dag4));
	print_newline()
done;
print_newline();
print_newline();
print_endline "ordonnanceur avec heuristique";
print_newline();
for i=1 to 5 do 
	print_string "  dag1 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_avec_heuristique i dag1));
	print_newline()
done;
print_newline();
for i=1 to 5 do 
	print_string "  dag2 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_avec_heuristique i dag2));
	print_newline()
done;
print_newline();
for i=1 to 5 do 
	print_string "  dag3 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_avec_heuristique i dag3));
	print_newline()
done;
print_newline();
for i=1 to 5 do 
	print_string "  dag4 r=";
	print_int i;
	print_string " t=";
	print_int (List.length (ordonnanceur_avec_heuristique i dag4));
	print_newline()
done;
print_newline();
print_newline();
print_string "ordonnanceur avec contrainte mémoire";
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag1 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire i j dag1))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag2 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire i j dag2))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag3 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire i j dag3))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag4 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire i j dag4))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
print_newline();
print_string "ordonnanceur avec contrainte mémoire bonus";
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag1 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire_bonus i j dag1))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag2 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire_bonus i j dag2))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag3 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire_bonus i j dag3))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;
print_newline();
for j=1 to 5 do
	for i=1 to 5 do 
		print_string "  dag4 m=";
		print_int j;
		print_string " r=";
		print_int i;
		print_string " t=";
		try 
			print_int (List.length (ordonnanceur_contrainte_memoire_bonus i j dag4))
		with
			MemoireInsuffisante m -> print_string "Mémoire insuffisante : "; print_int m;
		print_newline()
	done;
	print_newline()
done;

