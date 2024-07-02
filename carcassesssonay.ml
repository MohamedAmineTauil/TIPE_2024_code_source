type tuiles_t_1 = Route_1 | Ville_1 | Abbaie;;
type tuiles_t_2 = Route_2 | Prairie | Idem;; (*Idem si il fait référence au type précédant*)
type voisinage_tuile = Route_3 | Ville_2 | Prairie_2;;
type direction = Haut | Droite | Bas | Gauche | NAD;;(*NAD = Not A Direction*)
type axe_absc = Droite_axe | Gauche_axe | NAA_X;;(*NAA = Not An Axis*)
type axe_ord = Haut_axe | Bas_axe | NAA_Y;;

type tuiles= direction array * tuiles_t_1 * direction array *  tuiles_t_2 * direction array array;;
type tab_tuiles  = T of tuiles | Vide;;(*tuiles désignés par les tableaux*)





(*fonctions de base et conversions*)

let func_of_arr arr =
	fun k -> if k >= 1 && k <= Array.length arr then 
		arr.(k - 1) else arr.( (Array.length arr )  - 1);;

let num_to_direction num = match ( if num > 0 then ( ( (num - 1) mod 4) + 1 ) else( ( ( (-3)*num + 3 ) mod 4) + 1 ) ) with
	|1 -> Haut
	|2 -> Droite
	|3 -> Bas
	|4 -> Gauche
	|_ -> NAD;;
	
let direction_to_num = function
	|Haut -> 1
	|Droite -> 2
	|Bas -> 3
	|Gauche -> 4;
	|_ -> 0;;
	
let axe_to_dir = function
	|Droite_axe -> Droite
	|Gauche_axe -> Gauche
	|_ -> NAD;;

let ord_to_dir = function
	|Haut_axe -> Haut
	|Bas_axe -> Bas
	|_ -> NAD;;
	
let dir_to_axe = function
	|Droite -> Droite_axe
	|Gauche -> Gauche_axe
	|_ -> NAA_X;;

let dir_to_ord = function
	|Haut -> Haut_axe
	|Bas -> Bas_axe
	|_ -> NAA_Y;;
	
let dir_to_line = function
	|Haut -> (NAA_X,Haut_axe)
	|Droite -> (Droite_axe,NAA_Y)
	|Bas -> (NAA_X,Bas_axe)
	|Gauche -> (Gauche_axe,NAA_Y)
	|_ -> (NAA_X,NAA_Y);;
	
let dir_to_move = function
	|Haut -> (0,1)
	|Droite -> (1,0)
	|Bas -> (0,-1)
	|Gauche -> (-1,0);
	|_ -> (0,0);;
	
let axe_to_move = function
	|Droite_axe -> 1
	|Gauche_axe -> -1;
	|_ -> 0;;
	
let ord_to_move = function
	|Haut_axe -> 1
	|Bas_axe -> -1;
	|_ -> 0;;
	
let demi_tour_axe = function
	|Droite_axe -> Gauche_axe
	|Gauche_axe -> Droite_axe
	|_ -> NAA_X;;
	
let demi_tour_ord = function
	|Haut_axe -> Bas_axe
	|Bas_axe -> Haut_axe
	|_ -> NAA_Y;;
	
let if_axe dir = function
	|Gauche
	|Droite -> true
	|_ -> false;;

let if_ord dir = function
	|Haut
	|Bas -> true 
	|_ -> false;;

let print_dir = function
|Haut -> print_string "dir_Haut"	
|Droite -> print_string "dir_Droite"	
|Bas -> print_string "dir_Bas"	
|Gauche -> print_string "dir_Gauche"	
|_ -> print_string "dir_NAD";;

let print_axe = function
|Gauche_axe -> print_string "axe_Gauche"
|Droite_axe -> print_string "axe_Droite"	
|_ -> print_string "axe_NAA_X";;

let print_ord = function
|Bas_axe -> print_string "axe_Bas"
|Haut_axe -> print_string "axe_Haut"	
|_ -> print_string "axe_NAA_Y";;
	
let add_couple = function (a,b) -> function (c,d) -> (a + c,b + d);;
let subs_couple = function (a,b) -> function (c,d) -> (a - c,b - d);;
let mult_couple = function (a,b) -> function (c,d) -> (a * c,b * d);;
	
let rotation i dir = num_to_direction ( ( ( ( (direction_to_num dir) - 1) + i ) mod 4 ) + 1 );;
let rotation_arr i arr = Array.map (rotation i) arr;;
let rotation_tuile i tuile = match tuile with
	|(dir_1,t_1,dir_2,t_2,dir_3) -> 
	(rotation_arr i dir_1,t_1,rotation_arr i dir_2,t_2,Array.map (rotation_arr i) dir_3);;
let every_dir = Array.init 4 (fun k -> num_to_direction ( k + 1) );;

(*score des terrains*)
let sc_prairie = 3;; 
let sc_route = 1;;
let sc_ville = 2;;
let sc_abbaie = 9;;
let sc_abbaie_par_case = 1;;


(*tuiles du jeu*)

let a = (every_dir,Abbaie,[|Bas|],Route_2,[| [||] |]);;
let b = (every_dir,Abbaie,[||],Idem,[| [||] |]);;
let c = (every_dir, Ville_1, [||], Idem, [| [| |] |]);;
let d = ([|Droite|], Ville_1, [|Haut;Bas|], Route_2 , [| [| |] |]);;
let e = ([|Haut|], Ville_1, [| Droite; Bas; Gauche |],Prairie , [| [| |] |]);;
let f = ([|Droite;Gauche|], Ville_1, [|Haut;Bas|], Prairie, [| [| |] |]);;
let g = ([|Haut; Bas|], Ville_1, [|Droite;Gauche|], Prairie, [|[||]|]);;
let h = ([|Droite;Gauche|], Ville_1, [|Haut;Bas|],Prairie, [| [|Droite|]; [|Gauche|] |]);;(*rotation de g*)
let i = ([|Droite;Bas|], Ville_1, [|Haut;Gauche|], Prairie , [| [|Droite|]; [|Bas |] |]);;
let j = ([|Haut|], Ville_1, [|Droite;Bas|], Route_2, [| [| |] |]);;
let k = ([|Droite|], Ville_1, [|Haut;Gauche|], Route_2, [| [| |] |]);;
let l = ([|Droite|], Ville_1, [|Haut;Bas;Gauche|], Route_2, [| [| |] |]);;
let m = ([|Haut;Gauche|], Ville_1, [|Droite;Bas|], Prairie, [| [| |] |]);;
let n = rotation_tuile 0 m;;
let o = ([|Haut;Gauche|], Ville_1, [|Droite;Bas|], Route_2, [| [| |] |]);;
let p = rotation_tuile 0 o;;
let q = ([|Haut;Droite;Gauche|], Ville_1, [|Bas|], Prairie, [| [| |] |]);;
let r = rotation_tuile 0 q;;
let s = ([|Haut;Droite;Gauche|], Ville_1, [|Bas|], Route_2, [| [| |] |]);;
let t = rotation_tuile 0 s ;;
let u = ([|Haut;Bas|], Route_1, every_dir,Prairie, [| [| |] |]);;
let v = ([|Bas;Gauche|], Route_1, every_dir,Prairie, [| [| |] |]);;
let w = ([|Droite;Bas;Gauche|], Route_1, every_dir,Prairie, [| [| |] |]);;
let x = (every_dir, Route_1, every_dir,Prairie, [| [| |] |]);;



(*fonctions d'enumeration de tuiles(pour ne pas faire des copies inutiles de tuiles de base)*)

let tiles_of_int = function
	|1 -> a |2 -> b |3 -> c |4 -> d |5 -> e |6 -> f
	|7 -> g |8 -> h |9 -> i |10 -> j |11 -> k |12 -> l
	|13 -> m |14 -> n |15 -> o |16 -> p |17 -> q |18 -> r 
	|19 -> s |20 -> t |21 -> u |22 -> v |23 -> w |24 -> x
	
	|_ -> failwith "More tiles coming Soon";;
	
let int_of_tiles = function
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = a) [|0;1;2;3|]-> 1 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = b) [|0;1;2;3|] -> 2 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = c) [|0;1;2;3|] -> 3 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = d) [|0;1;2;3|] -> 4 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = e) [|0;1;2;3|] -> 5 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = f) [|0;1;2;3|] -> 6
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = g) [|0;1;2;3|] -> 7 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = h) [|0;1;2;3|] -> 8 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = i) [|0;1;2;3|] -> 9 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = j) [|0;1;2;3|] -> 10 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = k) [|0;1;2;3|] -> 11 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = l) [|0;1;2;3|] -> 12
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = m) [|0;1;2;3|] -> 13 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = n) [|0;1;2;3|] -> 14 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = o) [|0;1;2;3|] -> 15 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = p) [|0;1;2;3|] -> 16 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = q) [|0;1;2;3|] -> 17 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = r) [|0;1;2;3|] -> 18
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = s) [|0;1;2;3|] -> 19 |t_var when Array.exists (fun i_0 -> (rotation_tuile i_0 t_var) = t) [|0;1;2;3|] -> 20 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = u) [|0;1;2;3|] -> 21 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = v) [|0;1;2;3|]  -> 22 
	|t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = w) [|0;1;2;3|] -> 23 |t when Array.exists (fun i_0 -> (rotation_tuile i_0 t) = x) [|0;1;2;3|] -> 24
	
	|_ -> failwith "Overruled!!!!";;
	
	
	
	
	
	
	(*table de jeu sous forme de "listes chainees"*)

type plateau =Bord | Enregist of {tuile_centrale : tuiles ; haut_p : plateau  ; droite_p : plateau ; bas_p : plateau ;
	gauche_p : plateau ; bords : (int*int) array;};;
	
type plateau_compr = Bord_compr | Enregist_compr of {tuile_centrale : int ; haut_p : plateau_compr
 	 ; droite_p : plateau_compr ; bas_p : plateau_compr ; gauche_p : plateau_compr ; 
 	 bords : (int*int) array;};;
 	 
type sub_plateau = {tuile_centrale : int ; haut_p : plateau  ; droite_p : plateau ; bas_p : plateau ;
	gauche_p : plateau ; bords : (int*int) array;};;
	
type plateau_complet = plateau * (*Reseau_Ville_of *) int array * (* Reseau_Route of *) int array * 
	(*Reseau_Prairie_of *) (axe_absc*axe_ord*int) array;;
	
type plateau_complet_compr = plateau_compr * (*Reseau_Ville_of *) int array * (* Reseau_Route of *) int array * 
	(*Reseau_Prairie_of *) (axe_absc*axe_ord*int) array;;
	
(*table de jeu complet*)
	
	
type plateau_table = tab_tuiles array array ;;

type plateau_table_bordure = plateau_table * (int * int) array;;

type plateau_table_compr = int array array ;;

type plateau_table_bordure_compr = plateau_table_compr * (int * int) array;;







(*Initialisation*)

let init_plateau_table k p tuile = 
	let tab = Array.make_matrix k p Vide in
		tab.(0).(0) <- T(tuile);tab;;
	
let init_plateau_table_bordure k p tuile = 
	let tab = Array.make_matrix k p Vide
	in tab.(0).(0) <- T(tuile);(tab, [|(0,0)|]);;
	
let init_plateau_table_compr k p int_tuile = 
	let tab = Array.make_matrix k p (-1)
	in tab.(0).(0) <- int_tuile;tab;;
	
let init_plateau_table_compr_bordure k p int_tuile = 
	let tab = Array.make_matrix k p (-1)
	in tab.(0).(0) <- int_tuile;(tab, [|(0,0)|]);;
	
let init_plateau tuile = Enregist( { tuile_centrale = tuile; haut_p = Bord;droite_p = Bord;
bas_p = Bord;gauche_p = Bord; bords = [|(0,0)|];} );;
	
	



	
(*fonctions pour gerer les directions*)

rotation_tuile 4 h;;


let not_direction arr = let all_dir = Array.make 4 true
	in Array.iter (fun dir -> if dir != NAD then all_dir.( (direction_to_num dir) - 1) <- false) arr ;
	let size = ref 0 in Array.iter (fun m -> if m then incr size) all_dir; 
	if !size = 0 then [||] else let cpt = ref 0 in 
		Array.init !size (fun k -> if !cpt <= 3 then while not all_dir.(!cpt)
			do incr cpt done;incr cpt; num_to_direction !cpt) ;;
			
not_direction [|Haut; Haut; Bas|];;Array.append [|Haut|] [|Haut;Gauche|];;




	
let dir_prairie (tuile : tuiles) = match tuile with
	|(_,_,dir_2,t_2,_) when t_2 = Prairie -> dir_2
	|(dir_1,t_1,_,_,_) when t_1 = Ville_1 -> not_direction dir_1;
	|_ -> Array.init 4 (fun k -> num_to_direction (k + 1));;
	
let dir_route tuile = match tuile with
	|(dir_1,t_1,_,_,_) when t_1 = Route_1 -> dir_1
	|(_,_,dir_2,t_2,_) when t_2 = Route_2 -> dir_2
	|_ -> [||];;





let extension_prairie tuile axe ord =
	let axe_dir = axe_to_dir axe in let ord_dir = ord_to_dir ord in
	let dir_pr = dir_prairie tuile in let dir_rt = dir_route tuile in
	let nb_dir = Array.length dir_pr in let nb_rt = Array.length dir_rt in
	 match nb_dir with
	 |0 -> [| |]
	 |1 -> if Array.mem dir_pr.(0) [|axe_dir;ord_dir|] then dir_pr else [||]
	 |2 ->if (rotation 2 dir_pr.(0)) = dir_pr.(1) then
			
			match tuile with
				|(_,_,_,_,[| [| |] |]) -> if Array.mem dir_pr.(0) [|Haut;Bas|] then [|ord_dir|]
												else [|axe_dir|]
				|_ -> dir_pr 
			
			else ( (  if ( (not (Array.mem axe_dir dir_pr) ) && (not (Array.mem ord_dir dir_pr ) ) )
			then [||] else dir_pr ) )
	 |_ -> if nb_rt = 4 then [|axe_dir;ord_dir|]  else
				( if (nb_rt = 1 || nb_rt = 0) then dir_pr else
					( if nb_rt = 3 then ( let not_dir = not_direction dir_rt in
						( if not ( Array.mem not_dir.(0) [|axe_dir;ord_dir|] ) then [|axe_dir;ord_dir|] else 
							(if Array.mem not_dir.(0) dir_pr then Array.init 3 (fun k -> rotation (k - 1) not_dir.(0) )
							else [|rotation 1 not_dir.(0); rotation (-1) not_dir.(0) |] ) ) )
						else 
						(
						if rotation 2 dir_rt.(0) = dir_rt.(1) then
							( if Array.mem dir_rt.(0) [|Haut;Bas|] then 
								(if Array.mem axe_dir dir_pr then [|Haut;axe_dir;Bas|] else [|Haut;Bas|] )
							else (if Array.mem ord_dir dir_pr then [|Droite;ord_dir;Gauche|] else [|Droite;Gauche|]  ) )
						else (
						if Array.mem axe_dir dir_rt then
							( if Array.mem ord_dir dir_rt then [|axe_dir;ord_dir|] 
							else ( if nb_dir = 4 then [|axe_dir;ord_dir; (rotation 2 axe_dir);(rotation 2 ord_dir )|] 
							else let blocked = not_direction dir_pr in Array.init 3 (fun k -> rotation (k + 1) blocked.(0) )  ) )
						else 
						( if nb_dir <> 4 then let blocked = not_direction dir_pr in 
							Array.init 3 (fun k -> rotation (k + 1) blocked.(0) )  
						else( if Array.mem ord_dir dir_rt then 
							[|ord_dir;axe_dir; (rotation 2 ord_dir); (rotation 2 axe_dir)|]
						else ( if nb_dir = 4 then [|(rotation 2 axe_dir); ord_dir; axe_dir; (rotation 2 ord_dir)|]
						else let blocked = not_direction dir_pr in Array.init 3 (fun k -> rotation (k + 1) blocked.(0) )  ) ) 
						) )) ));;
					   
Array.init 24 (fun k -> extension_prairie (rotation_tuile 0 ( tiles_of_int (k + 1) ) ) Droite_axe Haut_axe );;
	
	
dir_prairie ( [|Haut;Bas|],Ville_1, [|Droite|], Route_2, [| [||] |] );;
(* else
	( if Array.mem ord_dir dir_rt then [|(axe,ord), (axe_dir, (demi_tour_ord ord ) ) |]
	 else [|(axe_dir,ord_dir), (axe_dir, (rotation 2 ord_dir ) ), ((rotation 2 axe_dir ),ord_dir)  |] ) *)







let extract_from_plateau plat = match plat with
	|Enregist({tuile_centrale = a ; haut_p = b  ; droite_p = c ; bas_p = d ;
	gauche_p = e ; bords = f;}) -> [|a,b,c,d,e,f|]
	|Bord -> [||];;
	
 (* let enreg_plateau = function
	|Enregist(m) -> let [|a,b,c,d,e,f|] = extract_from_plateau ( Enregist(m) ) in [|{tuile_centrale = a; haut_p = b;
	droite_p = c; bas_p = d; gauche_p = e; bords = f;}|]
	|_ -> [||];; *)
let tab_to_plateau tab = match tab with
	|[|a,b,c,d,e,f|] -> Enregist({tuile_centrale = a ; haut_p = b  ; droite_p = c ; bas_p = d ;
	gauche_p = e ; bords = f;})
	|_ -> Bord;;

let deplacement_of plateau plat dir = match (extract_from_plateau plat) with
	|[|_,h_p,d_p,b_p,g_p,_|] -> let tab_dir = [|h_p;d_p;b_p;g_p|] in tab_dir.( (direction_to_num dir) - 1)
	|_ -> failwith "OUT OF BOUNDS!!!!";;

 let replace_in_plateau_p plat val_plat dir = let tab = extract_from_plateau plat in
	if tab = [||] then Bord else match tab.(0) with
	|(a,dir_1,dir_2,dir_3,dir_4,b) -> let arr = [|dir_1;dir_2;dir_3;dir_4|] in arr.( ( direction_to_num dir) - 1) <- val_plat;
 	 tab_to_plateau [|a,(arr.(0)),(arr.(1)),(arr.(2)),(arr.(3)),b|];;
let pl = init_plateau a;;
let k_plat = tab_to_plateau (extract_from_plateau pl);;
let k_plat = replace_in_plateau_p pl k_plat  Droite;;

let cote tuile dir = match tuile with
	|(dir_1,t_1,dir_2,t_2,_) -> if t_1 = Abbaie then ( if Array.mem dir dir_2 then 
		match t_2 with
			|Idem -> Prairie_2
			|Route_2 -> Route_3
			|_ -> Prairie_2
		else Prairie_2) else (*t_1 != Abbaie *)
			(if Array.mem dir dir_1 then ( match t_1 with
			|Route_1 -> Route_3
			|Ville_1 -> Ville_2
			|_ -> Prairie_2 )
		else  ( if Array.mem dir dir_2 then ( match t_2 with
			|Idem -> Prairie_2
			|Route_2 -> Route_3
			|_ -> Prairie_2 ) else Prairie_2) );;
rotation 0 Haut;;
cote j (rotation 3 Haut);;

let est_coup_valide tuile_table tuile_a_deposer dir (*tuile_a_deposer est à dir de tuile_table *) = 
	match (tuile_table,tuile_a_deposer) with
	|( (dir_1,t_1,dir_2,t_2,_),(dir_a,t_a,dir_b,t_b,_) )-> let dir_opp = rotation 2 dir in  
		cote tuile_table dir = cote tuile_a_deposer dir_opp;;
	est_coup_valide j (rotation_tuile 3 k) Haut;;
rotation (-3) Haut;;



let compare_coord_prairie tuile axe_0 ord_0 axe_1 ord_1 =
			
			
			
	if (axe_0,ord_0) = (axe_1,ord_1) then 
		(let dir_pr = dir_prairie tuile in 
			Array.exists (fun dir_0 -> Array.mem dir_0 dir_pr  )  
				[|(axe_to_dir axe_0); (ord_to_dir ord_0) |] )  
	else
	begin
		if (axe_0 = axe_1) then
			( let dir_rt = dir_route tuile in let dir_pr = dir_prairie tuile in 
					if not (Array.mem (axe_to_dir axe_0) dir_rt) then (if Array.mem (axe_to_dir axe_0) dir_pr then true else ( match tuile with
					|(_,_,_,_, [| [||] |]) -> not (Array.mem (rotation 2 (axe_to_dir axe_0)) dir_rt  )
					|_ -> false


					)) else
				(Array.length dir_rt <= 1) && (Array.length (dir_prairie tuile) >= 4 ) )
		else
			begin
				if(ord_0 = ord_1) then
					(let dir_rt = dir_route tuile in let dir_pr = dir_prairie tuile in if not (Array.mem (ord_to_dir ord_0) dir_rt) then (if (Array.mem (ord_to_dir ord_0) dir_pr) then true else ( match tuile with
				|(_,_,_,_, [| [||] |]) -> not (Array.mem (rotation 2 (ord_to_dir ord_0)) dir_rt  )
				|_ -> false


				)) else
						(Array.length dir_rt <= 1) && (Array.length (dir_prairie tuile) >= 4 ) )
				else
					let dir_pr = dir_prairie tuile in match Array.length dir_pr with 
					|0
					|1 -> false
					|nb_pr -> 
					(let dir_rt = dir_route tuile in match Array.length dir_rt with
						|0 -> ( let dir_pr = dir_prairie tuile in match Array.length dir_pr with 
							|2 -> if not (dir_pr.(1) = rotation 2 (dir_pr.(0)) ) then (match dir_pr.(0) with 
																																					|Haut
																																					|Bas -> if dir_pr.(0) = (axe_to_dir axe_0) then not (dir_pr.(1) = (ord_to_dir ord_0)) else dir_pr.(1) = (ord_to_dir ord_0) 
																																					|_ -> if dir_pr.(0) = (ord_to_dir ord_0) then not (dir_pr.(1) = (ord_to_dir ord_0)) else dir_pr.(1) = (ord_to_dir ord_0) )
										else
											(match tuile with
												|(_,_,_,_,cloturage_ville) -> cloturage_ville <> [| [||] |])
							|_  -> true   )
						|1 -> if nb_pr >= 4 then true else (if not (Array.mem (rotation 2 dir_rt.(0)) dir_pr) then false else (nb_pr >= 3 && (not (Array.exists (fun (a,b)-> let c = dir_rt.(0) in let d = (let dir_1_t,_,_,_,_ = tuile in dir_1_t.(0)) in ( (a,b) = (c,d) || (a,b) = (d,c) ) ) [|(axe_to_dir axe_0,ord_to_dir ord_0); (axe_to_dir axe_1,ord_to_dir ord_1)|] )  ) ) ) 
						|2 -> if dir_rt.(1) = rotation 2 dir_rt.(0) then false else not (Array.exists ( fun(a,b) -> let c = dir_rt.(0) in let d = dir_rt.(1) in ((a,b) = (c,d) || (a,b) = (d,c))  ) [|(axe_to_dir axe_0,ord_to_dir ord_0); (axe_to_dir axe_1,ord_to_dir ord_1)|] )
						|_(*trois directions ou plus*) -> false
						)
			end
	end ;;


let reseau_prairie table x y axe ord(*coordonnés, et cote de la prairie*) = let cloture_pr = ref true in
	let visited = Hashtbl.create 72 in let cpt = ref 0 in let nb_etage_recursif = ref (-1) in let coord_preced = ref (-1,-1)  in let lst_x_y = ref [] in
	if (x<0||y<0||x>=Array.length table||y>=Array.length table.(x) ) then ([(*((10,10),(axe,ord))*)],[],visited,false) else
		let rec reseau_prairie_stock table x_propag y_propag axe ord = incr nb_etage_recursif; let etage_actuel = !nb_etage_recursif in if (!cpt <= 42 && !nb_etage_recursif <= 10) then begin (*print_endline ""; print_int !cpt; print_endline ""; print_int x_propag ; print_string "k"; print_int y_propag;print_string "  "; print_int (axe_to_move axe); print_string "  "; print_int (ord_to_move ord); print_string "  nb_appels_recursifs : "; print_int !nb_etage_recursif;print_string " coord_avant : "; let x_preced,y_preced = !coord_preced in (print_string "("; print_int x_preced; print_string ","; print_int y_preced; print_string ")"); print_endline "";*)
		if (x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>=Array.length table.(x_propag) ) then (cloture_pr := false;[(* (4 + 100* !cpt,4 + 100* !cpt) *)]) else begin
		match table.(x_propag).(y_propag) with
		|Vide -> cloture_pr := false;[ (* ( (9 + 100* !cpt,9 + 100 * !cpt), (axe,ord)) *) ]
		|T(tuile) when ( dir_prairie tuile ) = [||] -> cloture_pr := false;[(* ( (9 - 100* !cpt,9 - 100 * !cpt), (axe,ord)) *) ]
		|T(tuile) ->  let dir_rt = dir_route tuile in (* let dir_pr = dir_prairie tuile in *)
		
		
		
		let a_1 = Hashtbl.mem visited (x_propag,y_propag) in let a_etendre = a_1 in  if ( if (not a_1) then false else  ( (* let extension = extension_prairie tuile axe ord in *)
		 let lst_axe_1_ord_1 = Hashtbl.find visited (x_propag,y_propag) in 
		 	List.exists (fun (axe_1,ord_1) -> if (axe_1,ord_1) = (axe,ord) then true else compare_coord_prairie tuile axe ord axe_1 ord_1) lst_axe_1_ord_1
		 (* begin 
			( ( Array.mem ( axe_to_dir axe_1 ) extension) || ( Array.mem (ord_to_dir ord_1) extension)) && 
		  (not ( (Array.mem ( axe_to_dir axe_1 ) dir_rt) && 
		  ( Array.mem (ord_to_dir ord_1) dir_rt) ) ) && (not  ((Array.mem ( axe_to_dir axe ) dir_rt) && 
		  ( Array.mem (ord_to_dir ord) dir_rt)) ) 
				end *)


		  
		   ) )  then [] else begin
			match table.(x_propag).(y_propag) with
			|Vide -> []
			|T(tuile) ->
				let dir_propag = extension_prairie tuile axe ord in 
				let nb_propag = Array.length dir_propag in
				match nb_propag with
				|0 -> (*print_int (-5); print_string " : " ; print_int (100* !cpt);*)Hashtbl.add visited (x_propag,y_propag) [(axe,ord)];[]
				|1 ->coord_preced := (x_propag,y_propag);if (not a_etendre ) then ((*print_string "!1";*)Hashtbl.add visited (x_propag,y_propag) [(axe,ord)]; if (x_propag,y_propag) = (x,y) then ((*print_string "Got it!1";*)lst_x_y := (axe,ord)::(!lst_x_y)) else ()) 
								else ((*print_string "?1";*)Hashtbl.replace visited (x_propag,y_propag) ((axe,ord)::(Hashtbl.find visited (x_propag,y_propag))); if (x_propag,y_propag) = (x,y) then ((*print_string "Got it?1";*)lst_x_y := (axe,ord)::(!lst_x_y)) else () ); 
								let x_sv,y_sv = add_couple (x_propag,y_propag) (dir_to_move dir_propag.(0) ) in



							let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag(* + 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in

							debut@( reseau_prairie_stock table x_sv y_sv 
							(if x_propag <> x_sv then demi_tour_axe axe else axe)
							(if y_propag <> y_sv then demi_tour_ord ord else ord) )
							
				
				
				|2 ->coord_preced := (x_propag,y_propag); if (not a_etendre) then ((*print_string "!2";*)Hashtbl.add visited (x_propag,y_propag) [(axe,ord)]; if (x_propag,y_propag) = (x,y) then (lst_x_y := ((axe,ord)::(!lst_x_y));(*print_string "Got it!2"; List.iter (fun (axe_lst,ord_lst) -> print_axe axe_lst; print_ord ord_lst) (!lst_x_y)*)) else ()) 
								else ((*print_string "?2";*)Hashtbl.replace visited (x_propag,y_propag) ((axe,ord)::(Hashtbl.find visited (x_propag,y_propag))); if (x_propag,y_propag) = (x,y) then ((*print_string "Got it?2";*)lst_x_y := (axe,ord)::(!lst_x_y)) else () ); 
								if ( rotation 2 dir_propag.(0) = dir_propag.(1) ) then
							( if Array.mem (axe_to_dir axe) dir_propag then 
								let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag(* + 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
								
								debut@(incr cpt; incr cpt;
								let a = reseau_prairie_stock table (x_propag + 1) y_propag Gauche_axe ord in
								nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table (x_propag - 1) y_propag Droite_axe ord in
								(if Array.length dir_rt <> 1 then a@b else (nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag); let c = reseau_prairie_stock table (x_propag + (axe_to_move (dir_to_axe dir_rt.(0)))) y_propag (demi_tour_axe (dir_to_axe dir_rt.(0))) (demi_tour_ord ord) in a@(b@c) ))  )
								
								
								 else
									let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
								
								debut@(incr cpt; incr cpt;
								let a = reseau_prairie_stock table x_propag (y_propag + 1) axe Bas_axe in
								nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table x_propag (y_propag - 1) axe Haut_axe in
								(if Array.length dir_rt <> 1 then a@b else (nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let c = reseau_prairie_stock table x_propag (y_propag + (ord_to_move (dir_to_ord dir_rt.(0))))  (demi_tour_axe axe) (demi_tour_ord (dir_to_ord dir_rt.(0))) in a@(b@c) ))))
							else
							begin
							
							let dir_rt = dir_route tuile in 
								(if Array.mem (axe_to_dir axe) dir_rt then
									( if Array.mem (ord_to_dir ord) dir_rt
										then ((* print_int x_propag; print_string " "; print_int y_propag; print_string " "; print_int (ord_to_move ord); print_int (axe_to_move axe);print_string "  ";print_int 42;*) incr cpt;incr cpt;(* print_string " "; print_int !cpt; print_endline ""; (if a_etendre then print_string "W" else print_string "L");*)
										
								let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag(* + 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
										 
									debut@(let a = reseau_prairie_stock table x_propag (y_propag + ord_to_move ord) axe (demi_tour_ord ord) in
									nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table (x_propag + axe_to_move axe) y_propag (demi_tour_axe axe) ord in a@b) )
											 
											 
											 
									else
									( let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
									
									debut@(incr cpt; incr cpt; let a = reseau_prairie_stock table x_propag 
									(y_propag + ( ( if(Array.mem  (ord_to_dir ord) dir_propag) then 1 else (-1) ) *  ( ord_to_move ord) ) ) 
									(demi_tour_axe axe) 
									( if(Array.mem  (ord_to_dir ord) dir_propag) then demi_tour_ord ord else ord  ) in
									nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table (x_propag + axe_to_move axe) y_propag (demi_tour_axe axe) ord in a@b ) )
										)
										else
										
										
										(if Array.mem (ord_to_dir ord) dir_rt
										then 
										( let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
									
										debut@(incr cpt; incr cpt; let a = reseau_prairie_stock table 
										(x_propag + ( (if(Array.mem  (axe_to_dir axe) dir_propag) then 1 else (-1) ) *  ( axe_to_move axe) ) ) y_propag
									( if(Array.mem  (axe_to_dir axe) dir_propag) then demi_tour_axe axe else axe  )
									(demi_tour_ord ord)  in
									nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table x_propag (y_propag + ord_to_move ord) axe (demi_tour_ord ord) in a@b ))
										else
										
										
										( let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
									
										debut@(incr cpt; incr cpt; let x_1,y_1 = add_couple (x_propag,y_propag) (dir_to_move dir_propag.(0) ) in
											let x_2,y_2 = add_couple (x_propag,y_propag) (dir_to_move dir_propag.(1) ) in
										let a = reseau_prairie_stock table x_1 y_1 
										(if x_1<> x_propag then demi_tour_axe (dir_to_axe dir_propag.(0) ) else  axe )
										(if y_1<> y_propag then demi_tour_ord (dir_to_ord dir_propag.(0) ) else ord) in
										
										
										nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table x_2 y_2 
										(if x_2<> x_propag then demi_tour_axe (dir_to_axe dir_propag.(1) ) else  axe )
										(if y_2<> y_propag then demi_tour_ord (dir_to_ord dir_propag.(1) ) else ord) in a@b  ) )))
							end
					|3 ->coord_preced := (x_propag,y_propag); if (not a_etendre) then ((*print_string "!3";*)Hashtbl.add visited (x_propag,y_propag) [(axe,ord)]; if (x_propag,y_propag) = (x,y) then ((*print_string "Got it!3";*)lst_x_y := (axe,ord)::(!lst_x_y)) else ()) 
				else ((*print_string "?3";*)Hashtbl.replace visited (x_propag,y_propag) ((axe,ord)::(Hashtbl.find visited (x_propag,y_propag))); if (x_propag,y_propag) = (x,y) then ((*print_string "Got it?3";*)lst_x_y := (axe,ord)::(!lst_x_y)) else () ); 
					
					begin let dir_rt = dir_route tuile in match Array.length dir_rt with
							|0
							|1
							|3 ->incr cpt; incr cpt;incr cpt; let arr = Array.make 3 [] in ( let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in

												debut@(for i = 0 to (Array.length dir_rt) - 1 do
													nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let x_sv,y_sv = add_couple (x_propag,y_propag) (dir_to_move dir_propag.(i) ) in
													arr.(i) <-reseau_prairie_stock table x_sv y_sv 
													(if ( x_propag <> x_sv && (axe_to_dir axe = dir_propag.(i) )) then demi_tour_axe axe else axe)
													(if ( y_propag <> y_sv && (ord_to_dir ord = dir_propag.(i) )) then demi_tour_ord ord else ord);done;
													arr.(0)@(arr.(1)@arr.(2) ) ) )
							|_ -> let blocage = not_direction dir_propag in
										let entourage = rotation 1 blocage.(0) in let (passe_libre,adjacent_passe_libre,passe_rt,adjacent_passe_rt) = 
											( if Array.mem entourage dir_rt then (if Array.mem (rotation 2 entourage) dir_rt then (rotation 2 blocage.(0),entourage,rotation 2 entourage,rotation 2 blocage.(0)) else (rotation 2 entourage,rotation 2 blocage.(0),entourage,blocage.(0))) else (entourage,rotation 2 blocage.(0),rotation 2 entourage,blocage.(0))) in
												let x_dir_1,y_dir_1 = dir_to_move passe_libre in
													let x_dir_2,y_dir_2 = dir_to_move adjacent_passe_libre in
														let x_dir_3,y_dir_3 = dir_to_move passe_rt in
														(
														let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
														
															debut@(incr cpt; incr cpt;incr cpt;
																let a = reseau_prairie_stock table (x_propag + x_dir_1) (y_propag + y_dir_1) 
																(if x_dir_1 <> 0 then demi_tour_axe (dir_to_axe passe_libre) else (dir_to_axe adjacent_passe_libre ) )
																(if y_dir_1 <> 0 then demi_tour_ord (dir_to_ord passe_libre) else (dir_to_ord adjacent_passe_libre )) in
														
																nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table (x_propag + x_dir_2) (y_propag + y_dir_2) 
																(if x_dir_2 <> 0 then demi_tour_axe (dir_to_axe adjacent_passe_libre) else (dir_to_axe passe_libre ) )
																(if y_dir_2 <> 0 then demi_tour_ord (dir_to_ord adjacent_passe_libre) else (dir_to_ord passe_libre ));
																in
														
																nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let c = reseau_prairie_stock table (x_propag + x_dir_3) (y_propag + y_dir_3) 
																(if x_dir_3 <> 0 then demi_tour_axe (dir_to_axe passe_rt) else (dir_to_axe adjacent_passe_rt ) )
																(if y_dir_3 <> 0 then demi_tour_ord (dir_to_ord passe_rt) else (dir_to_ord adjacent_passe_rt ))
																in a@(b@c)  )   ) end
				|_ ->coord_preced := (x_propag,y_propag); if (not a_etendre) then ((*print_string "!4";*)Hashtbl.add visited (x_propag,y_propag) [(axe,ord)]; if (x_propag,y_propag) = (x,y) then ((*print_string "Got it!4";*)lst_x_y := (axe,ord)::(!lst_x_y)) else ()) 
			else ((*print_string "?4";*)Hashtbl.replace visited (x_propag,y_propag) ((axe,ord)::(Hashtbl.find visited (x_propag,y_propag))); if (x_propag,y_propag) = (x,y) then ((*print_string "Got it?4";*)lst_x_y := (axe,ord)::(!lst_x_y)) else () ); 
				
									if Array.mem dir_propag.(0) [|Haut;Bas|] then
										( let y_dir = ord_to_move (dir_to_ord dir_propag.(0)) in
											let x_dir = axe_to_move (dir_to_axe dir_propag.(3)) in 
											let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
														( 
															debut@(incr cpt; incr cpt;incr cpt;incr cpt;
															let a = reseau_prairie_stock table x_propag (y_propag + y_dir) (dir_to_axe dir_propag.(1) ) (demi_tour_ord (dir_to_ord dir_propag.(0))) in
															nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table (x_propag - x_dir) y_propag (dir_to_axe dir_propag.(3)) (dir_to_ord dir_propag.(2)) in
															nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let c = reseau_prairie_stock table x_propag (y_propag - y_dir) (dir_to_axe dir_propag.(1) ) (dir_to_ord dir_propag.(0)) in
															nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let d = reseau_prairie_stock table (x_propag + x_dir) y_propag (demi_tour_axe (dir_to_axe dir_propag.(3))) (dir_to_ord dir_propag.(2)) in
															(if Array.length dir_rt <> 1 then ((*print_string "#";*)a@(b@(c@d))) else let x_dir_rt,y_dir_rt = dir_to_move dir_rt.(0) in 
															nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let e = reseau_prairie_stock table (x_propag + x_dir_rt) (y_propag + y_dir_rt) (if x_dir_rt <> 0 then demi_tour_axe (dir_to_axe dir_rt.(0)) else demi_tour_axe (dir_to_axe dir_propag.(1) ) ) (if y_dir_rt <> 0 then demi_tour_ord (dir_to_ord dir_rt.(0)) else demi_tour_ord (dir_to_ord dir_propag.(2)) ) in  a@(b@(c@(d@e))) ) )
															
															) )
									else ( 
										let debut = ((*if (not a_etendre) then print_string "A" else print_string"B";*)[( (x_propag (*+ 100* !cpt*),y_propag(*+100* !cpt*)),(axe,ord) ) ]) in
														( 
														debut@(
									let y_dir = ord_to_move (dir_to_ord dir_propag.(3)) in
									let x_dir = axe_to_move (dir_to_axe dir_propag.(0)) in (incr cpt; incr cpt;incr cpt;incr cpt;
										let a = reseau_prairie_stock table x_propag (y_propag + y_dir) (dir_to_axe dir_propag.(2) ) (demi_tour_ord (dir_to_ord dir_propag.(3))) in
										nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let b = reseau_prairie_stock table (x_propag - x_dir) y_propag (dir_to_axe dir_propag.(0)) (dir_to_ord dir_propag.(1)) in
										nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let c = reseau_prairie_stock table x_propag (y_propag - y_dir) (dir_to_axe dir_propag.(2) ) (dir_to_ord dir_propag.(3)) in
										nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let d = reseau_prairie_stock table (x_propag + x_dir) y_propag (demi_tour_axe (dir_to_axe dir_propag.(0))) (dir_to_ord dir_propag.(1)) in 
										(if Array.length dir_rt <> 1 then ((*print_string "#";*)a@(b@(c@d))) else let x_dir_rt,y_dir_rt = dir_to_move dir_rt.(0) in 
										nb_etage_recursif := etage_actuel;coord_preced := (x_propag,y_propag);let e = reseau_prairie_stock table (x_propag + x_dir_rt) (y_propag + y_dir_rt) (if x_dir_rt <> 0 then demi_tour_axe (dir_to_axe dir_rt.(0)) else demi_tour_axe (dir_to_axe dir_propag.(2) )) (if y_dir_rt <> 0 then demi_tour_ord (dir_to_ord dir_rt.(0)) else demi_tour_ord (dir_to_ord dir_propag.(1)) ) in  a@(b@(c@(d@e))) )))
										
										))
			
			end end end else []; in (let reseau_p = reseau_prairie_stock table x y axe ord in (reseau_p,!lst_x_y,visited,!cloture_pr));;ord_to_move Bas_axe;;


let compare_dir_route tuile dir_0 dir_1 = let dir_rt = dir_route tuile in  Array.mem dir_0 dir_rt && (if dir_0 = dir_1 then true else (Array.length dir_rt <= 2 && Array.mem dir_1 dir_rt));;

let reseau_route table x y dir = let visited = Hashtbl.create 72 in let lst_x_y = ref [] in let cloture_rt = ref true in
			if(x<0||y<0||x>=Array.length table||y>= Array.length table.(x)) then ([],[],false) else
				let rec reseau_route_stock table x_propag y_propag dir = 
					if(if Hashtbl.mem visited (x_propag,y_propag) then List.mem dir (Hashtbl.find visited (x_propag,y_propag)) else false|| let out_of_bounds = x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>= Array.length table.(x_propag) in if out_of_bounds then (cloture_rt := false);out_of_bounds ) then [] else
						match table.(x_propag).(y_propag) with
						|Vide -> cloture_rt := false;[]
						|T(tuile) when (Array.mem dir (dir_route tuile) )  -> let dir_rt = dir_route tuile in let acc =  ref [] in if (Array.length dir_rt)<=2 then 
						((if not (Hashtbl.mem visited (x_propag,y_propag)) then Hashtbl.add visited (x_propag,y_propag) [] else ());
						let is_in_lst = List.exists (fun dir_0 -> Array.mem dir_0 dir_rt) (Hashtbl.find visited (x_propag,y_propag)) in 
						if (not is_in_lst) && ((x_propag,y_propag) = (x,y))  then lst_x_y := dir::(!lst_x_y) else ();
						for i = 0 to (Array.length dir_rt) - 1 do
							if List.mem dir_rt.(i) (Hashtbl.find visited (x_propag,y_propag)) then () else 


							(let x_sv,y_sv = add_couple (x_propag,y_propag) ( dir_to_move dir_rt.(i) ) in Hashtbl.replace visited (x_propag,y_propag) ((dir_rt.(i))::(Hashtbl.find visited (x_propag,y_propag)));
								acc := (reseau_route_stock table x_sv y_sv (rotation 2 dir_rt.(i)) )@(!acc);)
						 done;
						 if dir_rt = [||] then [] else (if is_in_lst then !acc else (((x_propag,y_propag),dir)::(!acc)));)
						 else ( let x_sv,y_sv = add_couple (x_propag,y_propag) ( dir_to_move dir ) in Hashtbl.add visited (x_propag,y_propag) [dir];if (x_propag,y_propag) = (x,y) then lst_x_y := dir::(!lst_x_y) else ();
							acc:= (reseau_route_stock table x_sv y_sv (rotation 2 dir));
						if dir_rt = [||] then [] else (((x_propag,y_propag),dir)::(!acc));)
						|_ -> cloture_rt := false;[]
						in let reseau_premier = reseau_route_stock table x y dir in (reseau_premier,!lst_x_y,!cloture_rt);;


let compare_dir_ville tuile dir_0 dir_1 = let dir_t_1,t_1,_,_,entourage = tuile in t_1 = Ville_1 && Array.mem dir_0 dir_t_1 && (if dir_0 = dir_1 then true else match Array.find_opt (fun arr -> Array.mem dir_0 arr) entourage with
																																																																																											|Some arr -> Array.mem dir_1 arr
																																																																																											|_ -> Array.mem dir_1 dir_t_1 ) ;;
	

let reseau_ville table x y dir = let visited = Hashtbl.create 72 in let lst_x_y = ref [] in let cloture_vl = ref true in let cpt = ref 1 in
if(x<0||y<0||x>=Array.length table||y>= Array.length table.(x)) then ([],[],false,visited) else
	let rec reseau_ville_stock table x_propag y_propag dir = (*print_string "iteration N ::: ";print_int !cpt;print_string "position_search : (";print_int x_propag;print_string ","; print_int y_propag; print_string ") "; print_dir dir;*)  if !cpt <= 5000 then (incr cpt;
		if(if (Hashtbl.mem visited (x_propag,y_propag)) then List.mem dir (Hashtbl.find visited (x_propag,y_propag)) else false||let out_of_bounds = x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>= Array.length table.(x_propag) in if out_of_bounds then (cloture_vl := false);out_of_bounds) then [] else
			(match table.(x_propag).(y_propag) with
			|Vide -> cloture_vl := false;[]
			|T(dir_1,t_1,_,_,entourage) when (t_1 = Ville_1 && Array.mem dir dir_1) -> let acc = ref [] in let catcher = ref [||]
			in let searcher = Array.iter (fun arr -> if !catcher = [||]  then if Array.mem dir arr then catcher := arr) in searcher entourage;
			let propag = (if !catcher = [||] then dir_1 else !catcher) in let already_in_list = ref false in (if (Hashtbl.mem visited (x_propag,y_propag)) then 
				let local_visited = Hashtbl.find visited (x_propag,y_propag) in already_in_list := Array.exists (fun dir_0 -> List.mem dir_0 local_visited ) propag);
			 for i = 0 to (Array.length propag) - 1 do
				let expand = ref false in let x_sv,y_sv = add_couple (x_propag,y_propag) ( dir_to_move propag.(i) ) in if (Hashtbl.mem visited (x_propag,y_propag)) then (if (x_propag,y_propag) = (x,y) then (if not (List.exists (fun dir_0 -> compare_dir_ville (dir_1,t_1,[||],Idem,entourage) dir_0 propag.(i)) (Hashtbl.find visited (x,y))) then lst_x_y := ((propag.(i))::(!lst_x_y)) else () ) else ();
				if not (List.mem propag.(i) (Hashtbl.find visited (x_propag,y_propag))) then 
					((*print_int x_propag;print_dir dir;print_int y_propag;print_string "Replaaaaace";print_endline "";*)expand := true;Hashtbl.replace visited (x_propag,y_propag) ((propag.(i) )::(Hashtbl.find visited (x_propag,y_propag))  ))) else ((*print_int x_propag;print_dir dir;print_int y_propag;print_string "Adddddd";print_endline "";*)expand := true;Hashtbl.add visited (x_propag,y_propag) [propag.(i)];if (x_propag,y_propag) = (x,y) then lst_x_y := (propag.(i))::(!lst_x_y) else ());
					(*print_int x_propag;print_dir dir;print_int y_propag;print_dir propag.(i);print_string "Continueeeee";print_endline "";*)if (!expand) then acc :=((reseau_ville_stock table x_sv y_sv (rotation 2 propag.(i)))@(!acc));
			done;
			if propag = [||] then [] else (if (!already_in_list) then ((*print_int x_propag;print_dir dir;print_int y_propag;print_string "Olllllld";print_endline "";*)!acc) else  ( (*print_int x_propag;print_dir dir;print_int y_propag;print_string "Newwwww";print_endline "";*)((x_propag,y_propag),dir)::(!acc) ) );
			|_ -> cloture_vl := false;[];)) else (failwith "Too much to handle!!!!!";) in let reseau_premier = reseau_ville_stock table x y dir in (reseau_premier,!lst_x_y,!cloture_vl,visited);;

let score_abbaie table x y  = if(x<=0||y<=0||x>=(Array.length table) - 1||y>= (Array.length table.(x)) - 1) then 0 else
	match table.(x).(y) with
		|Vide -> 0
		|T(_,t_1,_,_,_) when t_1 = Abbaie -> let entoure = ref true in 
		for i = -1 to 1 do
			for j = -1 to 1 do
				if(table.(x + i).(y + j) = Vide) then entoure := false
			done;
		done; if !entoure then sc_abbaie else 0;;

		type placement = Paysan of axe_absc*axe_ord | Chevalier of direction | Moine |Voleur of direction;;

		type arr_placement = Libre | Employe of placement;;

		type joueur = J1 | J2;;

		type jeu = plateau_table * (int * int * arr_placement * joueur) array ;;

		type jeu_bordure = jeu * (int * int) array;;
		
		type jeu_compr = plateau_table_compr * (int * int * arr_placement * joueur) array  ;;
		
		type jeu_compr_bordure = jeu_compr * (int * int) array;;

		type ('a, 'b, 'c )  mcts_carcassonne_base = Fin | Node of 'b  * ('a ref)  * ( (('a,'c,'b) mcts_carcassonne_base) list) ref;;

		type 'a tuile_mcts_carcassonne = ('a,int,(int * int * int * arr_placement)) mcts_carcassonne_base;;

		type 'a mvt_mcts_carcassonne = ('a,(int * int * int * arr_placement),int) mcts_carcassonne_base;;
		
		type 'a mcts_carcassonne = Fin_1 | Tuile of 'a tuile_mcts_carcassonne  | Mouvement of 'a mvt_mcts_carcassonne;;

		type 'a mcts_carcassonne_complet =
		| Racine of ('a,int,(int * int * int * arr_placement)) mcts_carcassonne_base 
		| N_interne_Tuile  of ('a mcts_carcassonne_complet  * 'a tuile_mcts_carcassonne)
		| N_interne_Mouvement  of ('a mcts_carcassonne_complet  * 'a mvt_mcts_carcassonne);;
		(*int = numero de tuile *) (* (int * int * int * arr_placement) = coordonnes du tuile (int * int) * rotation du tuile * choix optionnel du meeple *)

		type ('a,'b) probab_tree = Null | Pr of 'a * 'b * (int * 'a,'b) probab_tree array;;

let echanger_joueur = function
		|J1 -> J2
		|J2 -> J1 ;;


let score_abbaie_all table tab_meeple = let acc = ref 0 in  Array.iter (fun (a,b) -> acc:= !acc + (score_abbaie table a b) ) tab_meeple;!acc;;

let score_abbaie_fin_jeu table meeples = let gain_j_1 = ref 0 in let gain_j_2 = ref 0 in Array.iter (fun (x,y,meeple,joueur) -> if meeple = Employe(Moine) then let cpt = ref 0 in (for i = -1 to 1 do
																																																																			for j = -1 to 1 do
																																																																				let x_sv = x + i in let y_sv = y + j in
																																																																					if( if ((x_sv<Array.length table)&& (x_sv>=0)) then (if ((y_sv<Array.length table.(x_sv))&& (y_sv>=0)) then (match table.(x_sv).(y_sv) with
																																																																																																																													|T(_,t_1,_,_,_) when t_1 = Abbaie -> true 
																																																																																																																													|_ -> false) else false ) else false) then incr cpt;done;done; if !cpt <> 9 then (if joueur = J1 then gain_j_1 := (sc_abbaie_par_case * !cpt) + (!gain_j_1) else gain_j_2 := (sc_abbaie_par_case * !cpt) + (!gain_j_2)) ) ) meeples; (!gain_j_1,!gain_j_2);;


let heuristique = 0;;

		let is_possible table x y tuile = if(x<0||y<0||x>=Array.length table||y>= Array.length table.(x)) then false else
		match table.(x).(y) with
		|Vide ->
			let est_valide = ref true in let est_isole = ref true in 
			for i = 1 to 4 do
					let move = dir_to_move (num_to_direction i) in let x_sv,y_sv = add_couple (x,y) move in
						if(x_sv>=0 && x_sv< Array.length table && y_sv >=0 && y_sv < Array.length table.(x_sv)) then
							match table.(x_sv).(y_sv) with
							|T(tuile_voisin) -> if (not (est_coup_valide  tuile  tuile_voisin  (num_to_direction i) )) then (est_valide := false ) else (); est_isole := false;
							|_ -> ()
			done;
						!est_valide && (not !est_isole);
		|_ -> false;;

		let grid_possible table tuile = Array.init (Array.length table) (fun x -> Array.init (Array.length table.(x)) (fun y -> is_possible table x y tuile)  );;

		let lst_possible table tuile = let acc = ref [] in for x = 0 to (Array.length table) - 1 do
			for y = 0 to (Array.length table.(x)) - 1 do
				if (is_possible table x y tuile) then acc := (x,y)::(!acc);
			done;
		done;
		!acc;;
		
		let peut_deposer_meeple jeu_plat x y placement = 
			let table, meeples =  jeu_plat in if(x<0||y<0||x>=Array.length table ||y>= Array.length table.(x)) then false else
			(*si on se permet de ne pas suivre les règles:
			if Array.exists (fun (x_elem,y_elem,a,b) -> (x_elem,y_elem) = (x,y)  ) meeples then false else *)
				begin
					match table.(x).(y) with
					|T(tuile) -> match placement with
						|Moine -> ( match tuile with
							|(_,t_1,_,_,_) -> t_1 = Abbaie )
						|Chevalier(dir) ->( match tuile with
							|(dir_1,t_1,dir_2,t_2,_) -> let reseau_vl,_,_,_ = reseau_ville table x y dir in ( t_1 = Ville_1 && ( Array.mem dir dir_1 ) && (not (Array.exists (fun (x_0,y_0,plac_0,_) -> match plac_0 with
																																																																																											|Employe(Chevalier(dir_0)) -> List.exists (fun ((x_vl,y_vl),dir_vl) ->  (x_0,y_0) = (x_vl,y_vl) && (compare_dir_ville tuile dir_0 dir_vl) ) reseau_vl 
																																																																																											|_ -> false ) meeples)   ) ))
						|Paysan(axe,ord) -> let voisins = extension_prairie tuile axe ord in if voisins = [||] then false else
							let reseau_pr,_,_,_ = reseau_prairie table x y axe ord in not (Array.exists (fun(x_0,y_0,meeple_0,_) -> match meeple_0 with
																																											|Employe(Paysan(axe_0,ord_0)) -> List.exists (fun ( (x_pr,y_pr),(axe_pr,ord_pr) ) -> (x_pr,y_pr) = (x_0,y_0) && compare_coord_prairie tuile axe_pr ord_pr axe_0 ord_0 )  reseau_pr
																																											|_ -> false) meeples )
						|Voleur(dir) -> let dir_rt = dir_route tuile in let reseau_rt,_,_ = reseau_route table x y dir in (Array.mem dir dir_rt) && (not (Array.exists (fun(x_0,y_0,meeple_0,_) -> match meeple_0 with
																																																																																										|Employe(Voleur(dir_0)) -> List.exists (fun ((x_1,y_1),dir_1) -> (x_1,y_1) = (x_0,y_0) && compare_dir_route tuile dir_0 dir_1 )  reseau_rt
																																																																																										|_ -> false) meeples ) )
					|_ -> false 
				end;;
		let deposer_meeple jeu_plat x y placement joueur_j =
			let table, meeples = jeu_plat in let cpt = ref 0 in while ( let _,_,c,_ = meeples.(!cpt) in c<> Libre ) do incr cpt done;
				meeples.(!cpt) <- (x,y,Employe(placement),joueur_j );;
		

				let rec shuffle_arr arr_piochage = if arr_piochage = [||] then [||] else ( let pioche = Random.int (Array.length arr_piochage) in
				let reste = Array.init  ((Array.length arr_piochage) -1) (fun k -> if k< pioche then arr_piochage.(k) else arr_piochage.(k +1)) in
				let melange = shuffle_arr reste in Array.init (Array.length arr_piochage) (fun k -> if k = 0 then arr_piochage.(pioche) else melange.(k - 1 ) ));;

let shuffle_lst lst_piochage = let arr_piochage = Array.of_list lst_piochage in Array.to_list (shuffle_arr arr_piochage);;


let rec shuffle_arr_without_fail lst_piochage table = let rec split lst_piochage table lst_injouable lst_jouable =
			 match lst_piochage with
			|t::q ->let possibilites =  let possibilites_0 = Array.map (fun k -> (k,0)) ( Array.of_list (lst_possible table t) ) in 
								Array.append possibilites_0 (let possibilites_1 = Array.map (fun k -> (k,1)) ( Array.of_list (lst_possible table (rotation_tuile 1 t )) ) in 
								Array.append possibilites_1 (let possibilites_2 = Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 t ))) in 
								Array.append possibilites_2 (let possibilites_3 = Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t )))  in possibilites_3) ) ) in 

									if possibilites = [||] then split q table (t::lst_injouable) lst_jouable else split q table lst_injouable (t::lst_jouable)
									
			|_ -> let arr_jouable = Array.of_list lst_jouable in  let arr_injouable = Array.of_list lst_injouable in 
			if arr_jouable = [||] then [||] else ( let pioche = Random.int (Array.length arr_jouable) in 
				let reste = Array.init ((Array.length arr_jouable) + (Array.length arr_injouable) - 1) 
					(fun k -> if k<pioche then arr_jouable.(k) else 
						(if k < ((Array.length arr_jouable) - 1) then arr_jouable.(k + 1) else arr_injouable.(k - ( (Array.length arr_jouable) - 1)  ) ) )
					in let melange = shuffle_arr reste
				in
				 Array.init ( (Array.length arr_jouable) + (Array.length arr_injouable) ) (fun k -> if k = 0 then arr_jouable.(pioche) else melange.(k - 1) ) )
			in split lst_piochage table [] [];;


let shuffle_lst_without_fail lst_piochage table = Array.to_list (shuffle_arr_without_fail lst_piochage table);;


let cloture_ville table x y dir = let visited = Hashtbl.create 72 in let lst_x_y = ref [] in
	let rec cloture_ville_aux table x_propag y_propag dir = if(x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>= Array.length table.(x_propag)) then false else
		match table.(x_propag).(y_propag) with 
		|T(dir_1,t_1,_,_,entourage) when (t_1 = Ville_1 && (Array.mem dir dir_1)) ->let not_visited = not (Hashtbl.mem visited (x_propag,y_propag)) in if not_visited then Hashtbl.add visited (x_propag,y_propag) [] else ();
			 let catcher = ref [||]
				in let searcher = Array.iter (fun arr -> if !catcher = [||]  then if Array.mem dir arr then catcher := arr) in searcher entourage;
					let propag = (if !catcher <> [||] then !catcher else dir_1) in

					if not ( Array.for_all (fun dir_0 -> let x_sv, y_sv = add_couple (x_propag,y_propag) (dir_to_move dir_0 ) in
					if(x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>= Array.length table.(x_propag)) then false else
						match table.(x_sv).(y_sv) with
							|T(_,t_0,_,_,_) when t_0 = Ville_1 -> true
							|_ -> false) propag ) 
						then false 
					else
							Array.for_all (fun dir_0 -> let unvisited = not_direction ( Array.of_list (Hashtbl.find visited (x_propag,y_propag)) ) in
								 if Array.mem dir_0 unvisited then ( let x_sv, y_sv = add_couple (x_propag,y_propag) (dir_to_move dir_0 ) in
								let finding_1 = Hashtbl.find visited (x_propag,y_propag) in 
									Hashtbl.replace visited (x_propag,y_propag) (dir_0::finding_1) ;if (x_propag,y_propag) = (x,y) then lst_x_y := dir_0::(!lst_x_y) else (); 
									if not  (Hashtbl.mem visited (x_sv,y_sv) ) then ( Hashtbl.add visited (x_sv,y_sv) [(rotation 2 dir_0)];if (x_propag,y_propag) = (x_sv,y_sv) then lst_x_y := (rotation 2 dir_0)::(!lst_x_y) else ()  )
									else ( let finding_2 = Hashtbl.find visited (x_sv,y_sv) in
										Hashtbl.replace visited (x_sv,y_sv) ((rotation 2 dir_0)::finding_2); if (x_propag,y_propag) = (x_sv,y_sv) then lst_x_y := (rotation 2 dir_0)::(!lst_x_y) else (); );
									cloture_ville_aux table x_sv y_sv (rotation 2 dir_0)) else true)  propag;
		|_ -> false
	in (cloture_ville_aux table x y dir , let acc = ref [] in Hashtbl.iter (fun a -> fun b -> acc := (a,b)::(!acc) ) visited;!acc , !lst_x_y)  ;;

let cloture_route table x y dir = if(x<0||y<0||x>=Array.length table||y>= Array.length table.(x)) then (false,[],[]) else
	let visited = Hashtbl.create 72 in  let lst_x_y = ref [] in let rec cloture_route_aux x_propag y_propag dir = 
		if(x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>= Array.length table.(x_propag)) then false else
			match table.(x_propag).(y_propag) with
				|T(tuile) when (Array.mem dir (dir_route tuile) ) -> let dir_rt = dir_route tuile in 
					let nb_rt = Array.length dir_rt in 
						if(nb_rt <= 2) then
							begin
								let entourage_rt = ref true in if (if not (Hashtbl.mem visited (x_propag,y_propag)) then true else (List.length (Hashtbl.find visited (x_propag,y_propag)) ) <= 1  )
								then
								((if not (Array.for_all (fun dir_0 -> let x_sv,y_sv = add_couple (x_propag,y_propag) (dir_to_move dir_0) in
									if(x_sv<0||y_sv<0||x_sv>=Array.length table||y_sv>= Array.length table.(x_sv)) then false else
										match table.(x_sv).(y_sv) with
											|T(tuile) -> Array.mem (rotation 2 dir_0) (dir_route tuile)
											|_ -> false) dir_rt) 
									then entourage_rt := false 
								else () );if not (Hashtbl.mem visited (x_propag,y_propag)) then Hashtbl.add visited (x_propag,y_propag) []) else ();
								if not (!entourage_rt) then false else
									let dir_reste = (let discovered = Hashtbl.find visited (x_propag,y_propag) in  match List.length discovered with
																			|0 -> dir_rt
																			|1 -> let dir_discovered = List.hd discovered in if dir_rt.(0) = dir_discovered then [|dir_rt.(1)|] else [|dir_rt.(0)|]
																			|_ -> [||])
									in Array.for_all (fun dir_0 -> let discovered = Hashtbl.find visited (x_propag,y_propag) in
										if List.mem dir_0 discovered then true else 
											(Hashtbl.replace visited (x_propag,y_propag) (dir_0::discovered);if (x_propag,y_propag) = (x,y) then lst_x_y := dir_0::(!lst_x_y) else (); 
											let x_sv,y_sv = add_couple (x_propag,y_propag) (dir_to_move dir_0) in (if (Hashtbl.mem visited (x_sv,y_sv) ) then
												(Hashtbl.replace visited (x_sv,y_sv) ( (rotation 2 dir_0)::(Hashtbl.find visited (x_sv,y_sv)));if (x_propag,y_propag) = (x_sv,y_sv) then lst_x_y := (rotation 2 dir_0)::(!lst_x_y) else (); ) 
											else Hashtbl.add visited (x_sv,y_sv) [(rotation 2 dir_0)]);if (x_propag,y_propag) = (x_sv,y_sv) then lst_x_y := (rotation 2 dir_0)::(!lst_x_y) else ();  cloture_route_aux x_sv y_sv  (rotation 2 dir_0) ) ) dir_reste
							end
						else
							let already_seen = ref false in let is_seen = Hashtbl.mem visited (x_propag,y_propag) in  if is_seen then 
								if (List.mem dir (Hashtbl.find visited (x_propag,y_propag))) then already_seen := true else ();
							if (!already_seen) then true else
								let x_sv,y_sv = add_couple (x_propag,y_propag) (dir_to_move dir) in
									if(x_propag<0||y_propag<0||x_propag>=Array.length table||y_propag>= Array.length table.(x_propag)) then false else
										begin
											match table.(x_sv).(y_sv) with
												|T(tuile) when Array.mem (rotation 2 dir) (dir_route tuile) -> 
													if (not is_seen) then Hashtbl.add visited (x_propag,y_propag) [] else ();
													if (not (Hashtbl.mem visited (x_sv,y_sv))) then Hashtbl.add visited (x_sv,y_sv) [] else ();
													Hashtbl.replace visited (x_propag,y_propag) (dir::(Hashtbl.find visited (x_propag,y_propag)));if (x_propag,y_propag) = (x,y) then lst_x_y := dir::(!lst_x_y) else ();
													Hashtbl.replace visited (x_sv,y_sv) ( (rotation 2 dir)::(Hashtbl.find visited (x_propag,y_propag)));if (x_propag,y_propag) = (x_sv,y_sv) then lst_x_y := (rotation 2 dir)::(!lst_x_y) else ();
													cloture_route_aux x_sv y_sv (rotation 2 dir) 
										end
	in (cloture_route_aux x y dir ,(let acc = ref [] in Hashtbl.iter (fun a -> fun b -> acc := (a,b)::(!acc) ) visited;!acc), !lst_x_y);;

let cloture_prairie table x y axe ord = let reseau_pr,_,_,_ = reseau_prairie table x y axe ord in
	( List.for_all( fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with
																									 |T(tuile)  -> let extension_pr = extension_prairie tuile axe_0 ord_0 in 
																											Array.for_all(fun dir_0 -> let x_sv,y_sv = add_couple (x,y) (dir_to_move dir_0) in List.exists (fun ((x_1,y_1),_) -> (x_1,y_1) = (x_sv,y_sv)) reseau_pr) extension_pr
																										|_ -> false ) reseau_pr,reseau_pr);;
	



							








	 (* let reseau_rt = reseau_route table x y dir
		in List.for_all (fun (m,n) -> match table.(m).(n) with
			|T(tuile) ->
			let dir_rt = dir_route tuile in  if (Array.length dir_rt <= 2) then 
			Array.for_all (fun dir_0 -> let x_sv, y_sv = add_couple (x,y) (dir_to_move dir_0 ) in
				List.mem  (x_sv,y_sv) reseau_rt) dir_rt else
			Array.exists (fun dir_0 -> let x_sv, y_sv = add_couple (x,y) (dir_to_move dir_0 ) in
			(List.mem (x_sv,y_sv) reseau_rt) ) dir_rt
			|_ -> false
					  ) reseau_rt *)


	let coins_distincts_prairie tuile = let dir_pr = dir_prairie tuile in match Array.length dir_pr with
		|0 -> [||]
		|1 -> if Array.mem dir_pr.(0) (dir_route tuile) then (match (Array.mem dir_pr.(0) [|Haut;Bas|]) with
			|true -> [|(Gauche_axe,dir_to_ord dir_pr.(0));(Droite_axe,dir_to_ord dir_pr.(0))|]
			|_ -> [|(dir_to_axe dir_pr.(0),Bas_axe);(dir_to_axe dir_pr.(0),Haut_axe)  |])
					else (match (Array.mem dir_pr.(0) [|Haut;Bas|]) with
					|true -> [|(Gauche_axe,dir_to_ord dir_pr.(0))|]
					|_ -> [|(dir_to_axe dir_pr.(0),Bas_axe)|] )
		|2 -> if dir_pr.(1) = rotation 2 dir_pr.(0) then (let _,_,_,_,entourage = tuile in match entourage with
			|[| [||] |] -> (let dir_rt = dir_route tuile in match Array.length dir_rt with
												|0 -> [|(Gauche_axe,Bas_axe);(Droite_axe,Haut_axe)|]
												|1 -> if Array.mem dir_rt.(0) [|Haut;Bas|] then [|(Gauche_axe,dir_to_ord dir_rt.(0));(Droite_axe,dir_to_ord dir_rt.(0));(Gauche_axe,dir_to_ord (rotation 2 dir_rt.(0)))|]
															else [|(dir_to_axe dir_rt.(0),Bas_axe);(dir_to_axe dir_rt.(0), Haut_axe);(dir_to_axe (rotation 2 dir_rt.(0)), Bas_axe)|]
												|_ -> [|(Gauche_axe,Bas_axe);(Gauche_axe,Haut_axe);(Droite_axe,Bas_axe);(Droite_axe,Haut_axe)|])
			|_ -> (let dir_rt = dir_route tuile in match Array.length dir_rt with
							|2 -> if Array.mem dir_rt.(0) [|Haut;Bas|] then [|(Gauche_axe,Bas_axe);(Droite_axe,Bas_axe)|] else [|(Gauche_axe,Bas_axe);(Gauche_axe,Haut_axe)|] 
							|_ -> [|(Gauche_axe,Bas_axe)|]))
																							else
																								(let dir_rt = dir_route tuile in match Array.length dir_rt with
																									|0 -> if Array.mem dir_pr.(0) [|Haut;Bas|] then [|(dir_to_axe dir_pr.(1),dir_to_ord dir_pr.(0))|] else [|(dir_to_axe dir_pr.(0),dir_to_ord dir_pr.(1))|]
																									|1 -> [|(Gauche_axe,Bas_axe)|]
																									|_ -> if Array.mem dir_pr.(0) [|Haut;Bas|] then [|(dir_to_axe dir_pr.(1),dir_to_ord dir_pr.(0));(dir_to_axe (rotation 2 dir_pr.(0)),dir_to_ord dir_pr.(1))|] else [|(dir_to_axe dir_pr.(0),dir_to_ord dir_pr.(1));(dir_to_axe dir_pr.(0),dir_to_ord (rotation 2 dir_pr.(1)))|] )
		|3 -> (let dir_rt = dir_route tuile in match Array.length dir_rt with
						|0 -> [|(Gauche_axe,Bas_axe)|]
						|1
						|2 ->if Array.mem dir_rt.(0) [|Haut;Bas|] then [|(Gauche_axe,dir_to_ord dir_rt.(0));(Droite_axe,dir_to_ord dir_rt.(0))|] else [|(dir_to_axe dir_rt.(0),Bas_axe);(dir_to_axe dir_rt.(0),Haut_axe)|] 
						|_ -> let dir_vl,_,_,_,_ = tuile in if Array.mem dir_vl.(0) [|Haut;Bas|] then [|(Gauche_axe,dir_to_ord (rotation 2 dir_vl.(0)));(Droite_axe,dir_to_ord (rotation 2 dir_vl.(0)));(Gauche_axe,dir_to_ord dir_vl.(0))|] else [|(dir_to_axe (rotation 2 dir_vl.(0)),Bas_axe);(dir_to_axe  (rotation 2 dir_vl.(0)),Haut_axe); (dir_to_axe dir_vl.(0),Bas_axe)|] )
		|_ ->(let dir_rt = dir_route tuile in match Array.length dir_rt with
		|0
		|1 -> [|(Gauche_axe,Bas_axe)|]
		|2 ->if Array.mem dir_rt.(0) [|Haut;Bas|] then [|(Gauche_axe,dir_to_ord dir_rt.(0));(Droite_axe,dir_to_ord dir_rt.(0))|] else [|(dir_to_axe dir_rt.(0),Bas_axe);(dir_to_axe dir_rt.(0),Haut_axe)|]
		|3 -> let dir_libre = not_direction dir_rt in if Array.mem dir_libre.(0) [|Haut;Bas|] then [|(Gauche_axe,dir_to_ord (rotation 2 dir_libre.(0)));(Droite_axe,dir_to_ord (rotation 2 dir_libre.(0)));(Gauche_axe,dir_to_ord dir_libre.(0))|] else [|(dir_to_axe (rotation 2 dir_libre.(0)),Bas_axe);(dir_to_axe  (rotation 2 dir_libre.(0)),Haut_axe); (dir_to_axe dir_libre.(0),Bas_axe)|]
		|_ -> [|(Gauche_axe,Bas_axe);(Gauche_axe,Haut_axe);(Droite_axe,Bas_axe);(Droite_axe,Haut_axe)|]);;



		let dir_distincts_route tuile = let dir_rt = dir_route tuile in if dir_rt = [||] then [||] else (if Array.length dir_rt <= 2 then [|dir_rt.(0)|] else dir_rt)

		let dir_distincts_ville tuile = let dir_1,t_1,_,_,entourage = tuile in if (t_1 <> Ville_1) then [||] else (if entourage = [| [||] |] then [|dir_1.(0)|] else (Array.map (fun arr -> if arr = [||] then NAD else arr.(0)) entourage) )


	let options_meeples tuile = let dir_rt = dir_route tuile in if dir_rt = [||] then  (if (let _,t_1,_,_,_ = tuile in t_1 = Abbaie) then [|Moine;Paysan(Gauche_axe,Bas_axe)|]  else 
																																											(let dir_pr = dir_prairie tuile in if dir_pr <> [||] then 
																																												(if (let _,t_1,_,_,_ = tuile in t_1 = Ville_1) then Array.append (Array.map  (fun (axe,ord) -> Paysan((axe,ord) ) )  (coins_distincts_prairie tuile)) (Array.map (fun dir -> Chevalier(dir)) (dir_distincts_ville tuile)) 
																																													else (Array.map  (fun (axe,ord) -> Paysan((axe,ord) ) )  (coins_distincts_prairie tuile)) )
																																												else (Array.map (fun dir -> Chevalier(dir)) (dir_distincts_ville tuile))  ) )
																																								else (let options_voleur = Array.map (fun dir -> Voleur(dir)) (dir_distincts_route tuile) in if (let _,t_1,_,_,_ = tuile in t_1 = Abbaie) then Array.append [|Moine|] options_voleur  else 
																																									(let dir_pr = dir_prairie tuile in if dir_pr <> [||] then 
																																										(if (let _,t_1,_,_,_ = tuile in t_1 = Ville_1) then Array.append (Array.append (Array.map  (fun (axe,ord) -> Paysan((axe,ord) ) )  (coins_distincts_prairie tuile)) (Array.map (fun dir -> Chevalier(dir)) (dir_distincts_ville tuile))) options_voleur
																																											else Array.append (Array.map  (fun (axe,ord) -> Paysan((axe,ord) ) )  (coins_distincts_prairie tuile)) options_voleur )
																																										else Array.append (Array.map (fun dir -> Chevalier(dir)) (dir_distincts_ville tuile)) options_voleur  ) );;
	
	let merge_hashtbl_lst hashtbl_1 hashtbl_2 = Hashtbl.iter (fun a -> fun lst -> if Hashtbl.mem hashtbl_1 a then Hashtbl.replace hashtbl_1 a (lst@(Hashtbl.find hashtbl_1 a)) else Hashtbl.add hashtbl_1 a lst ) hashtbl_2;;

	let prairie_touche_ville tuile axe ord = let dir_pr = dir_prairie tuile in if not (Array.exists (fun dir ->Array.mem dir dir_pr) [|axe_to_dir axe; ord_to_dir ord|]) then [||] else (let dir_1,t_1,_,_,entourage = tuile in if t_1 = Ville_1 then ( let arr_compartiments = (if entourage = [| [||] |] then [|dir_1|] else entourage) in let voisinages = ref [||] in Array.iter (fun compart -> match  Array.length compart with
																																																																																																																																																																																														|0 -> ()
																																																																																																																																																																																														|1 -> if Array.exists (fun i -> let entour_vl_dir = rotation i compart.(0) in let line = Array.mem compart.(0) [|Haut;Bas|] in compare_coord_prairie tuile axe ord (dir_to_axe (if line then entour_vl_dir else compart.(0))) (dir_to_ord (if line then compart.(0) else entour_vl_dir))  ) [|1;-1|] then voisinages := Array.append compart (!voisinages) else ()
																																																																																																																																																																																														|2 ->if Array.exists (fun compart_dir -> Array.exists (fun i -> let entour_vl_dir = rotation i compart_dir in let line = Array.mem compart_dir [|Haut;Bas|] in compare_coord_prairie tuile axe ord (dir_to_axe (if line then entour_vl_dir else compart_dir)) (dir_to_ord (if line then compart_dir else entour_vl_dir))  ) [|1;-1|]) compart then voisinages := Array.append compart (!voisinages) else () 
																																																																																																																																																																																														|_ -> voisinages := Array.append compart (!voisinages) ) arr_compartiments;!voisinages
																																																																																											) else [||] );;
	
	let entourage_prairie_score table reseau_pr = let cpt = ref 0 in let villes_voisins = Hashtbl.create 72 in List.iter(fun ((x_0,y_0),(axe,ord)) -> match table.(x_0).(y_0) with 
																																																								|T(dir_1,t_1,dir_2,t_2,entourage_vl) when t_1 = Ville_1 -> if Hashtbl.mem villes_voisins (x_0,y_0) then let voisinage_villes = prairie_touche_ville (dir_1,t_1,dir_2,t_2,entourage_vl) axe ord in Array.iter (fun dir_compartiment -> if not (List.mem dir_compartiment (Hashtbl.find villes_voisins (x_0,y_0)))     then let _,_,cloture_vl,visite = reseau_ville table x_0 y_0 dir_compartiment in merge_hashtbl_lst villes_voisins visite; if cloture_vl then incr cpt;) voisinage_villes
																																																								|_ -> ()  ) reseau_pr;!cpt;;

	let prairies_non_termines table meeples = let gain_j_1 = ref 0 in let gain_j_2 = ref 0 in Array.iter (fun (x_0,y_0,meeple,joueur) -> match meeple with
																																																					|Employe(Paysan(axe,ord)) -> let nb_meep_j_1 = ref 0 in let nb_meep_j_2 = ref 0 in let reseau_pr,_,_,cloture_pr = reseau_prairie table x_0 y_0 axe ord in 
																																																					if not (cloture_pr) then for j = 0 to (Array.length meeples) -1 do
																																																																																																																					let x_meeple,y_meeple,meeple_0,joueur_0 = meeples.(j) in
																																																																																																																						match meeple_0 with
																																																																																																																							|Employe(Paysan(axe_0,ord_0)) when List.exists (fun ((x_pr,y_pr),(axe_pr,ord_pr)) -> (x_pr,y_pr) = (x_meeple,y_meeple) && (match table.(x_meeple).(y_meeple) with
																																																																																																																																																																																					|T(tuile) -> compare_coord_prairie tuile axe_pr ord_pr axe_0 ord_0
																																																																																																																																																																																					|_ -> false) ) reseau_pr -> if joueur_0 = J1 then incr nb_meep_j_1 else (incr nb_meep_j_2);meeples.(j) <-(x_meeple,y_meeple,Libre,joueur_0)
																																																																																																																							|_ -> ()
																																																																																																																					done;let voisinage_villes = entourage_prairie_score table reseau_pr in ( if((!nb_meep_j_1) >= (!nb_meep_j_2))  then gain_j_1 := (sc_prairie * voisinage_villes) + (!gain_j_1);if((!nb_meep_j_1) <= (!nb_meep_j_2))  then gain_j_2 := (sc_prairie * voisinage_villes) + (!gain_j_2) )
																																																					|_ -> () ) meeples; (!gain_j_1,!gain_j_2);;
	
	
	let score_reseau_ville table reseau_vl meeples = let nb_meeples_j_1 = ref 0 in let nb_meeples_j_2 = ref 0 in let visited = Hashtbl.create 72 in let nb_tuiles_reseau = ref 0 in
	 	List.iter (fun ((x,y),dir) -> if not (Hashtbl.mem visited (x,y)) then (Hashtbl.add visited (x,y) [];incr nb_tuiles_reseau;let meeple_joueur = ref [||] in let continuer = ref true in let i = ref 0 in while((!i < Array.length meeples) && (!continuer)) do
			(let (x_0,y_0,meeple,j_0) = meeples.(!i) in if (x_0,y_0) = (x,y) then (match table.(x_0).(y_0) with
				|T(tuile) -> if (match meeple with
														|Employe(Chevalier(dir_0)) -> compare_dir_ville tuile dir_0 dir
														|_ -> false) then (continuer := false;if j_0 = J1 then incr nb_meeples_j_1 else incr nb_meeples_j_2; meeples.(!i) <-(x_0,y_0,Libre,j_0));
																																								
				|_ -> ());incr i)
			done;)
			) reseau_vl; ( (if (!nb_meeples_j_1 >= !nb_meeples_j_2) then  sc_ville * !nb_tuiles_reseau else 0;),!nb_meeples_j_1,(if (!nb_meeples_j_1 <= !nb_meeples_j_2) then  sc_ville * !nb_tuiles_reseau else 0;),!nb_meeples_j_2);; 


	let score_reseau_route table reseau_rt meeples = let nb_meeples_j_1 = ref 0 in let nb_meeples_j_2 = ref 0 in let visited = Hashtbl.create 72 in let nb_tuiles_reseau = ref 0 in
		List.iter (fun ((x,y),dir) -> if not (Hashtbl.mem visited (x,y)) then (Hashtbl.add visited (x,y) [];incr nb_tuiles_reseau;let meeple_joueur = ref [||] in let continuer = ref true in let i = ref 0 in while((!i < Array.length meeples) && (!continuer)) do
		 (let (x_0,y_0,meeple,j_0) = meeples.(!i) in if (x_0,y_0) = (x,y) then (match table.(x_0).(y_0) with
			 |T(tuile) -> if (match meeple with
													 |Employe(Voleur(dir_0)) -> compare_dir_route tuile dir_0 dir
													 |_ -> false) then (continuer := false;if j_0 = J1 then incr nb_meeples_j_1 else incr nb_meeples_j_2; meeples.(!i) <-(x_0,y_0,Libre,j_0));
																																							 
			 |_ -> ());incr i) done;)) reseau_rt; ( (if (!nb_meeples_j_1 >= !nb_meeples_j_2) then  sc_route * !nb_tuiles_reseau else 0;),!nb_meeples_j_1,(if (!nb_meeples_j_1 <= !nb_meeples_j_2) then  sc_route * !nb_tuiles_reseau else 0;),!nb_meeples_j_2);; 


	let score_reseau_prairie table reseau_pr meeples = let nb_meeples_j_1 = ref 0 in let nb_meeples_j_2 = ref 0 in let visited = Hashtbl.create 72 in
		List.iter (fun ((x,y),(axe,ord)) -> if not (Hashtbl.mem visited (x,y)) then (Hashtbl.add visited (x,y) [];let meeple_joueur = ref [||] in let continuer = ref true in let i = ref 0 in while((!i < Array.length meeples) && (!continuer)) do
		(let (x_0,y_0,meeple,j_0) = meeples.(!i) in if (x_0,y_0) = (x,y) then (match table.(x_0).(y_0) with
			|T(tuile) -> if (match meeple with
													|Employe(Paysan(axe_0,ord_0)) -> compare_coord_prairie tuile axe_0 ord_0 axe ord
													|_ -> false) then (continuer := false;if j_0 = J1 then incr nb_meeples_j_1 else incr nb_meeples_j_2; meeples.(!i) <-(x_0,y_0,Libre,j_0));
																																									
			|_ -> ());incr i)
		done;)
		) reseau_pr; (let nb_voisinages = entourage_prairie_score table reseau_pr in (if (!nb_meeples_j_1 >= !nb_meeples_j_2) then sc_prairie * nb_voisinages else 0;),!nb_meeples_j_1,(if (!nb_meeples_j_1 <= !nb_meeples_j_2) then  sc_prairie * nb_voisinages else 0;),!nb_meeples_j_2);;
	 




	let rec simulation jeu_plat joueur_j lst_piochage coup_predeterminee_ultime nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 nb_meeples_total_j_1 nb_meeples_total_j_2 
	historique_score_j_1 historique_score_j_2 (historique_ajout_nb_meeple_prairie_irrecuperable_j_1,historique_nb_meeples_prairie_j_1) 
	(historique_ajout_nb_meeple_prairie_irrecuperable_j_2,historique_nb_meeples_prairie_j_2)
	historique_nb_meeples_ville_j_1 historique_nb_meeples_ville_j_2 historique_nb_meeples_route_j_1 historique_nb_meeples_route_j_2
	historique_nb_meeples_abbaie_j_1 historique_nb_meeples_abbaie_j_2
	historique_sc_meeples_prairie_j_1 historique_sc_meeples_prairie_j_2
	historique_sc_meeples_ville_j_1 historique_sc_meeples_ville_j_2 historique_sc_meeples_route_j_1 historique_sc_meeples_route_j_2
	historique_sc_meeples_abbaie_j_1 historique_sc_meeples_abbaie_j_2 si_compte_final = Gc.full_major (); let table,meeples = jeu_plat in print_string "Simulation.....";print_int (List.length lst_piochage);print_string "meeples stock for player 1 : "; print_int nb_meeples_j_1; print_string " and meeples stock for player 2 : "; print_int nb_meeples_j_1; print_string "Number of empty meeple spaces"; print_int (let cpt = ref 0 in Array.iter (fun (_,_,etat,_) -> if etat = Libre then incr cpt) meeples;!cpt );  print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";



		
		 match lst_piochage with
			|tuile::q ->((*print_string "searching for positions.......";*)let possibilites_0 = Array.map (fun k -> (k,0)) (Array.of_list (lst_possible table tuile)) in 
			let possibilites = Array.append possibilites_0 (let possibilites_1 = (if tuile <> (rotation_tuile 1 tuile) then Array.map (fun k -> (k,1)) (Array.of_list (lst_possible table (rotation_tuile 1 tuile ) )) else [||] ) in 
			Array.append possibilites_1 (let possibilites_2 = (if ((tuile <> rotation_tuile 2 tuile) && ( (rotation_tuile 1 tuile) <> (rotation_tuile 2 tuile))) then Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 tuile ))) else [||]) in 
			Array.append possibilites_2 (let possibilites_3 = (if ((tuile <> rotation_tuile 3 tuile) && ( (rotation_tuile 1 tuile) <> rotation_tuile 3 tuile) && ((rotation_tuile 2 tuile) <> (rotation_tuile 3 tuile))) then  Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t ))) else [||] )  in possibilites_3) ) )
			in
				if ((coup_predeterminee_ultime = None) && (possibilites = [||])) then



					(print_string "No Move possible,Shuffling........";let nv_lst_piochage = shuffle_lst_without_fail lst_piochage table in
				simulation  jeu_plat joueur_j nv_lst_piochage coup_predeterminee_ultime nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 nb_meeples_total_j_1 nb_meeples_total_j_2 
				historique_score_j_1 historique_score_j_2 (historique_ajout_nb_meeple_prairie_irrecuperable_j_1,historique_nb_meeples_prairie_j_1) (historique_ajout_nb_meeple_prairie_irrecuperable_j_2,historique_nb_meeples_prairie_j_2)
				historique_nb_meeples_ville_j_1 historique_nb_meeples_ville_j_2 historique_nb_meeples_route_j_1 historique_nb_meeples_route_j_2
				historique_nb_meeples_abbaie_j_1 historique_nb_meeples_abbaie_j_2
				historique_sc_meeples_prairie_j_1 historique_sc_meeples_prairie_j_2
				historique_sc_meeples_ville_j_1 historique_sc_meeples_ville_j_2 historique_sc_meeples_route_j_1 historique_sc_meeples_route_j_2
				historique_sc_meeples_abbaie_j_1 historique_sc_meeples_abbaie_j_2 si_compte_final  )





			else
				begin
					(*print_string "Choosing a maybe random tile move.....";*)let coup_random = Random.int (Array.length possibilites) in let (x_1,y_1),rot_1  = (match coup_predeterminee_ultime with
						|Some (x,y,rot,_) -> ((x,y),rot)
						|_ -> possibilites.(coup_random)) in (*print_string "x and y and tiles_rotation chosennnn : "; print_int x_1; print_string " "; print_int y_1; print_string " "; print_int rot_1; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline ""; print_endline "";*)
						table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile);(*print_string "Scanning the area around......";*)
						let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in (*print_string "Checkpoint_1::";*)let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
							Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
														then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
																(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
							(*print_string "Searching for farmer.........";*)let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																				|Vide -> false
																																																																																				|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																											|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																											|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
							in
								let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
								Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
															then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
																	(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
								(*"Searching for thieves.......";*)let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																					|Vide -> false
																																																																																					|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																												|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																												|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
									in (*print_string "Checkpoint 2 :::";*)let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
									(match tuile with
										|(_,t_1,_,_,_) when t_1 = Ville_1 ->		(*print_string "Starting with cities.......";*)Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																														let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in (*print_string "City_Network found";*) 
																															(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;

																												(*print_string "Searching for knights...";*)let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																		|Vide -> false
																																																																																		|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																									|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																									|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																								in let all_options = ((*print_string "Assembling all meeple options.......";*)!options_paysan@(!options_chevalier@(!options_voleur))) in if not ((List.for_all (fun elem -> elem = []) all_options)||(let temp = (if joueur_j = J1 then nb_meeples_j_1 <=0 else nb_meeples_j_2<=0) in if temp then ((*print_string "Unlimited supply HeHeHeHeHeHeHeHe";print_endline ""*));temp)) then 
																									((*print_string "Placing a meeple anyway :|"; print_endline "";*)
																									(match coup_predeterminee_ultime with
																										|Some(_,_,_,meeple) -> meeple_joue := meeple
																										|_ -> let si_meeple = Random.int 2 in if si_meeple = 1 then (let all_options_uniform = ref [] in List.iter (fun lst_options -> List.iter (fun elem -> all_options_uniform := elem::(!all_options_uniform)) lst_options) all_options;
																														meeple_joue := (Employe(let pioche = Random.int (List.length !all_options_uniform) in List.nth !all_options_uniform pioche)) )) );
											|(_,t_1,_,_,_) when t_1 = Abbaie -> let all_options = ((*print_string "Assembling all meeple options.......";*)[Moine]::(!options_paysan@(!options_voleur))) in if not ((List.for_all (fun elem -> elem = []) all_options)||(let temp = (if joueur_j = J1 then nb_meeples_j_1 <=0 else nb_meeples_j_2<=0) in if temp then ((*print_string "Unlimited supply HeHeHeHeHeHeHeHe";print_endline ""*));temp)) then 
																														((*print_string "Placing a meeple anyway :|"; print_endline "";*)
																														(match coup_predeterminee_ultime with
																															|Some(_,_,_,meeple) -> meeple_joue := meeple
																															|_ -> let si_meeple = Random.int 2 in if si_meeple = 1 then (let all_options_uniform = ref [] in List.iter (fun lst_options -> List.iter (fun elem -> all_options_uniform := elem::(!all_options_uniform)) lst_options) all_options;
																															meeple_joue := (Employe(let pioche = Random.int (List.length !all_options_uniform) in List.nth !all_options_uniform pioche))))  );
											|_ -> let all_options = ((*print_string "Assembling all meeple options.......";*)(!options_paysan@(!options_voleur))) in if not ((List.for_all (fun elem -> elem = []) all_options)||(let temp = (if joueur_j = J1 then nb_meeples_j_1 <=0 else nb_meeples_j_2<=0) in if temp then ((*print_string "Unlimited supply HeHeHeHeHeHeHeHe";print_endline ""*));temp)) then 
												((*print_string "Placing a meeple anyway :|"; print_endline "";*)
												match coup_predeterminee_ultime with
													|Some(_,_,_,meeple) -> meeple_joue := meeple
													|_ -> let si_meeple = Random.int 2 in if si_meeple = 1 then (let all_options_uniform = ref [] in List.iter (fun lst_options -> List.iter (fun elem -> all_options_uniform := elem::(!all_options_uniform)) lst_options) all_options;
																	meeple_joue := (Employe(let pioche = Random.int (List.length !all_options_uniform) in List.nth !all_options_uniform pioche)))));
											(*print_string "meeple chosen!!!!!!";*)
										let i = ref 0 in if !meeple_joue <> Libre then 
											( while (let _,_,occupation,_ = meeples.(!i) in occupation <>Libre ) do incr i done; 
										meeples.(!i) <- (x_1,y_1,!meeple_joue,joueur_j) );(*print_string "played a move !!!!";*)


										let gain_j_1 = ref 0 in let gain_j_2 = ref 0 in let gain_meeples_j_1 = ref 0 in let gain_meeples_j_2 = ref 0 in let ajout_nb_meeple_prairie_irrecuperable_j_1 = ref 0 in let ajout_nb_meeple_prairie_irrecuperable_j_2 = ref 0 in
											List.iter ( fun (branches,reseau) -> (*print_string "Field "scoring"....";*)if branches <> [] then let axe_branche,ord_branche = List.hd branches in 
												if (List.exists (fun ((axe_0,ord_0),cloture_0) -> cloture_0 && (compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe_branche ord_branche)) !lst_branches_prairies_visite ) 
													then let sc_j_1,nb_meep_j_1,sc_j_2,nb_meep_j_2 = score_reseau_prairie table reseau meeples in 
														gain_j_1 := sc_j_1 + (!gain_j_1);gain_j_2 := sc_j_2 + (!gain_j_2); ajout_nb_meeple_prairie_irrecuperable_j_1 := nb_meep_j_1 + (!ajout_nb_meeple_prairie_irrecuperable_j_1);ajout_nb_meeple_prairie_irrecuperable_j_2 := nb_meep_j_2 + (!ajout_nb_meeple_prairie_irrecuperable_j_2)  ) !reseaux_branches_pr;


											let gain_sc_pr_j_1 = !gain_j_1 in let gain_sc_pr_j_2 = !gain_j_2 in

											(*print_string "Field score caculated !!!!!!";*)List.iter ( fun (branches,reseau) -> let dir_branche = List.hd branches in 
											if (List.exists (fun (dir_0,cloture_0) -> cloture_0 && (compare_dir_route (rotation_tuile rot_1 tuile) dir_0 dir_branche)) !lst_branches_routes_visite ) 
												then let sc_j_1,nb_meep_j_1,sc_j_2,nb_meep_j_2 = score_reseau_route table reseau meeples in 
													gain_j_1 := sc_j_1 + (!gain_j_1);gain_j_2 := sc_j_2 + (!gain_j_2); gain_meeples_j_1 := nb_meep_j_1 + (!gain_meeples_j_1);gain_meeples_j_2 := nb_meep_j_2 + (!gain_meeples_j_2)  ) !reseaux_branches_rt;

													let nb_recup_meeple_route_j_1 = !gain_meeples_j_1 in let nb_recup_meeple_route_j_2 = !gain_meeples_j_2 in let gain_sc_rt_j_1 = !gain_j_1 - gain_sc_pr_j_1 in let gain_sc_rt_j_2 = !gain_j_2 - gain_sc_pr_j_2 in
													let checkpoint_gain_sc_j_1 = !gain_j_1 in let checkpoint_gain_sc_j_2 = !gain_j_2 in 

											(*print_string "Checkpoint 3::::";*)List.iter ( fun (branches,reseau) -> let dir_branche = List.hd branches in 
											if (List.exists (fun (dir_0,cloture_0) -> cloture_0 && (compare_dir_ville (rotation_tuile rot_1 tuile) dir_0 dir_branche)) !lst_branches_villes_visite ) 
												then let sc_j_1,nb_meep_j_1,sc_j_2,nb_meep_j_2 = score_reseau_ville table reseau meeples in 
													gain_j_1 := sc_j_1 + (!gain_j_1);gain_j_2 := sc_j_2 + (!gain_j_2); gain_meeples_j_1 := nb_meep_j_1 + (!gain_meeples_j_1);gain_meeples_j_2 := nb_meep_j_2 + (!gain_meeples_j_2)  ) !reseaux_branches;

													let nb_recup_meeple_ville_j_1 = !gain_meeples_j_1 - nb_recup_meeple_route_j_1 in let nb_recup_meeple_ville_j_2 = !gain_meeples_j_2 - nb_recup_meeple_route_j_2 in 
													let gain_sc_vl_j_1 = !gain_j_1 - checkpoint_gain_sc_j_1 in let gain_sc_vl_j_2 = !gain_j_2 - checkpoint_gain_sc_j_2 in 
													let checkpoint_meeple_j_1 = !gain_meeples_j_1 in let checkpoint_meeple_j_2 = !gain_meeples_j_2 in let checkpoint_gain_sc_j_1 = !gain_j_1 in let checkpoint_gain_sc_j_2 = !gain_j_2 in 


											(*print_string "City score calculated!!!!";*)if !meeple_joue = Employe(Moine) then (if joueur_j = J1 then gain_j_1 := (score_abbaie table x_1 y_1) + (!gain_j_1) else gain_j_2 := (score_abbaie table x_1 y_1) + (!gain_j_2) ) else ();
											Array.iter (fun (x_a,y_a) -> let i_0 = ref 0 in let continuer = ref true in while ((!continuer)&& ((!i_0) < (Array.length meeples))) do
												(*print_int !i_0;print_string "     ";*) let x_meeple,y_meeple,meeple_1,j = meeples.(!i_0) in if (x_meeple,y_meeple,meeple_1) = (x_a,y_a,Employe(Moine)) then let score_abb = score_abbaie table x_a y_a in (continuer := false;if j = J1 then (gain_j_1 := score_abb + (!gain_j_1); if score_abb <> 0 then (incr gain_meeples_j_1;meeples.(!i_0) <-(x_a,y_a,Libre,J1)) ) else (gain_j_2 := score_abb + (!gain_j_2); if score_abb <> 0 then (incr gain_meeples_j_2;meeples.(!i_0) <-(x_a,y_a,Libre,J2)) ) ) else (); incr i_0 done;) [|(x_1 - 1,y_1);(x_1 + 1,y_1);(x_1,y_1 - 1);(x_1, y_1 + 1)|];

											(*print_string "Dealing with cloisters....";*)let nb_recup_meeple_abbaie_j_1 = !gain_meeples_j_1 - checkpoint_meeple_j_1 in let nb_recup_meeple_abbaie_j_2 = !gain_meeples_j_2 - checkpoint_meeple_j_2 in 
											let gain_sc_abb_j_1 = !gain_j_1 - checkpoint_gain_sc_j_1 in let gain_sc_abb_j_2 = !gain_j_2 - checkpoint_gain_sc_j_2 in 
											
											
											
											(*print_string "End Turn.....";*)simulation (table,meeples) (echanger_joueur joueur_j) (if coup_predeterminee_ultime = None then q else []) coup_predeterminee_ultime (nb_meeples_j_1 + (!gain_meeples_j_1) - (if joueur_j = J1 then (if !meeple_joue <> Libre then 1 else 0) else 0) ) (nb_meeples_j_2 + (!gain_meeples_j_2) - (if joueur_j = J2 then (if !meeple_joue <> Libre then 1 else 0) else 0) ) (score_j_1 + (!gain_j_1)) (score_j_2 + (!gain_j_2)) 
											(if joueur_j = J1 then (if !meeple_joue <> Libre then nb_meeples_total_j_1 + 1 else nb_meeples_total_j_1) else nb_meeples_total_j_1 ) (if joueur_j = J1 then (if !meeple_joue <> Libre then nb_meeples_total_j_1 + 1 else nb_meeples_total_j_1) else nb_meeples_total_j_1 ) 
											(if !gain_j_1 = 0 then historique_score_j_1 else ((score_j_1 + (!gain_j_1))::historique_score_j_1)) (if !gain_j_2 = 0 then historique_score_j_2 else ((score_j_2 + (!gain_j_2))::historique_score_j_2)) 

											( (if !ajout_nb_meeple_prairie_irrecuperable_j_1 = 0 then historique_ajout_nb_meeple_prairie_irrecuperable_j_1 else (if historique_ajout_nb_meeple_prairie_irrecuperable_j_1 = [] then [!ajout_nb_meeple_prairie_irrecuperable_j_1] else (!ajout_nb_meeple_prairie_irrecuperable_j_1 + (List.hd historique_ajout_nb_meeple_prairie_irrecuperable_j_1))::historique_ajout_nb_meeple_prairie_irrecuperable_j_1))
											, (let nb_total_modif =  - (if joueur_j = J1 then (match !meeple_joue with |Employe(Paysan(_)) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_prairie_j_1 else (if historique_nb_meeples_prairie_j_1 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_prairie_j_1)  )::historique_nb_meeples_prairie_j_1)) 
											) 
											( (if !ajout_nb_meeple_prairie_irrecuperable_j_2 = 0 then historique_ajout_nb_meeple_prairie_irrecuperable_j_2 else (if historique_ajout_nb_meeple_prairie_irrecuperable_j_2 = [] then [!ajout_nb_meeple_prairie_irrecuperable_j_2] else (!ajout_nb_meeple_prairie_irrecuperable_j_2 + (List.hd historique_ajout_nb_meeple_prairie_irrecuperable_j_2))::historique_ajout_nb_meeple_prairie_irrecuperable_j_2))
											, (let nb_total_modif =  - (if joueur_j = J2 then (match !meeple_joue with |Employe(Paysan(_)) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_prairie_j_2 else (if historique_nb_meeples_prairie_j_2 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_prairie_j_2)  )::historique_nb_meeples_prairie_j_2)) 
											) 
											
											(let nb_total_modif = nb_recup_meeple_ville_j_1 - (if joueur_j = J1 then (match !meeple_joue with |Employe(Chevalier(_)) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_ville_j_1 else (if historique_nb_meeples_ville_j_1 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_ville_j_1)  )::historique_nb_meeples_ville_j_1)) 
											(let nb_total_modif = nb_recup_meeple_ville_j_2 - (if joueur_j = J2 then (match !meeple_joue with |Employe(Chevalier(_)) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_ville_j_2 else (if historique_nb_meeples_ville_j_2 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_ville_j_2)  )::historique_nb_meeples_ville_j_2))

											(let nb_total_modif = nb_recup_meeple_route_j_1 - (if joueur_j = J1 then (match !meeple_joue with |Employe(Voleur(_)) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_route_j_1 else (if historique_nb_meeples_route_j_1 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_route_j_1)  )::historique_nb_meeples_route_j_1)) 
											(let nb_total_modif = nb_recup_meeple_route_j_2 - (if joueur_j = J2 then (match !meeple_joue with |Employe(Voleur(_)) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_route_j_2 else (if historique_nb_meeples_route_j_2 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_route_j_2)  )::historique_nb_meeples_route_j_2))

											(let nb_total_modif = nb_recup_meeple_abbaie_j_1 - (if joueur_j = J1 then (match !meeple_joue with |Employe(Moine) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_abbaie_j_1 else (if historique_nb_meeples_abbaie_j_1 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_abbaie_j_1)  )::historique_nb_meeples_abbaie_j_1)) 
											(let nb_total_modif = nb_recup_meeple_abbaie_j_2 - (if joueur_j = J2 then (match !meeple_joue with |Employe(Moine) -> 1 |_ -> 0) else 0) in if nb_total_modif = 0 then historique_nb_meeples_abbaie_j_2 else (if historique_nb_meeples_abbaie_j_2 = [] then [nb_total_modif] else (nb_total_modif + (List.hd historique_nb_meeples_abbaie_j_2)  )::historique_nb_meeples_abbaie_j_2))


											
											
											
											(if gain_sc_pr_j_1 = 0 then historique_sc_meeples_prairie_j_1 else (if historique_sc_meeples_prairie_j_1 = [] then [gain_sc_pr_j_1] else (gain_sc_pr_j_1 + (List.hd historique_sc_meeples_prairie_j_1)  )::historique_sc_meeples_prairie_j_1)) 
											(if gain_sc_pr_j_2 = 0 then historique_sc_meeples_prairie_j_2 else (if historique_sc_meeples_prairie_j_2 = [] then [gain_sc_pr_j_2] else (gain_sc_pr_j_2 + (List.hd historique_sc_meeples_prairie_j_2)  )::historique_sc_meeples_prairie_j_2)) 
											
											(if gain_sc_vl_j_1 = 0 then historique_sc_meeples_ville_j_1 else (if historique_sc_meeples_ville_j_1 = [] then [gain_sc_vl_j_1] else (gain_sc_vl_j_1 + (List.hd historique_sc_meeples_ville_j_1)  )::historique_sc_meeples_ville_j_1)) 
											(if gain_sc_vl_j_2 = 0 then historique_sc_meeples_ville_j_2 else (if historique_sc_meeples_ville_j_2 = [] then [gain_sc_vl_j_2] else (gain_sc_vl_j_2 + (List.hd historique_sc_meeples_ville_j_2)  )::historique_sc_meeples_ville_j_2)) 

											(if gain_sc_rt_j_1 = 0 then historique_sc_meeples_route_j_1 else (if historique_sc_meeples_route_j_1 = [] then [gain_sc_rt_j_1] else (gain_sc_rt_j_1 + (List.hd historique_sc_meeples_route_j_1)  )::historique_sc_meeples_route_j_1)) 
											(if gain_sc_rt_j_2 = 0 then historique_sc_meeples_route_j_2 else (if historique_sc_meeples_route_j_2 = [] then [gain_sc_rt_j_2] else (gain_sc_rt_j_2 + (List.hd historique_sc_meeples_route_j_2)  )::historique_sc_meeples_route_j_2)) 

											(if gain_sc_abb_j_1 = 0 then historique_sc_meeples_abbaie_j_1 else (if historique_sc_meeples_abbaie_j_1 = [] then [gain_sc_abb_j_1] else (gain_sc_abb_j_1 + (List.hd historique_sc_meeples_abbaie_j_1)  )::historique_sc_meeples_abbaie_j_1)) 
											(if gain_sc_abb_j_2 = 0 then historique_sc_meeples_abbaie_j_2 else (if historique_sc_meeples_abbaie_j_2 = [] then [gain_sc_abb_j_2] else (gain_sc_abb_j_2 + (List.hd historique_sc_meeples_abbaie_j_2)  )::historique_sc_meeples_abbaie_j_2))
											si_compte_final 	

				end)
					|_ -> (*print_string "Resulllllllllllt (drums playing) ::: ";print_endline "";print_endline  "";print_endline "";print_endline  "";print_endline "";print_endline  "";print_endline "";print_endline  "";*) 
					let gain_pr_j_1,gain_pr_j_2 = (if si_compte_final then (prairies_non_termines table meeples) else (0,0)) in let gain_abbaie_j_1,gain_abbaie_j_2 = (if si_compte_final then  (score_abbaie_fin_jeu table meeples) else (0,0)) in 
					let gain_rt_j_1= ref 0 in let gain_rt_j_2 = ref 0 in let gain_vl_j_1 = ref 0 in let gain_vl_j_2 = ref 0 in 
					let current_nb_meeples_vl_j_1 = ref 0 in let current_nb_meeples_vl_j_2 = ref 0 in let current_nb_meeples_rt_j_1 = ref 0 in let current_nb_meeples_rt_j_2 = ref 0 in
					if si_compte_final then (Array.iter(fun (x,y,meeple,joueur) -> match meeple with 
																									|Employe(Chevalier(dir)) -> let reseau_vl,_,_,_ = reseau_ville table x y dir in List.iter (fun ((x_rt,y_rt),dir_rt) -> Array.iteri 
																																							(fun index -> fun (x_meeple,y_meeple,meeple_0,joueur_meeple) ->  if ((x_rt,y_rt) = (x_meeple,y_meeple)) && (match meeple_0 with |Employe(Chevalier(dir_chevalier)) -> compare_dir_ville (match table.(x_rt).(y_rt) with |T(tile_0) -> tile_0) dir_chevalier dir |_ -> false ) then (meeples.(index) <- (x_meeple,y_meeple,Libre,joueur_meeple); if joueur_meeple = J1 then incr current_nb_meeples_vl_j_1 else incr current_nb_meeples_vl_j_2 )  ) meeples ) reseau_vl;
																																							if (!current_nb_meeples_vl_j_1 >= !current_nb_meeples_vl_j_2) then gain_vl_j_1 := !current_nb_meeples_vl_j_1 + (!gain_vl_j_1);if (!current_nb_meeples_vl_j_2 >= !current_nb_meeples_vl_j_1) then gain_vl_j_2 := !current_nb_meeples_vl_j_2 + (!gain_vl_j_2);
																																							current_nb_meeples_vl_j_1 := 0;current_nb_meeples_vl_j_2 := 0;
																									|Employe(Voleur(dir)) ->let reseau_rt,_,_ = reseau_route table x y dir in List.iter (fun ((x_rt,y_rt),dir_rt) -> Array.iteri 
																																					(fun index -> fun (x_meeple,y_meeple,meeple_0,joueur_meeple) ->  if ((x_rt,y_rt) = (x_meeple,y_meeple)) && (match meeple_0 with |Employe(Voleur(dir_voleur)) -> compare_dir_ville (match table.(x_rt).(y_rt) with |T(tile_0) -> tile_0) dir_voleur dir |_ -> false ) then (meeples.(index) <- (x_meeple,y_meeple,Libre,joueur_meeple); if joueur_meeple = J1 then incr current_nb_meeples_rt_j_1 else incr current_nb_meeples_rt_j_2 )  ) meeples ) reseau_rt;
																																					if (!current_nb_meeples_rt_j_1 >= !current_nb_meeples_rt_j_2) then gain_rt_j_1 := !current_nb_meeples_rt_j_1 + (!gain_rt_j_1);if (!current_nb_meeples_rt_j_2 >= !current_nb_meeples_rt_j_1) then gain_rt_j_2 := !current_nb_meeples_rt_j_2 + (!gain_rt_j_2);
																																					current_nb_meeples_rt_j_1 := 0;current_nb_meeples_rt_j_2 := 0;
																									|_ -> ()
					
					) meeples);
					(nb_meeples_j_1,nb_meeples_j_2,score_j_1 + gain_pr_j_1 + gain_abbaie_j_1 + !gain_rt_j_1 + !gain_vl_j_1,score_j_2 + gain_pr_j_2 + gain_abbaie_j_2 + !gain_rt_j_2 + !gain_vl_j_2,(nb_meeples_total_j_1,nb_meeples_total_j_2,
					(if gain_pr_j_1 + gain_abbaie_j_1 + !gain_rt_j_1 + !gain_vl_j_1 = 0 then historique_score_j_1 else (if historique_score_j_1 = [] then  [gain_pr_j_1 + gain_abbaie_j_1 + !gain_rt_j_1 + !gain_vl_j_1] else (gain_pr_j_1 + gain_abbaie_j_1 + !gain_rt_j_1 + !gain_vl_j_1 + (List.hd historique_score_j_1))::historique_score_j_1 )),
					(if gain_pr_j_2 + gain_abbaie_j_2 + !gain_rt_j_2 + !gain_vl_j_2 = 0 then historique_score_j_2 else (if historique_score_j_2 = [] then  [gain_pr_j_2 + gain_abbaie_j_2 + !gain_rt_j_2 + !gain_vl_j_2] else (gain_pr_j_2 + gain_abbaie_j_2 + !gain_rt_j_2 + !gain_vl_j_2 + (List.hd historique_score_j_2))::historique_score_j_2 )),
					(historique_ajout_nb_meeple_prairie_irrecuperable_j_1,historique_nb_meeples_prairie_j_1),(historique_ajout_nb_meeple_prairie_irrecuperable_j_2,historique_nb_meeples_prairie_j_2),
					historique_nb_meeples_ville_j_1,historique_nb_meeples_ville_j_2,
					historique_nb_meeples_route_j_1,historique_nb_meeples_route_j_2,
					historique_nb_meeples_abbaie_j_1,historique_nb_meeples_abbaie_j_2,
					(if gain_pr_j_1 = 0 then historique_sc_meeples_prairie_j_1 else (if historique_sc_meeples_prairie_j_1 = [] then  [gain_pr_j_1] else (gain_pr_j_1 + (List.hd historique_sc_meeples_prairie_j_1))::historique_sc_meeples_prairie_j_1 )),
					(if gain_pr_j_2 = 0 then historique_sc_meeples_prairie_j_2 else (if historique_sc_meeples_prairie_j_2 = [] then  [gain_pr_j_2] else (gain_pr_j_2 + (List.hd historique_sc_meeples_prairie_j_2))::historique_sc_meeples_prairie_j_2 )),

					(if !gain_vl_j_1 = 0 then historique_sc_meeples_ville_j_1 else (if historique_sc_meeples_ville_j_1 = [] then  [!gain_vl_j_1] else (!gain_vl_j_1 + (List.hd historique_sc_meeples_ville_j_1))::historique_sc_meeples_ville_j_1 )),
					(if !gain_vl_j_2 = 0 then historique_sc_meeples_ville_j_2 else (if historique_sc_meeples_ville_j_2 = [] then  [!gain_vl_j_2] else (!gain_vl_j_2 + (List.hd historique_sc_meeples_ville_j_2))::historique_sc_meeples_ville_j_2 )),
					(if !gain_rt_j_1 = 0 then historique_sc_meeples_route_j_1 else (if historique_sc_meeples_route_j_1 = [] then  [!gain_rt_j_1] else (!gain_rt_j_1 + (List.hd historique_sc_meeples_route_j_1))::historique_sc_meeples_route_j_1 )),
					(if !gain_rt_j_2 = 0 then historique_sc_meeples_route_j_2 else (if historique_sc_meeples_route_j_2 = [] then  [!gain_rt_j_2] else (!gain_rt_j_2 + (List.hd historique_sc_meeples_route_j_2))::historique_sc_meeples_route_j_2 )),
					




					(if gain_abbaie_j_1 = 0 then historique_sc_meeples_abbaie_j_1 else (if historique_sc_meeples_abbaie_j_1 = [] then  [gain_abbaie_j_1] else (gain_abbaie_j_1 + (List.hd historique_sc_meeples_abbaie_j_1))::historique_sc_meeples_abbaie_j_1 )),
					if gain_abbaie_j_2 = 0 then historique_sc_meeples_abbaie_j_2 else (if historique_sc_meeples_abbaie_j_2 = [] then  [gain_abbaie_j_2] else (gain_abbaie_j_1 + (List.hd historique_sc_meeples_abbaie_j_2))::historique_sc_meeples_abbaie_j_2 ))
					);;


					let simulation_simple  jeu_plat joueur_j lst_piochage nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 si_compte_final = 
						simulation  jeu_plat joueur_j lst_piochage None nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 0 0 
						[] [] ([],[]) ([],[]) [] [] [] [] [] [] [] [] [] [] [] [] [] [] si_compte_final;;

					let simulation_ultra_simple jeu_plat joueur_j lst_piochage nb_meeples_j_1 nb_meeples_j_2 si_compte_final  = 
					simulation_simple jeu_plat joueur_j lst_piochage nb_meeples_j_1 nb_meeples_j_2 0 0 si_compte_final;;

					let jouer_coup_simple jeu_plat joueur_j coup nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 =
						simulation  jeu_plat joueur_j [a] coup nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 0 0 
						[] [] ([],[]) ([],[]) [] [] [] [] [] [] [] [] [] [] [] [] [] [] false;;














					let rec selection_expansion node init_donnees_fun eval_fun move_fun possible_moves_fun get_all_tiles = match node with
					|Fin_1 -> print_int (-111);print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";(Fin_1,Racine(Fin))
					|Tuile(Node(num_tuile,donnees_parent,mouvements)) -> print_int (-222);print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";
					if List.for_all (fun a -> a = Fin) !mouvements then (let all_moves = possible_moves_fun num_tuile in 
						(if all_moves = [||] then (Fin_1,Racine(Fin)) else let rand_move = Random.int (Array.length all_moves) in 
							mouvements := [Node( (all_moves.(rand_move)) ,  ref (init_donnees_fun ()), ref [])];move_fun num_tuile all_moves.(rand_move); 
							print_string "Exampleee";(Mouvement(List.hd !mouvements),Racine(Node(num_tuile,donnees_parent,mouvements)))  ) ) 
						else 
							let all_moves = possible_moves_fun num_tuile in (if (Array.length all_moves) <> (List.length !mouvements) then (let untried_moves = ref [] in Array.iter ( fun elem -> if not (List.exists (fun elem_mcts -> match elem_mcts with |Node(move_0,_,_) -> elem = move_0 |_ -> false) !mouvements ) then untried_moves := elem::(!untried_moves) ) all_moves;
							let pick_new_move = Random.int (List.length !untried_moves) in let rand_new_move = (List.nth !untried_moves pick_new_move) in  mouvements := (Node( rand_new_move ,  ref (init_donnees_fun ()), ref []))::(!mouvements); 
							move_fun num_tuile rand_new_move;(Mouvement(List.hd !mouvements),Racine(Node(num_tuile,donnees_parent,mouvements))) ) else
							begin
								let max_node = ref infinity in let next_move = ref Fin in
									List.iter (fun move -> match move with 
																|Fin -> ()
																|Node(a,donnees_exper,c) -> if !max_node = infinity then 
																	(max_node := eval_fun !donnees_exper !donnees_parent; next_move := Node(a,donnees_exper,c)) else 
																		(let value = eval_fun !donnees_exper !donnees_parent in if value > (!max_node) then (max_node := value;next_move := Node(a,donnees_exper,c)) )  ) !mouvements; 
											let res_a,res_b = selection_expansion (Mouvement(!next_move)) init_donnees_fun eval_fun  move_fun possible_moves_fun get_all_tiles in (res_a,N_interne_Tuile(res_b,Node(num_tuile,donnees_parent,mouvements)));
							end)
					|Mouvement(Node( (x,y,rot_tuile,meeple),donnees_exper,tuiles_sv)) ->print_int (-333);print_endline "";print_endline "";print_endline "";print_endline "";print_endline "";print_endline ""; let deck = get_all_tiles () in if deck = [||] then (Fin_1,N_interne_Mouvement(Racine(Fin),Node((x,y,rot_tuile,meeple),donnees_exper,tuiles_sv))) else ( let pioche = Random.int (Array.length deck) in let tuile_rand = deck.(pioche) in
						match List.find_opt (fun tuile -> match tuile with |Node(numero_tuile,_,_) -> numero_tuile = tuile_rand |_ -> false  ) !tuiles_sv with
							|Some next_tuile -> let res_a,res_b = selection_expansion (Tuile(next_tuile)) init_donnees_fun eval_fun move_fun possible_moves_fun get_all_tiles in (res_a,N_interne_Mouvement(res_b,Node((x,y,rot_tuile,meeple),donnees_exper,tuiles_sv)))
							|_ -> tuiles_sv := Node(tuile_rand,ref (init_donnees_fun ()),ref [])::(!tuiles_sv); (Tuile(List.hd !tuiles_sv) ,N_interne_Mouvement(Racine(Fin),Node((x,y,rot_tuile,meeple),donnees_exper,tuiles_sv)))   );;

			let init_donnees_carcassone = fun () -> (0,0);;


		let eval_node constant (nb_visites_i,victoire_i) (nb_visites_parent,victoire_parent_i) = (float_of_int (nb_visites_i) /. float_of_int(victoire_i) ) +. constant *. sqrt(log (float_of_int nb_visites_parent) /. (float_of_int nb_visites_i));; 
		
		let init_table tuile = let arr =  (Array.make_matrix 145 145 Vide , Array.make 25 (-1,-1,Libre,J1) ) in (fst arr).(72).(72) <- T(tuile);arr ;;

		let rec mise_a_jour modification_fun parcours_arbre = match parcours_arbre with
			|Racine(Node(_,donnees,_)) -> donnees := modification_fun !donnees;
			|N_interne_Mouvement(reste,Node(_,donnees,_)) 
			|N_interne_Tuile(reste,Node(_,donnees,_)) -> donnees := modification_fun !donnees; mise_a_jour modification_fun reste
			|N_interne_Mouvement(reste,_) 
			|N_interne_Tuile(reste,_) -> mise_a_jour modification_fun reste
			|_ -> ();;





			let base_jeu_mcts table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j constant_eval score_j_1 score_j_2 itermax = if lst_piochage = [] then None else begin 
			let table,meeples = table_jeu in
			let copie_jeu_plat = (print_string "copying....";Array.map (fun elem -> ((*print_string "copying....";*)Array.copy elem) ) table, Array.copy meeples) in
			let reinitialiser_jeu_plat = (fun () -> let copie_1,copie_2 = copie_jeu_plat in print_string "returning to initial state....";(Array.iteri (fun index -> fun elem -> ((*print_string "returning to initial state....";*)table.(index) <- Array.copy elem) ) copie_1; Array.iteri (fun index -> fun elem -> meeples.(index) <- elem) copie_2)) in
			let current_deck = ref lst_piochage in let current_score_j_1 = ref score_j_1 in 
			let current_score_j_2 = ref score_j_2 in let current_player = ref joueur_j in 
			let current_nb_meeples_j_1 = ref nb_meeples_j_1 in let current_nb_meeples_j_2 = (print_int (-1);ref nb_meeples_j_2) in
			print_string "fun_1_";let move_carcassonne num_tuile (x,y,rot_tuile,meeple)  = (print_string "x and y and rot_tuile ; ";print_int x; print_string " "; print_int y; print_string " "; print_int rot_tuile; let nb_meeples_1,nb_meeples_2,score_1,score_2,_ = jouer_coup_simple table_jeu (!current_player) (Some(x,y,rot_tuile,meeple)) nb_meeples_j_1 nb_meeples_j_2 (!current_score_j_1) (!current_score_j_2)
			in print_string "Tryyyiiinnngg";current_score_j_1 := score_1;current_score_j_2 := score_2;current_nb_meeples_j_1 :=  nb_meeples_1; current_nb_meeples_j_2 :=  nb_meeples_2;current_player := (echanger_joueur (!current_player)); 
			(let rec remove_from_deck tuile = function
			|t::q -> if t = tuile then q else t::(remove_from_deck tuile q) |_ -> []
			in current_deck := remove_from_deck (tiles_of_int num_tuile) lst_piochage );print_string "Finished move") in 
			print_string "fun_2_";let possible_moves_carcassonne num_tuile = let tuile = (tiles_of_int num_tuile) in
				(let rec pos_possible i =if (if i = 0 then true else  
					(if i = 1 then ((rotation_tuile 1 tuile) <> tuile) else 
						(if i = 2 then (((rotation_tuile 2 tuile) <> tuile)&&((rotation_tuile 2 tuile) <> (rotation_tuile 1 tuile) )) else 
							(if i = 3 then (((rotation_tuile 3 tuile) <> tuile)&&((rotation_tuile 3 tuile) <> (rotation_tuile 1 tuile))&&((rotation_tuile 3 tuile) <> (rotation_tuile 2 tuile))) else false   ) )) ) then ( (pos_possible (i + 1))@(List.map (fun elem -> (elem,i)) (lst_possible table (rotation_tuile i (tiles_of_int num_tuile)) ))  ) else (if i > 3 then [] else pos_possible (i + 1))
				in let lst_tuiles_possibles = pos_possible 0 in let tuile = (tiles_of_int num_tuile) in if ( ((joueur_j = J1)&&(nb_meeples_j_1 <= 0))|| ((joueur_j = J2)&&(nb_meeples_j_2 <= 0)) ) then Array.of_list (List.map (fun ((x_1,y_1),rot_1) -> (x_1,y_1,rot_1,Libre) ) lst_tuiles_possibles) else
				 let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
					
					



				let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
							Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
														then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
																(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
							let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																				|Vide -> false
																																																																																				|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																											|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																											|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
							in
								let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
								Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
															then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
																	(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
								let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																					|Vide -> false
																																																																																					|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																												|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																												|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
									in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
									(match tuile with
										|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																														let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																															(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;

																												let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																		|Vide -> false
																																																																																		|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																									|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																									|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																								in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
											|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
											|_ -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) )) 
											) lst_tuiles_possibles) 
										
				in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in Array.of_list every_move)

			in print_string "fun_3_";let get_all_tiles_carcassonne = (fun () -> let acc = ref [] in let visited = Hashtbl.create 72 in  
			(List.iter (fun tuile_0 ->if not (Hashtbl.mem visited tuile_0) then (Hashtbl.add visited tuile_0 []; if ( Array.exists (fun i -> lst_possible table (rotation_tuile i tuile_0) != [] )  [|0;1;2;3|]) then acc:= (int_of_tiles tuile_0)::(!acc) ) ) !current_deck;Array.of_list !acc))
			in let arbre_base = (let next_tile = List.hd lst_piochage in Node ( (int_of_tiles (next_tile), ref ( init_donnees_carcassone () )  , ref [] )  )) in



				print_string "begin_process____";let rec recherche_mcts itermax =
					for i = 1 to itermax do
					print_endline ""; print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					(*print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
					print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";*)
					
					let resultat = ref (0,0) in if itermax > 0 then 
				 ((let subject,branch_tree = selection_expansion (Tuile(arbre_base)) init_donnees_carcassone (eval_node constant_eval) move_carcassonne possible_moves_carcassonne get_all_tiles_carcassonne in (match subject with
			|Fin_1 -> let _,_,score_final_1,score_final_2,_ = simulation_simple  table_jeu (!current_player) (!current_deck) (!current_nb_meeples_j_1) (!current_nb_meeples_j_2) (!current_score_j_1) (!current_score_j_2) true
								in  resultat := (score_final_1,score_final_2);
			|Tuile(Node(num_tuile,_,_)) -> let rec mettre_en_tete elem = (function |t1::t2::q -> if t2 = elem then t2::t1::q else (if t1 = elem then t1::t2::q else let temp = (mettre_en_tete elem (t2::q)) in if (List.hd temp) = elem then elem::t1::(List.tl temp) else t1::t2::q  ) |m -> m )
							in current_deck :=  mettre_en_tete (tiles_of_int num_tuile) (!current_deck); let _,_,score_final_1,score_final_2,_ = simulation_simple  table_jeu (!current_player) (!current_deck) (!current_nb_meeples_j_1) (!current_nb_meeples_j_2) (!current_score_j_1) (!current_score_j_2) true
							in resultat := (score_final_1,score_final_2);
			|Mouvement(Node(_,_,_)) -> let _,_,score_final_1,score_final_2,_ = simulation_simple  table_jeu (!current_player) (!current_deck) (!current_nb_meeples_j_1) (!current_nb_meeples_j_2) (!current_score_j_1) (!current_score_j_2) true
														in  resultat := (score_final_1,score_final_2) );
			mise_a_jour (fun (a_1,b_1) -> (a_1 + 1, b_1 + (let c_1,d_1 = !resultat in if joueur_j = J1 then c_1 - d_1 else d_1 - c_1 )  ) ) branch_tree);reinitialiser_jeu_plat ();Gc.full_major ()) done;
			
		in recherche_mcts itermax; let best_move = (let max_node = ref neg_infinity in let next_move = ref (-1,-1,-1,Libre) in let donnees_parent,mouvements = (match arbre_base with |Node(_,donnees,mouvements_0) -> (donnees,mouvements_0) ) in
						((List.iter (fun move -> match move with 
												|Fin -> ()
												|Node(a,donnees_exper,c) -> if !max_node = neg_infinity then 
													(max_node := eval_node 2. !donnees_exper !donnees_parent; next_move := a) else 
														(let value = eval_node 2. !donnees_exper !donnees_parent in if value > (!max_node) then (max_node := value;next_move := a) )  ) !mouvements); if !next_move = (-1,-1,-1,Libre) then None else Some(!next_move) )) in best_move end;;

														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														
														let base_jeu_mctso table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j constant_eval score_j_1 score_j_2 itermax = if lst_piochage = [] then None else begin 
															let table,meeples = table_jeu in
															let copie_jeu_plat = (print_string "copying....";Array.map (fun elem -> ((*print_string "copying....";*)Array.copy elem) ) table, Array.copy meeples) in
															let reinitialiser_jeu_plat = (fun () -> let copie_1,copie_2 = copie_jeu_plat in print_string "returning to initial state....";(Array.iteri (fun index -> fun elem -> ((*print_string "returning to initial state....";*)table.(index) <- Array.copy elem) ) copie_1; Array.iteri (fun index -> fun elem -> meeples.(index) <- elem) copie_2)) in
															let current_deck = ref lst_piochage in let current_score_j_1 = ref score_j_1 in 
															let current_score_j_2 = ref score_j_2 in let current_player = ref joueur_j in 
															let current_nb_meeples_j_1 = ref nb_meeples_j_1 in let current_nb_meeples_j_2 = (print_int (-1);ref nb_meeples_j_2) in
															print_string "fun_1_";let move_carcassonne num_tuile (x,y,rot_tuile,meeple)  = (print_string "x and y and rot_tuile ; ";print_int x; print_string " "; print_int y; print_string " "; print_int rot_tuile; let nb_meeples_1,nb_meeples_2,score_1,score_2,_ = jouer_coup_simple table_jeu (!current_player) (Some(x,y,rot_tuile,meeple)) nb_meeples_j_1 nb_meeples_j_2 (!current_score_j_1) (!current_score_j_2)
															in print_string "Tryyyiiinnngg";current_score_j_1 := score_1;current_score_j_2 := score_2;current_nb_meeples_j_1 :=  nb_meeples_1; current_nb_meeples_j_2 :=  nb_meeples_2;current_player := (echanger_joueur (!current_player)); 
															(let rec remove_from_deck tuile = function
															|t::q -> if t = tuile then q else t::(remove_from_deck tuile q) |_ -> []
															in current_deck := remove_from_deck (tiles_of_int num_tuile) lst_piochage );print_string "Finished move") in 
															print_string "fun_2_";let possible_moves_carcassonne num_tuile = let tuile = (tiles_of_int num_tuile) in
																(let rec pos_possible i =if (if i = 0 then true else  
																	(if i = 1 then ((rotation_tuile 1 tuile) <> tuile) else 
																		(if i = 2 then (((rotation_tuile 2 tuile) <> tuile)&&((rotation_tuile 2 tuile) <> (rotation_tuile 1 tuile) )) else 
																			(if i = 3 then (((rotation_tuile 3 tuile) <> tuile)&&((rotation_tuile 3 tuile) <> (rotation_tuile 1 tuile))&&((rotation_tuile 3 tuile) <> (rotation_tuile 2 tuile))) else false   ) )) ) then ( (pos_possible (i + 1))@(List.map (fun elem -> (elem,i)) (lst_possible table (rotation_tuile i (tiles_of_int num_tuile)) ))  ) else (if i > 3 then [] else pos_possible (i + 1))
																in let lst_tuiles_possibles = pos_possible 0 in let tuile = (tiles_of_int num_tuile) in if ( ((joueur_j = J1)&&(nb_meeples_j_1 <= 0))|| ((joueur_j = J2)&&(nb_meeples_j_2 <= 0)) ) then Array.of_list (List.map (fun ((x_1,y_1),rot_1) -> (x_1,y_1,rot_1,Libre) ) lst_tuiles_possibles) else
																 let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
																	
																	
												
												
												
																let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
																			Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
																										then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
																												(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
																			let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																																|Vide -> false
																																																																																																|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																																							|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																																							|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
																			in
																				let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
																				Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
																											then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
																													(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
																				let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																																	|Vide -> false
																																																																																																	|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																																								|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																																								|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
																					in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
																					(match tuile with
																						|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																																										let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																																											(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;
												
																																								let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																														|Vide -> false
																																																																																														|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																																					|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																																					|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																																				in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
																							|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
																							|_ -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) )) 
																							) lst_tuiles_possibles) 
																						
																in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in Array.of_list every_move)
												
															in print_string "fun_3_";let get_all_tiles_carcassonne = (fun () -> let acc = ref [] in let visited = Hashtbl.create 72 in  
															(List.iter (fun tuile_0 ->if not (Hashtbl.mem visited tuile_0) then (Hashtbl.add visited tuile_0 []; if ( Array.exists (fun i -> lst_possible table (rotation_tuile i tuile_0) != [] )  [|0;1;2;3|]) then acc:= (int_of_tiles tuile_0)::(!acc) ) ) !current_deck;Array.of_list !acc))
															in let arbre_base = (let next_tile = List.hd lst_piochage in Node ( (int_of_tiles (next_tile), ref ( init_donnees_carcassone () )  , ref [] )  )) in
												
												
												
																print_string "begin_process____";let rec recherche_mcts itermax =
																	for i = 1 to itermax do
																	print_endline ""; print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	(*print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";
																	print_string "Number Processes to do : ";print_int i;print_endline "";print_string "Number Processes to do : ";print_int i;print_endline "";*)
																	
																	let resultat = ref (0,0) in if itermax > 0 then 
																 ((let subject,branch_tree = selection_expansion (Tuile(arbre_base)) init_donnees_carcassone (eval_node constant_eval) move_carcassonne possible_moves_carcassonne get_all_tiles_carcassonne in (match subject with
															|Fin_1 -> let _,_,score_final_1,score_final_2,_ = simulation_simple  table_jeu (!current_player) (!current_deck) (!current_nb_meeples_j_1) (!current_nb_meeples_j_2) (!current_score_j_1) (!current_score_j_2) true
																				in  resultat := (score_final_1,score_final_2);
															|Tuile(Node(num_tuile,_,_)) -> let rec mettre_en_tete elem = (function |t1::t2::q -> if t2 = elem then t2::t1::q else (if t1 = elem then t1::t2::q else let temp = (mettre_en_tete elem (t2::q)) in if (List.hd temp) = elem then elem::t1::(List.tl temp) else t1::t2::q  ) |m -> m )
																			in current_deck :=  mettre_en_tete (tiles_of_int num_tuile) (!current_deck); let _,_,score_final_1,score_final_2,_ = simulation_simple  table_jeu (!current_player) (!current_deck) (!current_nb_meeples_j_1) (!current_nb_meeples_j_2) (!current_score_j_1) (!current_score_j_2) true
																			in resultat := (score_final_1,score_final_2);
															|Mouvement(Node(_,_,_)) -> let _,_,score_final_1,score_final_2,_ = simulation_simple  table_jeu (!current_player) (!current_deck) (!current_nb_meeples_j_1) (!current_nb_meeples_j_2) (!current_score_j_1) (!current_score_j_2) true
																										in  resultat := (score_final_1,score_final_2) );
															mise_a_jour (fun (a_1,b_1) -> (a_1 + 1, b_1 + (let c_1,d_1 = !resultat in if joueur_j = J1 then (if c_1 > d_1 then 1 else (if c_1 < d_1 then (-1) else 0)) else (if c_1 < d_1 then 1 else (if c_1 > d_1 then (-1) else 0)) )  ) ) branch_tree);reinitialiser_jeu_plat ();Gc.full_major ()) done;
															
														in recherche_mcts itermax; let best_move = (let max_node = ref neg_infinity in let next_move = ref (-1,-1,-1,Libre) in let donnees_parent,mouvements = (match arbre_base with |Node(_,donnees,mouvements_0) -> (donnees,mouvements_0) ) in
																		((List.iter (fun move -> match move with 
																								|Fin -> ()
																								|Node(a,donnees_exper,c) -> if !max_node = neg_infinity then 
																									(max_node := eval_node 2. !donnees_exper !donnees_parent; next_move := a) else 
																										(let value = eval_node 2. !donnees_exper !donnees_parent in if value > (!max_node) then (max_node := value;next_move := a) )  ) !mouvements); if !next_move = (-1,-1,-1,Libre) then None else Some(!next_move) )) in best_move end;;
												









let rec expectimax depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 =
let table,meeples = jeu_plat in let value_heuristic = fun () -> 0. in if (depth = 0 || shuffle_arr_without_fail lst_piochage table = [||] ) then value_heuristic () else
	begin
	let distrib_tuiles = Hashtbl.create 72 in let score = ref 0. in
		List.iter (fun tuile -> if Hashtbl.mem distrib_tuiles tuile then Hashtbl.replace distrib_tuiles tuile ( (Hashtbl.find distrib_tuiles tuile) + 1 ) else 
			if (Array.exists (fun i -> lst_possible table (rotation_tuile i tuile) != []) [|0;1;2;3|] ) then  Hashtbl.add distrib_tuiles tuile 1 ) lst_piochage;
	let nb_tuiles_possibles_total = (let cpt = ref 0 in (Hashtbl.iter (fun _ -> fun b -> cpt := b + (!cpt) );!cpt)) in
	(Hashtbl.iter (fun tuile -> fun nb_tuile -> let rec mettre_en_tete elem = (function |t1::t2::q -> if t2 = elem then t2::t1::q else (if t1 = elem then t1::t2::q else let temp = (mettre_en_tete elem (t2::q)) in if (List.hd temp) = elem then elem::t1::(List.tl temp) else t1::t2::q  ) |m -> m ) 
		in let current_deck =  mettre_en_tete tuile lst_piochage in let copy_table,copy_meeples = (Array.map (fun arr -> Array.copy arr ) table , Array.copy meeples) in
		let valeur = negamax depth (copy_table,copy_meeples) current_deck joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 in score := (valeur *. ((float_of_int nb_tuile) /. (float_of_int nb_tuiles_possibles_total)) ) +. (!score)
		
		
		) distrib_tuiles ; !score)
	end

and negamax depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 = let score = ref neg_infinity in let tuile = List.hd lst_piochage in let t = tuile in let reste_piochage = List.tl lst_piochage in
	let table,meeples = jeu_plat in
	let possibilities =  let possibilities_0 = Array.map (fun k -> (k,0)) ( Array.of_list (lst_possible table t) ) in 
	Array.append possibilities_0 (let possibilities_1 = Array.map (fun k -> (k,1)) ( Array.of_list (lst_possible table (rotation_tuile 1 t )) ) in 
	Array.append possibilities_1 (let possibilities_2 = Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 t ))) in 
	Array.append possibilities_2 (let possibilities_3 = Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t )))  in possibilities_3) ) ) in 

	((Array.iter ( fun ((x_0,y_0),rot_0) -> let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
					
					



	let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
				Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
											then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
													(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
				let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																	|Vide -> false
																																																																																	|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																								|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																								|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
				in
					let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
					Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
												then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
														(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
					let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																		|Vide -> false
																																																																																		|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																									|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																									|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
						in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
						(match tuile with
							|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																											let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																												(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;

																									let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																															|Vide -> false
																																																																															|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																						|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																						|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																					in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
								|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
								|_ -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) ) 
								) ) [((x_0,y_0),rot_0)]) 
							
	in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in
	
	

	List.iter ( fun (x,y,rot,meeple) -> let copy_table,copy_meeples = (let table,meeples = jeu_plat in (Array.map (fun arr -> Array.copy arr) table,Array.copy meeples)) in let nv_score_j_1 = ref 0 in let nv_score_j_2 = ref 0 in
	let nv_nb_meeples_j_1,nv_nb_meeples_j_2,fst_nv_score_j_1,fst_nv_score_j_2,_ =  jouer_coup_simple (copy_table,copy_meeples) joueur_j (Some(x,y,rot,meeple)) nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2
	in nv_score_j_1 := fst_nv_score_j_1; nv_score_j_2 := fst_nv_score_j_2 ;
	let coups_suivants = shuffle_arr_without_fail reste_piochage copy_table in 
		if coups_suivants = [||] then 
			(let _,_,snd_nv_score_j_1,snd_nv_score_j_2,_ = simulation_simple  (copy_table,copy_meeples) (echanger_joueur joueur_j) [] nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) true  in 
				nv_score_j_1 := snd_nv_score_j_1; nv_score_j_2 := snd_nv_score_j_2);
	let valeur_expectimax = -. (expectimax (depth - 1) (copy_table,copy_meeples) reste_piochage (echanger_joueur joueur_j) nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) )
			in if (!score = neg_infinity) then score := valeur_expectimax else score := max valeur_expectimax (!score)  )  every_move ) possibilities);!score);;








































				let rec star_1 alpha beta player_turn(*1 si c'est le premier joueur,-1 sinon*) depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 =
				let table,meeples = jeu_plat in 
				let value_heuristic = fun () -> 0. in let u(*max_heuristique*) = 0. in let l(*min_heuristique*) = 0. in if (depth = 0 || shuffle_arr_without_fail lst_piochage table = [||] ) then (value_heuristic ()) *. (float_of_int player_turn) else
					begin
					let distrib_tuiles = Hashtbl.create 72 in let score = ref 0. in let cur_x = ref 0. in let cur_y = ref 1. in 
						List.iter (fun tuile -> if Hashtbl.mem distrib_tuiles tuile then Hashtbl.replace distrib_tuiles tuile ( (Hashtbl.find distrib_tuiles tuile) + 1 ) else 
							if (Array.exists (fun i -> lst_possible table (rotation_tuile i tuile) != []) [|0;1;2;3|] ) then  Hashtbl.add distrib_tuiles tuile 1 ) lst_piochage;
					let nb_tuiles_possibles_total = (let cpt = ref 0 in (Hashtbl.iter (fun _ -> fun b -> cpt := b + (!cpt) );!cpt)) in
					let result = ref (!cur_x) in let immediate_answer = ref false in
					(Hashtbl.iter (fun tuile -> fun nb_tuile -> let probability = (float_of_int nb_tuile)/.(float_of_int nb_tuiles_possibles_total) in cur_y := (!cur_y) -. probability;
						let cur_alpha = ( alpha -. (!cur_x) -. (u *. (!cur_y))) /. probability in let cur_beta = (beta -. (!cur_x) -. (l*.(!cur_y))) /. probability in
						let ax = max cur_alpha u in let bx = min cur_beta l in let copy_table,copy_meeples = (Array.map (fun arr -> Array.copy arr ) table , Array.copy meeples) in 
						
						let rec mettre_en_tete elem = (function |t1::t2::q -> if t2 = elem then t2::t1::q else (if t1 = elem then t1::t2::q else let temp = (mettre_en_tete elem (t2::q)) in if (List.hd temp) = elem then elem::t1::(List.tl temp) else t1::t2::q  ) |m -> m )
						in let current_deck =  mettre_en_tete tuile lst_piochage in let valeur = negamax_star_1 ax bx player_turn depth (copy_table,copy_meeples) current_deck joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 in 
						if valeur >= cur_beta then (immediate_answer := true;result := beta;Hashtbl.clear distrib_tuiles) else (if valeur <= cur_alpha then (immediate_answer := true;result := alpha;Hashtbl.clear distrib_tuiles) else cur_x := (!cur_x ) +. (probability *. valeur) ); 
						score := (valeur *. ((float_of_int nb_tuile) /. (float_of_int nb_tuiles_possibles_total)) ) +. (!score)
						
						
						) distrib_tuiles ; if (!immediate_answer) then !result else !cur_x )
					end
				
				and negamax_star_1 ax bx player_turn (*1 si c'est le premier joueur, -1 sinon*) depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 = let score = ref neg_infinity in let tuile = List.hd lst_piochage in let t = tuile in let reste_piochage = List.tl lst_piochage in
				let table,meeples = jeu_plat in
				let possibilities =  let possibilities_0 = Array.map (fun k -> (k,0)) ( Array.of_list (lst_possible table t) ) in 
				Array.append possibilities_0 (let possibilities_1 = Array.map (fun k -> (k,1)) ( Array.of_list (lst_possible table (rotation_tuile 1 t )) ) in 
				Array.append possibilities_1 (let possibilities_2 = Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 t ))) in 
				Array.append possibilities_2 (let possibilities_3 = Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t )))  in possibilities_3) ) ) in 


				(ignore((Array.exists ( fun ((x_0,y_0),rot_0) -> let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
								
								
			
			
			
				let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
							Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
														then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
																(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
							let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																				|Vide -> false
																																																																																				|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																											|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																											|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
							in
								let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
								Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
															then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
																	(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
								let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																					|Vide -> false
																																																																																					|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																												|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																												|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
									in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
									(match tuile with
										|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																														let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																															(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;
			
																												let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																		|Vide -> false
																																																																																		|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																									|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																									|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																								in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
											|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
											|_ -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) ) ) ) [((x_0,y_0),rot_0)]) 
										
				in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in
				
				
				
				List.exists ( fun (x,y,rot,meeple) -> let copy_table,copy_meeples = (let table,meeples = jeu_plat in (Array.map (fun arr -> Array.copy arr) table,Array.copy meeples)) in let nv_score_j_1 = ref 0 in let nv_score_j_2 = ref 0 in
				let nv_nb_meeples_j_1,nv_nb_meeples_j_2,fst_nv_score_j_1,fst_nv_score_j_2,_ =  jouer_coup_simple (copy_table,copy_meeples) joueur_j (Some(x,y,rot,meeple)) nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2
				in nv_score_j_1 := fst_nv_score_j_1; nv_score_j_2 := fst_nv_score_j_2 ;
				let coups_suivants = shuffle_arr_without_fail reste_piochage copy_table in 
					if coups_suivants = [||] then 
						(print_string "Finishing....";flush stdout;let _,_,snd_nv_score_j_1,snd_nv_score_j_2,_ = simulation_simple  (copy_table,copy_meeples) (echanger_joueur joueur_j) [] nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) true  in 
							nv_score_j_1 := snd_nv_score_j_1; nv_score_j_2 := snd_nv_score_j_2);
				let valeur_star_1 = -. (star_1 (-.bx) (-.ax) (-player_turn) (depth - 1) (copy_table,copy_meeples) reste_piochage (echanger_joueur joueur_j) nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) )
						in if (!score = neg_infinity) then score := valeur_star_1 else (score := max valeur_star_1 (!score));
						if !score <= ax then (score := ax;true) else (if !score >= bx then (score := bx;true) else false  )  )  every_move) ) possibilities);!score);;



































						let rec star_2_p_5 alpha beta prob_factor (*player_turn(*1 si c'est le premier joueur,-1 sinon*)*) depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 =
							Gc.full_major ();print_int depth ; print_string " for Star2.5    ";let table,meeples = jeu_plat in let cur_beta = ref beta in let cur_lower_bound = ref alpha in
							let value_heuristic = fun () -> float_of_int (score_j_1 - score_j_2) in let u(*max_heuristique*),l(*min_heuristique*) = (let cpt = ref 0 in (Array.iter(fun arr -> Array.iter (fun tile -> match tile with |T(_) -> incr cpt |_ -> () ) arr) table;(float_of_int ((2 * !cpt) + score_j_1 - score_j_2),float_of_int(score_j_1 - score_j_2 - (2 * !cpt)) )) ) in 	
							if (depth = 0 || shuffle_arr_without_fail lst_piochage table = [||] ) then (let _ = (print_int depth ; print_string "    ") in ();value_heuristic ()) else
								begin
									let distrib_tuiles = Hashtbl.create 72 in let score = ref 0. in let cur_x = ref 0. in let cur_y = ref 1. in let cur_w = ref 0. in let fst_tuile_container = ref Vide in 
									List.iter (fun tuile -> if Hashtbl.mem distrib_tuiles tuile then Hashtbl.replace distrib_tuiles tuile ( (Hashtbl.find distrib_tuiles tuile) + 1 ) else 
										if (Array.exists (fun i -> lst_possible table (rotation_tuile i tuile) != []) [|0;1;2;3|] ) then  (if !fst_tuile_container = Vide then fst_tuile_container := T(tuile);Hashtbl.add distrib_tuiles tuile 1) ) lst_piochage;
								let nb_tuiles_possibles_total = (let cpt = ref 0 in (Hashtbl.iter (fun _ -> fun b -> cpt := b + (!cpt) );!cpt)) in 
								let fst_tuile = (match !fst_tuile_container with |T(t) -> t |_ -> a) in let nb_fst_tuile = Hashtbl.find distrib_tuiles fst_tuile in 
								let probab_fst_tuile = (float_of_int nb_fst_tuile) /. (float_of_int nb_tuiles_possibles_total) in 
								let cur_alpha = ((alpha -. u *.(1. -. probab_fst_tuile)) /. probab_fst_tuile) in let ax = max l cur_alpha in

								let result = ref beta in let immediate_answer = ref false in let distrib_tuiles_copy = Hashtbl.copy distrib_tuiles in
								Hashtbl.iter (fun tuile -> fun nb_tuile -> let probab_tuile = (float_of_int nb_tuile) /. (float_of_int nb_tuiles_possibles_total) in
								 cur_y := (!cur_y) -. probab_tuile;
								 let cur_beta = (beta -. (l *. !cur_y) -. (!cur_x)) /. probab_tuile in
								 let bx = min u cur_beta in let copy_table,copy_meeples = (Array.map (fun arr -> Array.copy arr ) table , Array.copy meeples) in 
								 let rec mettre_en_tete elem = (function |t1::t2::q -> if t2 = elem then t2::t1::q else (if t1 = elem then t1::t2::q else let temp = (mettre_en_tete elem (t2::q)) in if (List.hd temp) = elem then elem::t1::(List.tl temp) else t1::t2::q  ) |m -> m ) 
								 in let reste_piochage_probe =  mettre_en_tete tuile lst_piochage in
								 let value = nProbe ax bx prob_factor (*player_turn*) depth (copy_table,copy_meeples) reste_piochage_probe joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 in
								 cur_w := (!cur_w) +. value;if (value >= cur_beta) then (Hashtbl.clear distrib_tuiles_copy;immediate_answer := true) ) distrib_tuiles_copy;Hashtbl.clear distrib_tuiles_copy;
								 match !immediate_answer with
								 |true -> print_string "Elagaaaage!!!";flush stdout;!result
								 |false ->

									result := !cur_x;cur_y := 1.;cur_x := 0.;
								(Hashtbl.iter (fun tuile -> fun nb_tuile -> let probability = (float_of_int nb_tuile)/.(float_of_int nb_tuiles_possibles_total) in cur_y := (!cur_y) -. probability;
									let cur_alpha = ( alpha -. (!cur_x) -. (!cur_w)) /. probability in let cur_beta = (beta -. (!cur_x) -. (l *. (!cur_y))) /. probability in
									let ax = max cur_alpha u in let bx = min cur_beta l in let copy_table,copy_meeples = jeu_plat (*(Array.map (fun arr -> print_string "copying.....";flush stdout;Array.copy arr ) table , Array.copy meeples)*) in
									
									let rec mettre_en_tete elem = (function |t1::t2::q -> if t2 = elem then t2::t1::q else (if t1 = elem then t1::t2::q else let temp = (mettre_en_tete elem (t2::q)) in if (List.hd temp) = elem then elem::t1::(List.tl temp) else t1::t2::q  ) |m -> m ) 
									in let current_deck =  mettre_en_tete tuile lst_piochage in let valeur = negamax_star_2_p_5(*exactement le meme que negamax_star_1*) ax bx prob_factor (*player_turn*) depth (copy_table,copy_meeples) current_deck joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 in 
									if valeur >= cur_beta then (print_string "Elagaaaage!!!";flush stdout;immediate_answer := true;result := beta;Hashtbl.clear distrib_tuiles) else (if valeur <= cur_alpha then (print_string "Elagaaaage!!!";flush stdout;immediate_answer := true;result := alpha;Hashtbl.clear distrib_tuiles) else cur_x := (!cur_x ) +. (probability *. valeur) ); 
									score := (valeur *. ((float_of_int nb_tuile) /. (float_of_int nb_tuiles_possibles_total)) ) +. (!score)
									
									
									) distrib_tuiles ; if (!immediate_answer) then !result else !cur_x )
								end
							
							and negamax_star_2_p_5(*"presque"exactement le meme que negamax_star_1*) ax bx prob_factor (*player_turn (*1 si c'est le premier joueur, -1 sinon*)*) depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 = let score = ref neg_infinity in let tuile = List.hd lst_piochage in let t = tuile in let reste_piochage = List.tl lst_piochage in
							Gc.full_major ();let table,meeples = jeu_plat in print_string "Negamax......";flush stdout;let value_heuristic = fun () -> float_of_int (score_j_1 - score_j_2) in let dict_all_moves_tables_results = Hashtbl.create 400 in
							let possibilities =  let possibilities_0 = Array.map (fun k -> (k,0)) ( Array.of_list (lst_possible table t) ) in 
							Array.append possibilities_0 (let possibilities_1 = Array.map (fun k -> (k,1)) ( Array.of_list (lst_possible table (rotation_tuile 1 t )) ) in 
							Array.append possibilities_1 (let possibilities_2 = Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 t ))) in 
							Array.append possibilities_2 (let possibilities_3 = Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t )))  in possibilities_3) ) ) in 
							
							let rec list_iter_while_cond cond fun_to_iter= function
									|t::q -> if cond then fun_to_iter t; list_iter_while_cond cond fun_to_iter q;
									|_ -> ()
								in let rec array_iter_while_cond cond fun_to_iter arr = 
									(let j = ref 0 in while (!j < Array.length arr) && cond do (fun_to_iter arr.(!j);incr j) done)
								in let immediate_answer = ref false in
							(ignore(array_iter_while_cond (not !immediate_answer) ( fun ((x_0,y_0),rot_0) -> let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> print_string "Position_Rotation_Negamax...."; flush stdout; table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
											
											
						
						
						
							let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
										Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
																	then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
																			(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
										let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																							|Vide -> false
																																																																																							|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																														|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																														|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
										in
											let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
											Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
																		then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
																				(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
											let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																								|Vide -> false
																																																																																								|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																															|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																															|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
												in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
												(match tuile with
													|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																																	let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																																		(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;
						
																															let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																					|Vide -> false
																																																																																					|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																												|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																												|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																											in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
														|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
														|_ ->  [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) ) ) ) [((x_0,y_0),rot_0)]) 
													
							in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in
							(*let arr_every_move = Array.of_list every_move in*) 
							List.iter( fun (x,y,rot,meeple) -> print_string "Move_Negamax....";flush stdout; let copy_table,copy_meeples = (let table_0,meeples_0 = jeu_plat in (Array.map (fun arr -> Array.copy arr) table_0,Array.copy meeples_0)) in let nv_score_j_1 = ref 0 in let nv_score_j_2 = ref 0 in
							let nv_nb_meeples_j_1,nv_nb_meeples_j_2,fst_nv_score_j_1,fst_nv_score_j_2,_ =  jouer_coup_simple (copy_table,copy_meeples) joueur_j (Some(x,y,rot,meeple)) nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2
							in nv_score_j_1 := fst_nv_score_j_1; nv_score_j_2 := fst_nv_score_j_2 ;
							let coups_suivants = shuffle_arr_without_fail reste_piochage copy_table in 
								if coups_suivants = [||] then 
									(let _,_,snd_nv_score_j_1,snd_nv_score_j_2,_ = simulation_simple  (copy_table,copy_meeples) (echanger_joueur joueur_j) [] nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) true  in 
										nv_score_j_1 := snd_nv_score_j_1; nv_score_j_2 := snd_nv_score_j_2);
										Hashtbl.add dict_all_moves_tables_results (x,y,rot,meeple) ((copy_table,copy_meeples),(nv_nb_meeples_j_1,nv_nb_meeples_j_2,!nv_score_j_1,!nv_score_j_2))  ) every_move)possibilities;


										Hashtbl.iter (fun (x,y,rot,meeple) -> fun  ((copy_table,copy_meeples),(nv_nb_meeples_j_1,nv_nb_meeples_j_2,nv_score_j_1,nv_score_j_2)) -> if depth = 1 then (immediate_answer := true; score := -. float_of_int(nv_score_j_1 - nv_score_j_2) ) else 
											(let valeur_star_2_p_5 = -. (star_2_p_5 (-.bx) (-.ax) prob_factor (*(-player_turn)*) (depth - 1) (copy_table,copy_meeples) reste_piochage (echanger_joueur joueur_j) nv_nb_meeples_j_1 nv_nb_meeples_j_2 (nv_score_j_1) (nv_score_j_2) )
													in (*table.(x).(y) <- Vide; if meeple <> Libre then (Array.iteri(fun index -> fun (x_meeple,y_meeple,occupation,player) -> if  (x_meeple,y_meeple,occupation) = (x,y,meeple) then  meeples.(index) <- (x,y,Libre,player)) meeples);*) 
														if (!score = neg_infinity) then score := valeur_star_2_p_5 else (score := max valeur_star_2_p_5 (!score));
													if !score <= ax then (score := ax;immediate_answer := true) else (if !score >= bx then (score := bx;immediate_answer := true)))  ) dict_all_moves_tables_results );!score)
								
								
								
								
								
								
								and nProbe alpha beta prob_factor (*player_turn*) depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 = let tuile = List.hd lst_piochage in let t = tuile in let reste_piochage = List.tl lst_piochage in
									print_string "nProbe....."; flush stdout;let table,meeples = jeu_plat in let new_alpha = ref alpha in let i = ref 0 in 
									let rec list_iter_while_cond cond fun_to_iter= function
									|t::q -> if cond then fun_to_iter t; list_iter_while_cond cond fun_to_iter q;
									|_ -> ()
								in let rec array_iter_while_cond cond fun_to_iter arr = 
									(let j = ref 0 in while (!j < Array.length arr) && cond do (fun_to_iter arr.(!j);incr j) done)
								in 
								
								let possibilities =  let possibilities_0 = Array.map (fun k -> (k,0)) ( Array.of_list (lst_possible table t) ) in 
								Array.append possibilities_0 (let possibilities_1 = Array.map (fun k -> (k,1)) ( Array.of_list (lst_possible table (rotation_tuile 1 t )) ) in 
								Array.append possibilities_1 (let possibilities_2 = Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 t ))) in 
								Array.append possibilities_2 (let possibilities_3 = Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t )))  in possibilities_3) ) ) in 
				
				
								let immediate_answer = ref false in
								array_iter_while_cond ((!i < prob_factor) & (not (!immediate_answer))  ) (fun ((x_0,y_0),rot_0) -> let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
					
					



								let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
											Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
																		then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
																				(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
											let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																								|Vide -> false
																																																																																								|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																															|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																															|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
											in
												let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
												Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
																			then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
																					(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
												let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																									|Vide -> false
																																																																																									|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																																|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																																|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
													in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
													(match tuile with
														|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																																		let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																																			(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;
							
																																let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																						|Vide -> false
																																																																																						|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																													|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																													|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																												in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
															|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
															|_ -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) ) ) ) [((x_0,y_0),rot_0)]) 
														
								in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in
								
								
								list_iter_while_cond ((!i < prob_factor) && not (!immediate_answer)) ( fun (x,y,rot,meeple) -> print_string "Move_Probe...."; flush stdout; let copy_table,copy_meeples = (let table,meeples = jeu_plat in (Array.map (fun arr -> Array.copy arr) table,Array.copy meeples)) in let nv_score_j_1 = ref 0 in let nv_score_j_2 = ref 0 in
								let nv_nb_meeples_j_1,nv_nb_meeples_j_2,fst_nv_score_j_1,fst_nv_score_j_2,_ =  jouer_coup_simple (copy_table,copy_meeples) joueur_j (Some(x,y,rot,meeple)) nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2
								in nv_score_j_1 := fst_nv_score_j_1; nv_score_j_2 := fst_nv_score_j_2 ;
								let coups_suivants = shuffle_arr_without_fail reste_piochage copy_table in 
									if coups_suivants = [||] then 
										(let _,_,snd_nv_score_j_1,snd_nv_score_j_2,_ = simulation_simple  (copy_table,copy_meeples) (echanger_joueur joueur_j) [] nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) true  in 
											nv_score_j_1 := snd_nv_score_j_1; nv_score_j_2 := snd_nv_score_j_2);
											let value = -.star_2_p_5 (-.beta) (-.alpha) prob_factor (*player_turn*) (depth-1) (copy_table,copy_meeples)  reste_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2  in 
											(*table.(x).(y) <- Vide; if meeple <> Libre then (Array.iteri(fun index -> fun (x_meeple,y_meeple,occupation,player) -> if  (x_meeple,y_meeple,occupation) = (x,y,meeple) then  meeples.(index) <- (x,y,Libre,player)) meeples);*) if value >= beta then 
												(immediate_answer := true; new_alpha := beta) else (if value > alpha then new_alpha := value ); incr i) every_move
								
									) possibilities;!new_alpha;;



			







			
					












			


			
			let arr_tuile = [| [|Vide;Vide;T(rotation_tuile 2 d);T(s) |];[|T(rotation_tuile 3 o);T(x);T(a)|]; [|T(rotation_tuile 1 v);T(rotation_tuile (-1) k)|] |];;
			let arr_tuile_ville = [| [|Vide;Vide;T(j)|];[|Vide;T(c);T(rotation_tuile 2 p)|];[|T(rotation_tuile 2 r);T(rotation_tuile 1 g);T(h)|];[|Vide;T(n);T(rotation_tuile (-1) n)|] |];;
			let arr_tuile_route = [| [|Vide;Vide;Vide;Vide|];[|T(x);T(u);T(j);Vide|];[|T(rotation_tuile 1 v);T(rotation_tuile 2 d);T(w)|];[|Vide;Vide;Vide|] |];;
			let arr_tuile_route_cercle_extensible = [|[|T(rotation_tuile 2 v);T(w)|];[|T(rotation_tuile 1 v);T(v)|]|];;
			arr_tuile.(1).(0);;
			let a,b,c,d = reseau_prairie arr_tuile 0 3 Droite_axe Bas_axe in (a,b,Hashtbl.length c,d);;
			extension_prairie (rotation_tuile 2 d) Gauche_axe Bas_axe;;
			extension_prairie a Droite_axe Bas_axe;;
			reseau_prairie arr_tuile 1 0 Droite_axe Haut_axe;;
			reseau_ville arr_tuile_ville 3 2 Gauche;;
			rotation_tuile (-1) n;;
			reseau_route arr_tuile_route 1 0 Droite;;
			print_endline "";print_endline "";print_endline "";print_endline "";reseau_route arr_tuile_route_cercle_extensible 0 1 Gauche;;
			print_endline "";print_endline "";print_endline "";print_endline "";reseau_prairie arr_tuile_route 1 0 Droite_axe Bas_axe;;
			let deck_standard = [a;a;b;b;b;b;c;d;d;d;e;e;e;e;e;f;f;g;h;h;h;i;i;j;j;j;k;k;k;l;l;l;m;m;n;n;n;o;o;p;p;p;q;r;r;r;s;s;t;u;u;u;u;u;u;u;u;v;v;v;v;v;v;v;v;v;w;w;w;w;x];;
			let deck_compr = List.map (fun tuile -> int_of_tiles tuile) deck_standard;;
			let rec sub_deck sub_size original_deck = if (original_deck =[]||sub_size <= 0) then [] else (let pioche_rand = Random.int (List.length original_deck) in let pioche_tuile = List.nth original_deck pioche_rand
			in let rec remove_from_deck elem = function |t::q -> if t = elem then q else t::(remove_from_deck elem q) |_ -> [] in let new_original_deck = remove_from_deck pioche_tuile original_deck in 
			pioche_tuile::(sub_deck (sub_size - 1) new_original_deck ));;
			let melange = shuffle_lst deck_compr;;
			let deck_compresse = [];;
			rotation_tuile 2 k;;
			let table = let arr =  Array.make_matrix 145 145 Vide in arr.(72).(72) <- T(d);arr;;
			let meeples = Array.make 25 (-1,-1,Libre,J1);;


			let rec partie_full_mcts constant_mcts1 constant_mcts2 table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j score_j_1 score_j_2 historique itermax_1 itermax_2  =
			(*let current_nb_meeples_j_1 = ref (nb_meeples_j_1) in let current_nb_meeples_j_2 = ref (nb_meeples_j_2) in 
			let current_deck = ref (lst_piochage) in let current_player = ref joueur_j in let current_score_j_1 = ref score_j_1 in let current_score_j_2 = ref score_j_2 in let cpt = ref 1 in*)
			let table,meeples = table_jeu in let hist_1,hist_2,hist_3,hist_4,hist_5,hist_6,hist_7,hist_8,hist_9,hist_10,hist_11,hist_12,hist_13,hist_14,hist_15,hist_16,hist_17,hist_18,hist_19,hist_20 = historique in
			if shuffle_arr_without_fail lst_piochage table != [||] then
				(print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";
				let coup = (base_jeu_mcts table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j (if joueur_j = J1 then constant_mcts1 else constant_mcts2) score_j_1 score_j_2 (if joueur_j = J1 then itermax_1 else itermax_2) ) in
				let new_nb_meeples_j_1,new_nb_meeples_j_2,new_score_j_1,new_score_j_2,new_statistique =  simulation table_jeu joueur_j [a] coup nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 hist_1 hist_2 hist_3 hist_4 hist_5 hist_6 hist_7 hist_8 hist_9 hist_10 hist_11 hist_12 hist_13 hist_14 hist_15 hist_16 hist_17 hist_18 hist_19 hist_20 false
				in Gc.full_major ();partie_full_mcts constant_mcts1 constant_mcts2 table_jeu new_nb_meeples_j_1 new_nb_meeples_j_2 (List.tl lst_piochage) (echanger_joueur joueur_j) new_score_j_1 new_score_j_2 new_statistique itermax_1 itermax_2
				(*in current_deck := (List.tl !current_deck); current_nb_meeples_j_1 := new_nb_meeples_j_1; current_nb_meeples_j_2 := new_nb_meeples_j_2; current_score_j_1 := new_score_j_1; current_score_j_2 := new_score_j_2; current_player := (echanger_joueur !current_player)*))
			else
			simulation table_jeu joueur_j [] None nb_meeples_j_1 nb_meeples_j_1 score_j_1 score_j_2 hist_1 hist_2 hist_3 hist_4 hist_5 hist_6 hist_7 hist_8 hist_9 hist_10 hist_11 hist_12 hist_13 hist_14 hist_15 hist_16 hist_17 hist_18 hist_19 hist_20 true ;;

			let rec partie_full_mcts_mctso constant_mcts1 constant_mcts2 table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j score_j_1 score_j_2 historique itermax_1 itermax_2  =
			let table,meeples = table_jeu in let hist_1,hist_2,hist_3,hist_4,hist_5,hist_6,hist_7,hist_8,hist_9,hist_10,hist_11,hist_12,hist_13,hist_14,hist_15,hist_16,hist_17,hist_18,hist_19,hist_20 = historique in
			if shuffle_arr_without_fail lst_piochage table != [||] then
				(print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";
				let coup =if joueur_j = J1 then (base_jeu_mcts table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j constant_mcts1 score_j_1 score_j_2 itermax_1 )
				else (base_jeu_mctso table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j constant_mcts2 score_j_1 score_j_2 itermax_2 )  in
				let new_nb_meeples_j_1,new_nb_meeples_j_2,new_score_j_1,new_score_j_2,new_statistique =  simulation table_jeu joueur_j [a] coup nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 hist_1 hist_2 hist_3 hist_4 hist_5 hist_6 hist_7 hist_8 hist_9 hist_10 hist_11 hist_12 hist_13 hist_14 hist_15 hist_16 hist_17 hist_18 hist_19 hist_20 false
				in Gc.full_major ();partie_full_mcts_mctso constant_mcts1 constant_mcts2 table_jeu new_nb_meeples_j_1 new_nb_meeples_j_2 (List.tl lst_piochage) (echanger_joueur joueur_j) new_score_j_1 new_score_j_2 new_statistique itermax_1 itermax_2)
			else
			simulation table_jeu joueur_j [] None nb_meeples_j_1 nb_meeples_j_1 score_j_1 score_j_2 hist_1 hist_2 hist_3 hist_4 hist_5 hist_6 hist_7 hist_8 hist_9 hist_10 hist_11 hist_12 hist_13 hist_14 hist_15 hist_16 hist_17 hist_18 hist_19 hist_20 true ;;





			let statistique_vide = (0,0,[],[],([],[]),([],[]),[],[],[],[],[],[],[],[],[],[],[],[],[],[]);;
			(*let deck_rand = shuffle_lst_without_fail (sub_deck 20 deck_standard) table in ( (List.map (fun a -> int_of_tiles a) deck_rand) ,partie_full_mcts  2. 7. (table,meeples) 8 8 deck_rand J1 0 0 statistique_vide 10 10);;*)

			(*let deck_rand = shuffle_lst_without_fail deck_standard table in ( (List.map (fun a -> int_of_tiles a) deck_rand) ,partie_full_mcts  2. 7. (table,meeples) 8 8 deck_rand J1 0 0 statistique_vide 25 25);;*)

			let table = let arr =  Array.make_matrix 145 145 Vide in arr.(72).(72) <- T(d);arr;;
			let meeples = Array.make 25 (-1,-1,Libre,J1);;

			let game_states_tables = Array.init 100 (fun _ -> Array.map (fun elem -> Array.copy elem) table);;
			let lst_game_states_tables_init = Array.make 100 [];;
			let lst_game_states_tables_final = Array.make 100 [];;
			let game_states_meeples = Array.init 100 (fun _ -> Array.copy meeples);;
			let game_states_meeples_safe = Array.init 100 (fun _ -> Array.copy meeples);;
			let historique_game_states = Array.make 100 (0,0,0,0,statistique_vide);;
			let decks_game_state = Array.make 100 deck_standard;;
			let results_game_states = Array.make 100 (0,0,0,0,statistique_vide);;
			let competition_mcts_mctso = [|0.;0.7;sqrt 2.;(*1.8;2.3;*)2.8;(*3.;4.;6.2;*)7. |];;
			let competition_mcts = [|(0.,7.);(0.,1.);(2.,2.6);(3.,5.7);(sqrt 2.,3.)|];;




















			let next_move_star_2_p_5 alpha beta prob_factor (*player_turn(*1 si c'est le premier joueur,-1 sinon*)*) depth jeu_plat lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 =
				 let table,meeples = jeu_plat in if shuffle_arr_without_fail lst_piochage table = [||] then (None,[]) else 
					let tuile = List.hd lst_piochage in let t = tuile in let reste_piochage = List.tl lst_piochage in let current_best_moves = ref [] in let score_best_move = ref neg_infinity in
				begin
					let possibilities =  let possibilities_0 = Array.map (fun k -> (k,0)) ( Array.of_list (lst_possible table t) ) in 
					Array.append possibilities_0 (let possibilities_1 = Array.map (fun k -> (k,1)) ( Array.of_list (lst_possible table (rotation_tuile 1 t )) ) in 
					Array.append possibilities_1 (let possibilities_2 = Array.map (fun k -> (k,2)) (Array.of_list (lst_possible table (rotation_tuile 2 t ))) in 
					Array.append possibilities_2 (let possibilities_3 = Array.map (fun k -> (k,3)) (Array.of_list (lst_possible table (rotation_tuile 3 t )))  in possibilities_3) ) ) in
					
					(Array.iter ( fun ((x_0,y_0),rot_0) -> let every_possibility = (List.map ( fun ((x_1,y_1),rot_1) -> table.(x_1).(y_1) <- T(rotation_tuile rot_1 tuile ); 
					
					



	let coins_prairie = coins_distincts_prairie (rotation_tuile rot_1 tuile) in let lst_branches_prairies_visite = ref [] in let reseaux_branches_pr = ref [] in  
				Array.iter (fun (axe,ord) -> if not (List.exists (fun (lst_coord,_) ->  List.exists (fun (axe_0,ord_0) -> compare_coord_prairie (rotation_tuile rot_1 tuile) axe_0 ord_0 axe ord) lst_coord  ) (!reseaux_branches_pr)) 
											then let reseau_pr,branches_pr,_,cloture_pr = reseau_prairie table x_1 y_1 axe ord in 
													(lst_branches_prairies_visite := (List.map (fun coord_branche_visite -> (coord_branche_visite,cloture_pr)) branches_pr)@(!lst_branches_prairies_visite);reseaux_branches_pr := (branches_pr,reseau_pr)::(!reseaux_branches_pr) )) coins_prairie;
				let options_paysan = ref [] in let occupation_branches_prairies = List.map (fun (branches_pr,reseau_pr) -> (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr,let occupation = List.exists (fun ((x_0,y_0),(axe_0,ord_0)) -> match table.(x_0).(y_0) with 
																																																																																	|Vide -> false
																																																																																	|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																								|Employe(Paysan(axe_meeple,ord_meeple)) -> compare_coord_prairie (rotation_tuile rot_1 tuile)  axe_0 ord_0 axe_meeple ord_meeple 
																																																																																																																																								|_ -> false) ) meeples ) reseau_pr in (if not occupation then options_paysan := (List.map (fun (axe_a,ord_a) -> Paysan(axe_a,ord_a) ) branches_pr)::(!options_paysan));occupation)) !reseaux_branches_pr
				in
					let chemins_rt = dir_distincts_route (rotation_tuile rot_1 tuile) in let lst_branches_routes_visite = ref [] in let reseaux_branches_rt = ref [] in  
					Array.iter (fun dir -> if not (List.exists (fun (lst_dir,_) -> List.exists (fun dir_0 -> compare_dir_route (rotation_tuile rot_1 tuile) dir dir_0) lst_dir   ) (!reseaux_branches_rt)) 
												then let reseau_rt,branches_rt,cloture_rt = reseau_route table x_1 y_1 dir in 
														(lst_branches_routes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_rt)) branches_rt)@(!lst_branches_routes_visite);reseaux_branches_rt := (branches_rt,reseau_rt)::(!reseaux_branches_rt) )) chemins_rt;
					let options_voleur = ref [] in let occupation_branches_routes = List.map (fun (branches_rt,reseau_rt) -> (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																																		|Vide -> false
																																																																																		|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																									|Employe(Voleur(dir_meeple)) -> compare_dir_route (rotation_tuile rot_1 tuile)  dir_0 dir_meeple
																																																																																																																																									|_ -> false) ) meeples ) reseau_rt in (if not occupation then options_voleur := (List.map (fun dir_voleur -> Voleur(dir_voleur)) branches_rt)::(!options_voleur));occupation)) !reseaux_branches_rt
						in let meeple_joue = ref Libre in let branches_villes = dir_distincts_ville (rotation_tuile rot_1 tuile) in let lst_branches_villes_visite = ref [] in let reseaux_branches = ref [] in
						(match tuile with
							|(_,t_1,_,_,_) when t_1 = Ville_1 ->		Array.iter (fun dir_branche -> if not (List.exists (fun elem ->compare_dir_ville (rotation_tuile rot_1 tuile) (fst elem) dir_branche) (!lst_branches_villes_visite)) then 
																											let reseau_vl,branches_vl,cloture_vl,_ = reseau_ville table x_1 y_1 dir_branche in 
																												(lst_branches_villes_visite := (List.map (fun dir_branche_visite -> (dir_branche_visite,cloture_vl)) branches_vl)@(!lst_branches_villes_visite);reseaux_branches := (branches_vl,reseau_vl)::(!reseaux_branches) )) branches_villes;

																									let options_chevalier = ref [] in let occupation_branches_villes = List.map (fun (branches_vl,reseau_vl) -> (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl,let occupation = List.exists (fun ((x_0,y_0),dir_0) -> match table.(x_0).(y_0) with 
																																																																															|Vide -> false
																																																																															|T(tuile) -> Array.exists ( fun (x_meeple_0,y_meeple_0,meeple,_) -> (x_0,y_0) = (x_meeple_0,y_meeple_0) &&  (match meeple with	
																																																																																																																																						|Employe(Chevalier(dir_meeple)) -> compare_dir_ville (rotation_tuile rot_1 tuile)  dir_0 dir_meeple 
																																																																																																																																						|_ -> false) ) meeples ) reseau_vl in (if not occupation then options_chevalier := (List.map (fun dir_chevalier -> Chevalier(dir_chevalier)) branches_vl)::(!options_chevalier));occupation)) !reseaux_branches
																					in [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_chevalier@(!options_voleur)))) ; 
								|(_,t_1,_,_,_) when t_1 = Abbaie -> [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) ([Moine]::(!options_paysan@(!options_voleur))) ) 
								|_ ->   [(x_1,y_1,rot_1,Libre)]::(List.map (fun lst -> List.map (fun a -> (x_1,y_1,rot_1,Employe(a))) lst ) (!options_paysan@(!options_voleur)) )) ) [((x_0,y_0),rot_0)])
								
							
	in let every_move =  (let acc = ref [] in (List.iter (fun lst_of_lst -> List.iter (fun lst -> List.iter (fun elem -> acc := elem::(!acc)) lst ) lst_of_lst) every_possibility;!acc)) in
	
	

	List.iter ( fun (x,y,rot,meeple) -> Gc.full_major ();print_string "Moveeee  ::::";print_int (List.length every_move);print_string "  All_positions :::: "; print_int (Array.length possibilities); print_string "  "; print_endline ""; print_endline "";  flush stdout;  let copy_table,copy_meeples =  (let table,meeples = jeu_plat in (Array.map (fun arr -> print_string "copying.....";flush stdout;Array.copy arr) table,Array.copy meeples)) in let nv_score_j_1 = ref 0 in let nv_score_j_2 = ref 0 in
	let nv_nb_meeples_j_1,nv_nb_meeples_j_2,fst_nv_score_j_1,fst_nv_score_j_2,_ =  jouer_coup_simple (copy_table,copy_meeples) joueur_j (Some(x,y,rot,meeple)) nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2
	in nv_score_j_1 := fst_nv_score_j_1; nv_score_j_2 := fst_nv_score_j_2 ;
	let coups_suivants = shuffle_arr_without_fail reste_piochage copy_table in 
		if coups_suivants = [||] then 
			(print_string "????";let _,_,snd_nv_score_j_1,snd_nv_score_j_2,_ = simulation_simple  (copy_table,copy_meeples) (echanger_joueur joueur_j) [] nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) true  in 
				nv_score_j_1 := snd_nv_score_j_1; nv_score_j_2 := snd_nv_score_j_2);
	let new_score = -. (star_2_p_5 neg_infinity infinity prob_factor depth (copy_table,copy_meeples) reste_piochage (echanger_joueur joueur_j) nv_nb_meeples_j_1 nv_nb_meeples_j_2 (!nv_score_j_1) (!nv_score_j_2) )
			in if ((!score_best_move = neg_infinity)||(!current_best_moves = [])) then (score_best_move := new_score;current_best_moves := [(x,y,rot,meeple)]) else (let score_max = (if joueur_j = J1 then min else max) new_score (!score_best_move) in (if !score_best_move < score_max then (score_best_move := score_max;current_best_moves := [(x,y,rot,meeple)]) else (if !score_best_move = score_max then current_best_moves := (x,y,rot,meeple)::(!current_best_moves) ) ) )  )  every_move ) possibilities;
			if !current_best_moves = [] then (None,[]) else let pioche_move = Random.int (List.length !current_best_moves) in (Some(List.nth !current_best_moves pioche_move),!current_best_moves))
	
				end;;









				let rec partie_full_mcts_star	 constant_mcts prob_factor table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j score_j_1 score_j_2 historique itermax depth  =
			let table,meeples = table_jeu in let hist_1,hist_2,hist_3,hist_4,hist_5,hist_6,hist_7,hist_8,hist_9,hist_10,hist_11,hist_12,hist_13,hist_14,hist_15,hist_16,hist_17,hist_18,hist_19,hist_20 = historique in
			if shuffle_arr_without_fail lst_piochage table != [||] then
				(print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";print_string "Turns Remaining : "; print_int (List.length lst_piochage);print_endline "";
				let coup =if joueur_j = J1 then (base_jeu_mcts table_jeu nb_meeples_j_1 nb_meeples_j_2 lst_piochage joueur_j constant_mcts score_j_1 score_j_2 itermax )
				else (fst (next_move_star_2_p_5 neg_infinity infinity prob_factor depth table_jeu lst_piochage joueur_j nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2) )  in
				let new_nb_meeples_j_1,new_nb_meeples_j_2,new_score_j_1,new_score_j_2,new_statistique =  simulation table_jeu joueur_j [a] coup nb_meeples_j_1 nb_meeples_j_2 score_j_1 score_j_2 hist_1 hist_2 hist_3 hist_4 hist_5 hist_6 hist_7 hist_8 hist_9 hist_10 hist_11 hist_12 hist_13 hist_14 hist_15 hist_16 hist_17 hist_18 hist_19 hist_20 false
				in Gc.full_major ();partie_full_mcts_star constant_mcts prob_factor table_jeu new_nb_meeples_j_1 new_nb_meeples_j_2 (List.tl lst_piochage) (echanger_joueur joueur_j) new_score_j_1 new_score_j_2 new_statistique itermax depth)
			else
			simulation table_jeu joueur_j [] None nb_meeples_j_1 nb_meeples_j_1 score_j_1 score_j_2 hist_1 hist_2 hist_3 hist_4 hist_5 hist_6 hist_7 hist_8 hist_9 hist_10 hist_11 hist_12 hist_13 hist_14 hist_15 hist_16 hist_17 hist_18 hist_19 hist_20 true ;;





							(*for i = 0 to 9 do 
				if i <= 4 then 
				(for j = 0 to 9 do let st_1,st_2,st_3,st_4,st_5 = historique_game_states.(i) in
					let stat_1,stat_2,stat_3,stat_4,stat_5 = partie_full_mcts_mctso  competition_mcts_mctso.(i) competition_mcts_mctso.(i) (game_states_tables.(i),game_states_meeples.(i)) st_1 st_2 decks_game_state.(i) J2 st_3  st_4 st_5 25 25


					in results_game_states.( (10 * i) + j) <- (stat_1,stat_2,stat_3,stat_4,stat_5);
					lst_game_states_tables_final.((10 * i) + j) <- (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) game_states_tables.((10 * i) + j);!acc)
				done;)
			else
				(for j = 0 to 9 do let st_1,st_2,st_3,st_4,st_5 = historique_game_states.(i) in
					
					let stat_1,stat_2,stat_3,stat_4,stat_5 = partie_full_mcts  (fst (competition_mcts.(i - 5))) (snd (competition_mcts.(i - 5))) (game_states_tables.(i - 5),game_states_meeples.(i - 5)) st_1 st_2 decks_game_state.(i - 5) J2 st_3  st_4 st_5 25 25


					in results_game_states_init.( (10 * i) + j) <- (stat_1,stat_2,stat_3,stat_4,stat_5);
					lst_game_states_tables_final.((10 * i) + j) <- (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) game_states_tables.((10 * i) + j);!acc);
				done;)
			done;;*)


			(*  for i = 90 to 99 do
				let _ = print_int i;print_string "   "; in let deck_rand = shuffle_lst_without_fail (sub_deck 35 deck_standard) table in let rec remove_from_deck elem = function |t::q -> if t = elem then q else t::(remove_from_deck elem q) |_ -> [] in 
				let remainder_deck = (let deck = ref deck_standard in List.iter (fun a -> deck := remove_from_deck a !deck) deck_rand;!deck)
				in let st_1,st_2,st_3,st_4,st_5 = simulation_simple (game_states_tables.(i),game_states_meeples.(i)) J1 deck_rand 8 8 0 0 false
				in historique_game_states.(i) <- (st_1,st_2,st_3,st_4,st_5);decks_game_state.(i) <- remainder_deck;game_states_meeples_safe.(i) <- Array.copy game_states_meeples.(i);
				lst_game_states_tables_init.(i) <- (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) game_states_tables.(i);!acc)
			done;;  *)

			let table_mcts_star = let arr =  Array.make_matrix 145 145 Vide in arr.(72).(72) <- T(d);arr;;
			let results_partie_mcts_star = ref (0,0,0,0,statistique_vide);;
			let historique_mcts_star = ref (0,0,0,0,statistique_vide);;
			let deck_rand = shuffle_lst_without_fail (sub_deck 59 deck_standard) table;;
			let meeples_mcts_star = Array.copy meeples;;
			let meeples_mcts_star_safe = ref  [||];;
			let lst_partie_mcts_star_final = ref [];;
			let lst_partie_mcts_star_init = ref [];;
			let table_mcts_star_init = ref (let arr =  Array.make_matrix 145 145 Vide in arr.(72).(72) <- T(d);arr);;
			let rec remove_from_deck elem = function |t::q -> if t = elem then q else t::(remove_from_deck elem q) |_ -> [] in 
				let remainder_deck = (let deck = ref deck_standard in List.iter (fun a -> deck := remove_from_deck a !deck) deck_rand;!deck)
				in let st_1,st_2,st_3,st_4,st_5 = simulation_simple (table_mcts_star,meeples_mcts_star) J1 deck_rand 8 8 0 0 false
				in historique_mcts_star := (st_1,st_2,st_3,st_4,st_5); meeples_mcts_star_safe  := Array.copy meeples_mcts_star;
				table_mcts_star_init := Array.map (fun elem -> Array.copy elem) table_mcts_star;
				lst_partie_mcts_star_init := (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) table_mcts_star;!acc);
				results_partie_mcts_star := partie_full_mcts_star  3. 5 (table_mcts_star,meeples_mcts_star) st_1 st_2 remainder_deck J2 st_3  st_4 st_5 25 0;

				lst_partie_mcts_star_final := (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) table_mcts_star;!acc);;
			

				
			


			(*for i = 2 to 2 do 
				(for j = 0 to 9 do let st_1,st_2,st_3,st_4,st_5 = historique_game_states.((10 * i) + j) in
					let stat_1,stat_2,stat_3,stat_4,stat_5 = partie_full_mcts_mctso  competition_mcts_mctso.(i) competition_mcts_mctso.(i) (game_states_tables.((10 * i) + j),game_states_meeples.((10 * i) + j)) st_1 st_2 decks_game_state.((10 * i) + j) J2 st_3  st_4 st_5 25 25


					in results_game_states.( (10 * i) + j) <- (stat_1,stat_2,stat_3,stat_4,stat_5);
					lst_game_states_tables_final.((10 * i) + j) <- (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) game_states_tables.((10 * i) + j);!acc);
					print_string "Next Game :            ";print_int ( (10 * i) + j); print_endline "";done;)
			done;;*)





			(*for i = 9 to 9 do
				(for j = 0 to 9 do let st_1,st_2,st_3,st_4,st_5 = historique_game_states.((10 * i) + j) in
					
				let stat_1,stat_2,stat_3,stat_4,stat_5 = partie_full_mcts  (fst (competition_mcts.(i - 5))) (snd (competition_mcts.(i - 5))) (game_states_tables.((10 * i) + j),game_states_meeples.((10 * i) + j)) st_1 st_2 decks_game_state.((10 * i) + j) J2 st_3  st_4 st_5 25 25


				in results_game_states.( (10 * i) + j) <- (stat_1,stat_2,stat_3,stat_4,stat_5);
				lst_game_states_tables_final.((10 * i) + j) <- (let acc = ref [] in Array.iteri(fun x_0 -> fun arr_elem -> Array.iteri (fun y_0 -> fun tuile -> match tuile with |T(tuile) -> let num_tuile = int_of_tiles tuile in acc := (int_of_tiles tuile,(x_0,y_0),(match (Array.find_opt (fun i_0 -> rotation_tuile i_0 tuile = tiles_of_int num_tuile ) [|0;1;2;3|]) with |Some(rot) -> rot |_ -> -1   ) )::(!acc)   |_ -> ()) arr_elem ) game_states_tables.((10 * i) + j);!acc);
			print_string "Next Game :            ";print_int ( (10 * i) + j); print_endline "";done;)
		done;;*)




							

			(*let deck_rand = shuffle_lst_without_fail decks_game_state.(0) table in let st_1,st_2,st_3,st_4,_ = historique_game_states.(0) in ( (List.map (fun a_0 -> int_of_tiles a_0) deck_rand) ,next_move_star_2_p_5 neg_infinity infinity 5 (*player_turn(*1 si c'est le premier joueur,-1 sinon*)*) 0 (game_states_tables.(0),game_states_meeples.(0)) deck_rand J2 st_1 st_2 st_3 st_4);;*)


			(*historique_game_states.(0);;(List.map (fun a -> int_of_tiles a) decks_game_state.(0));; results_game_states.(0);;
		 	;;game_states_meeples.(0);;lst_game_states_tables_init.(0);;lst_game_states_tables_final.(0);; print_endline ""; print_int 0; print_string "NNNNExtt:::::" ;print_endline "";

			 historique_game_states.(1);;(List.map (fun a -> int_of_tiles a) decks_game_state.(1));; results_game_states.(1);;
			;;game_states_meeples.(1);;lst_game_states_tables_init.(1);;lst_game_states_tables_final.(1);; print_endline ""; print_int 1; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(2);;(List.map (fun a -> int_of_tiles a) decks_game_state.(2));; results_game_states.(2);;
		 	;;game_states_meeples.(2);;lst_game_states_tables_init.(2);;lst_game_states_tables_final.(2);; print_endline ""; print_int 2; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(3);;(List.map (fun a -> int_of_tiles a) decks_game_state.(3));; results_game_states.(3);;
			;;game_states_meeples.(3);;lst_game_states_tables_init.(3);;lst_game_states_tables_final.(3);; print_endline ""; print_int 3; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(4);;(List.map (fun a -> int_of_tiles a) decks_game_state.(4));; results_game_states.(4);;
		 	;;game_states_meeples.(4);;lst_game_states_tables_init.(4);;lst_game_states_tables_final.(4);; print_endline ""; print_int 4; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(5);;(List.map (fun a -> int_of_tiles a) decks_game_state.(5));; results_game_states.(5);;
			;;game_states_meeples.(5);;lst_game_states_tables_init.(5);;lst_game_states_tables_final.(5);; print_endline ""; print_int 5; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(6);;(List.map (fun a -> int_of_tiles a) decks_game_state.(6));; results_game_states.(6);;
		 	;;game_states_meeples.(6);;lst_game_states_tables_init.(6);;lst_game_states_tables_final.(6);; print_endline ""; print_int 6; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(7);;(List.map (fun a -> int_of_tiles a) decks_game_state.(7));; results_game_states.(7);;
			;;game_states_meeples.(7);;lst_game_states_tables_init.(7);;lst_game_states_tables_final.(7);; print_endline ""; print_int 7; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(8);;(List.map (fun a -> int_of_tiles a) decks_game_state.(8));; results_game_states.(8);;
		 	;;game_states_meeples.(8);;lst_game_states_tables_init.(8);;lst_game_states_tables_final.(8);; print_endline ""; print_int 8; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(9);;(List.map (fun a -> int_of_tiles a) decks_game_state.(9));; results_game_states.(9);;
			;;game_states_meeples.(9);;lst_game_states_tables_init.(9);;lst_game_states_tables_final.(9);; print_endline ""; print_int 9; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(10);;(List.map (fun a -> int_of_tiles a) decks_game_state.(10));; results_game_states.(10);;
		 	;;game_states_meeples.(10);;lst_game_states_tables_init.(10);;lst_game_states_tables_final.(10);; print_endline ""; print_int 10; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(11);;(List.map (fun a -> int_of_tiles a) decks_game_state.(11));; results_game_states.(11);;
			;;game_states_meeples.(11);;lst_game_states_tables_init.(11);;lst_game_states_tables_final.(11);; print_endline ""; print_int 11; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(12);;(List.map (fun a -> int_of_tiles a) decks_game_state.(12));; results_game_states.(12);;
		 	;;game_states_meeples.(12);;lst_game_states_tables_init.(12);;lst_game_states_tables_final.(12);; print_endline ""; print_int 12; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(13);;(List.map (fun a -> int_of_tiles a) decks_game_state.(13));; results_game_states.(13);;
			;;game_states_meeples.(13);;lst_game_states_tables_init.(13);;lst_game_states_tables_final.(13);; print_endline ""; print_int 13; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(14);;(List.map (fun a -> int_of_tiles a) decks_game_state.(14));; results_game_states.(14);;
		 	;;game_states_meeples.(14);;lst_game_states_tables_init.(14);;lst_game_states_tables_final.(14);; print_endline ""; print_int 14; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(15);;(List.map (fun a -> int_of_tiles a) decks_game_state.(15));; results_game_states.(15);;
			;;game_states_meeples.(15);;lst_game_states_tables_init.(15);;lst_game_states_tables_final.(15);; print_endline ""; print_int 15; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(16);;(List.map (fun a -> int_of_tiles a) decks_game_state.(16));; results_game_states.(16);;
		 	;;game_states_meeples.(16);;lst_game_states_tables_init.(16);;lst_game_states_tables_final.(16);; print_endline ""; print_int 16; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(17);;(List.map (fun a -> int_of_tiles a) decks_game_state.(17));; results_game_states.(17);;
			;;game_states_meeples.(17);;lst_game_states_tables_init.(17);;lst_game_states_tables_final.(17);; print_endline ""; print_int 17; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(18);;(List.map (fun a -> int_of_tiles a) decks_game_state.(18));; results_game_states.(18);;
		 	;;game_states_meeples.(18);;lst_game_states_tables_init.(18);;lst_game_states_tables_final.(18);; print_endline ""; print_int 18; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(19);;(List.map (fun a -> int_of_tiles a) decks_game_state.(19));; results_game_states.(19);;
			;;game_states_meeples.(19);;lst_game_states_tables_init.(19);;lst_game_states_tables_final.(19);; print_endline ""; print_int 19; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(20);;(List.map (fun a -> int_of_tiles a) decks_game_state.(20));; results_game_states.(20);;
		 	;;game_states_meeples.(20);;lst_game_states_tables_init.(20);;lst_game_states_tables_final.(20);; print_endline ""; print_int 20; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(21);;(List.map (fun a -> int_of_tiles a) decks_game_state.(21));; results_game_states.(21);;
			;;game_states_meeples.(21);;lst_game_states_tables_init.(21);;lst_game_states_tables_final.(21);; print_endline ""; print_int 21; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(22);;(List.map (fun a -> int_of_tiles a) decks_game_state.(22));; results_game_states.(22);;
		 	;;game_states_meeples.(22);;lst_game_states_tables_init.(22);;lst_game_states_tables_final.(22);; print_endline ""; print_int 22; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(23);;(List.map (fun a -> int_of_tiles a) decks_game_state.(23));; results_game_states.(23);;
			;;game_states_meeples.(23);;lst_game_states_tables_init.(23);;lst_game_states_tables_final.(23);; print_endline ""; print_int 23; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(24);;(List.map (fun a -> int_of_tiles a) decks_game_state.(24));; results_game_states.(24);;
		 	;;game_states_meeples.(24);;lst_game_states_tables_init.(24);;lst_game_states_tables_final.(24);; print_endline ""; print_int 24; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(25);;(List.map (fun a -> int_of_tiles a) decks_game_state.(25));; results_game_states.(25);;
			;;game_states_meeples.(25);;lst_game_states_tables_init.(25);;lst_game_states_tables_final.(25);; print_endline ""; print_int 25; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(26);;(List.map (fun a -> int_of_tiles a) decks_game_state.(26));; results_game_states.(26);;
		 	;;game_states_meeples.(26);;lst_game_states_tables_init.(26);;lst_game_states_tables_final.(26);; print_endline ""; print_int 26; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(27);;(List.map (fun a -> int_of_tiles a) decks_game_state.(27));; results_game_states.(27);;
			;;game_states_meeples.(27);;lst_game_states_tables_init.(27);;lst_game_states_tables_final.(27);; print_endline ""; print_int 27; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(28);;(List.map (fun a -> int_of_tiles a) decks_game_state.(28));; results_game_states.(28);;
		 	;;game_states_meeples.(28);;lst_game_states_tables_init.(28);;lst_game_states_tables_final.(28);; print_endline ""; print_int 28; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(29);;(List.map (fun a -> int_of_tiles a) decks_game_state.(29));; results_game_states.(29);;
			;;game_states_meeples.(29);;lst_game_states_tables_init.(29);;lst_game_states_tables_final.(29);; print_endline ""; print_int 29; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(30);;(List.map (fun a -> int_of_tiles a) decks_game_state.(30));; results_game_states.(30);;
		 	;;game_states_meeples.(30);;lst_game_states_tables_init.(30);;lst_game_states_tables_final.(30);; print_endline ""; print_int 30; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(31);;(List.map (fun a -> int_of_tiles a) decks_game_state.(31));; results_game_states.(31);;
			;;game_states_meeples.(31);;lst_game_states_tables_init.(31);;lst_game_states_tables_final.(31);; print_endline ""; print_int 31; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(32);;(List.map (fun a -> int_of_tiles a) decks_game_state.(32));; results_game_states.(32);;
		 	;;game_states_meeples.(32);;lst_game_states_tables_init.(32);;lst_game_states_tables_final.(32);; print_endline ""; print_int 32; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(33);;(List.map (fun a -> int_of_tiles a) decks_game_state.(33));; results_game_states.(33);;
			;;game_states_meeples.(33);;lst_game_states_tables_init.(33);;lst_game_states_tables_final.(33);; print_endline ""; print_int 33; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(34);;(List.map (fun a -> int_of_tiles a) decks_game_state.(34));; results_game_states.(34);;
		 	;;game_states_meeples.(34);;lst_game_states_tables_init.(34);;lst_game_states_tables_final.(34);; print_endline ""; print_int 34; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(35);;(List.map (fun a -> int_of_tiles a) decks_game_state.(35));; results_game_states.(35);;
			;;game_states_meeples.(35);;lst_game_states_tables_init.(35);;lst_game_states_tables_final.(35);; print_endline ""; print_int 35; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(36);;(List.map (fun a -> int_of_tiles a) decks_game_state.(36));; results_game_states.(36);;
		 	;;game_states_meeples.(36);;lst_game_states_tables_init.(36);;lst_game_states_tables_final.(36);; print_endline ""; print_int 36; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(37);;(List.map (fun a -> int_of_tiles a) decks_game_state.(37));; results_game_states.(37);;
			;;game_states_meeples.(37);;lst_game_states_tables_init.(37);;lst_game_states_tables_final.(37);; print_endline ""; print_int 37; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(38);;(List.map (fun a -> int_of_tiles a) decks_game_state.(38));; results_game_states.(38);;
		 	;;game_states_meeples.(38);;lst_game_states_tables_init.(38);;lst_game_states_tables_final.(38);; print_endline ""; print_int 38; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(39);;(List.map (fun a -> int_of_tiles a) decks_game_state.(39));; results_game_states.(39);;
			;;game_states_meeples.(39);;lst_game_states_tables_init.(39);;lst_game_states_tables_final.(39);; print_endline ""; print_int 39; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(40);;(List.map (fun a -> int_of_tiles a) decks_game_state.(40));; results_game_states.(40);;
		 	;;game_states_meeples.(40);;lst_game_states_tables_init.(40);;lst_game_states_tables_final.(40);; print_endline ""; print_int 40; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(41);;(List.map (fun a -> int_of_tiles a) decks_game_state.(41));; results_game_states.(41);;
			;;game_states_meeples.(41);;lst_game_states_tables_init.(41);;lst_game_states_tables_final.(41);; print_endline ""; print_int 41; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(42);;(List.map (fun a -> int_of_tiles a) decks_game_state.(42));; results_game_states.(42);;
		 	;;game_states_meeples.(42);;lst_game_states_tables_init.(42);;lst_game_states_tables_final.(42);; print_endline ""; print_int 42; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(43);;(List.map (fun a -> int_of_tiles a) decks_game_state.(43));; results_game_states.(43);;
			;;game_states_meeples.(43);;lst_game_states_tables_init.(43);;lst_game_states_tables_final.(43);; print_endline ""; print_int 43; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(44);;(List.map (fun a -> int_of_tiles a) decks_game_state.(44));; results_game_states.(44);;
		 	;;game_states_meeples.(44);;lst_game_states_tables_init.(44);;lst_game_states_tables_final.(44);; print_endline ""; print_int 44; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(45);;(List.map (fun a -> int_of_tiles a) decks_game_state.(45));; results_game_states.(45);;
			;;game_states_meeples.(45);;lst_game_states_tables_init.(45);;lst_game_states_tables_final.(45);; print_endline ""; print_int 45; print_string "NNNNExtt:::::" ; print_endline "";

			historique_game_states.(46);;(List.map (fun a -> int_of_tiles a) decks_game_state.(46));; results_game_states.(46);;
		 	;;game_states_meeples.(46);;lst_game_states_tables_init.(46);;lst_game_states_tables_final.(46);; print_endline ""; print_int 46; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(47);;(List.map (fun a -> int_of_tiles a) decks_game_state.(47));; results_game_states.(47);;
			;;game_states_meeples.(47);;lst_game_states_tables_init.(47);;lst_game_states_tables_final.(47);; print_endline ""; print_int 47; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(48);;(List.map (fun a -> int_of_tiles a) decks_game_state.(48));; results_game_states.(48);;
		 	;;game_states_meeples.(48);;lst_game_states_tables_init.(48);;lst_game_states_tables_final.(48);; print_endline ""; print_int 48; print_string "NNNNExtt:::::" ; print_endline "";

			 historique_game_states.(49);;(List.map (fun a -> int_of_tiles a) decks_game_state.(49));; results_game_states.(49);;
			;;game_states_meeples.(49);;lst_game_states_tables_init.(49);;lst_game_states_tables_final.(49);; print_endline ""; print_int 49; print_string "NNNNExtt:::::" ; print_endline "";
			
			historique_game_states.(50);;(List.map (fun a -> int_of_tiles a) decks_game_state.(50));; results_game_states.(50);;
;;game_states_meeples.(50);;lst_game_states_tables_init.(50);;lst_game_states_tables_final.(50);; print_endline ""; print_int 50; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(51);;(List.map (fun a -> int_of_tiles a) decks_game_state.(51));; results_game_states.(51);;
;;game_states_meeples.(51);;lst_game_states_tables_init.(51);;lst_game_states_tables_final.(51);; print_endline ""; print_int 51; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(52);;(List.map (fun a -> int_of_tiles a) decks_game_state.(52));; results_game_states.(52);;
;;game_states_meeples.(52);;lst_game_states_tables_init.(52);;lst_game_states_tables_final.(52);; print_endline ""; print_int 52; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(53);;(List.map (fun a -> int_of_tiles a) decks_game_state.(53));; results_game_states.(53);;
;;game_states_meeples.(53);;lst_game_states_tables_init.(53);;lst_game_states_tables_final.(53);; print_endline ""; print_int 53; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(54);;(List.map (fun a -> int_of_tiles a) decks_game_state.(54));; results_game_states.(54);;
;;game_states_meeples.(54);;lst_game_states_tables_init.(54);;lst_game_states_tables_final.(54);; print_endline ""; print_int 54; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(55);;(List.map (fun a -> int_of_tiles a) decks_game_state.(55));; results_game_states.(55);;
;;game_states_meeples.(55);;lst_game_states_tables_init.(55);;lst_game_states_tables_final.(55);; print_endline ""; print_int 55; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(56);;(List.map (fun a -> int_of_tiles a) decks_game_state.(56));; results_game_states.(56);;
;;game_states_meeples.(56);;lst_game_states_tables_init.(56);;lst_game_states_tables_final.(56);; print_endline ""; print_int 56; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(57);;(List.map (fun a -> int_of_tiles a) decks_game_state.(57));; results_game_states.(57);;
;;game_states_meeples.(57);;lst_game_states_tables_init.(57);;lst_game_states_tables_final.(57);; print_endline ""; print_int 57; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(58);;(List.map (fun a -> int_of_tiles a) decks_game_state.(58));; results_game_states.(58);;
;;game_states_meeples.(58);;lst_game_states_tables_init.(58);;lst_game_states_tables_final.(58);; print_endline ""; print_int 58; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(59);;(List.map (fun a -> int_of_tiles a) decks_game_state.(59));; results_game_states.(59);;
;;game_states_meeples.(59);;lst_game_states_tables_init.(59);;lst_game_states_tables_final.(59);; print_endline ""; print_int 59; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(60);;(List.map (fun a -> int_of_tiles a) decks_game_state.(60));; results_game_states.(60);;
;;game_states_meeples.(60);;lst_game_states_tables_init.(60);;lst_game_states_tables_final.(60);; print_endline ""; print_int 60; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(61);;(List.map (fun a -> int_of_tiles a) decks_game_state.(61));; results_game_states.(61);;
;;game_states_meeples.(61);;lst_game_states_tables_init.(61);;lst_game_states_tables_final.(61);; print_endline ""; print_int 61; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(62);;(List.map (fun a -> int_of_tiles a) decks_game_state.(62));; results_game_states.(62);;
;;game_states_meeples.(62);;lst_game_states_tables_init.(62);;lst_game_states_tables_final.(62);; print_endline ""; print_int 62; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(63);;(List.map (fun a -> int_of_tiles a) decks_game_state.(63));; results_game_states.(63);;
;;game_states_meeples.(63);;lst_game_states_tables_init.(63);;lst_game_states_tables_final.(63);; print_endline ""; print_int 63; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(64);;(List.map (fun a -> int_of_tiles a) decks_game_state.(64));; results_game_states.(64);;
;;game_states_meeples.(64);;lst_game_states_tables_init.(64);;lst_game_states_tables_final.(64);; print_endline ""; print_int 64; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(65);;(List.map (fun a -> int_of_tiles a) decks_game_state.(65));; results_game_states.(65);;
;;game_states_meeples.(65);;lst_game_states_tables_init.(65);;lst_game_states_tables_final.(65);; print_endline ""; print_int 65; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(66);;(List.map (fun a -> int_of_tiles a) decks_game_state.(66));; results_game_states.(66);;
;;game_states_meeples.(66);;lst_game_states_tables_init.(66);;lst_game_states_tables_final.(66);; print_endline ""; print_int 66; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(67);;(List.map (fun a -> int_of_tiles a) decks_game_state.(67));; results_game_states.(67);;
;;game_states_meeples.(67);;lst_game_states_tables_init.(67);;lst_game_states_tables_final.(67);; print_endline ""; print_int 67; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(68);;(List.map (fun a -> int_of_tiles a) decks_game_state.(68));; results_game_states.(68);;
;;game_states_meeples.(68);;lst_game_states_tables_init.(68);;lst_game_states_tables_final.(68);; print_endline ""; print_int 68; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(69);;(List.map (fun a -> int_of_tiles a) decks_game_state.(69));; results_game_states.(69);;
;;game_states_meeples.(69);;lst_game_states_tables_init.(69);;lst_game_states_tables_final.(69);; print_endline ""; print_int 69; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(70);;(List.map (fun a -> int_of_tiles a) decks_game_state.(70));; results_game_states.(70);;
;;game_states_meeples.(70);;lst_game_states_tables_init.(70);;lst_game_states_tables_final.(70);; print_endline ""; print_int 70; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(71);;(List.map (fun a -> int_of_tiles a) decks_game_state.(71));; results_game_states.(71);;
;;game_states_meeples.(71);;lst_game_states_tables_init.(71);;lst_game_states_tables_final.(71);; print_endline ""; print_int 71; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(72);;(List.map (fun a -> int_of_tiles a) decks_game_state.(72));; results_game_states.(72);;
;;game_states_meeples.(72);;lst_game_states_tables_init.(72);;lst_game_states_tables_final.(72);; print_endline ""; print_int 72; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(73);;(List.map (fun a -> int_of_tiles a) decks_game_state.(73));; results_game_states.(73);;
;;game_states_meeples.(73);;lst_game_states_tables_init.(73);;lst_game_states_tables_final.(73);; print_endline ""; print_int 73; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(74);;(List.map (fun a -> int_of_tiles a) decks_game_state.(74));; results_game_states.(74);;
;;game_states_meeples.(74);;lst_game_states_tables_init.(74);;lst_game_states_tables_final.(74);; print_endline ""; print_int 74; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(75);;(List.map (fun a -> int_of_tiles a) decks_game_state.(75));; results_game_states.(75);;
;;game_states_meeples.(75);;lst_game_states_tables_init.(75);;lst_game_states_tables_final.(75);; print_endline ""; print_int 75; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(76);;(List.map (fun a -> int_of_tiles a) decks_game_state.(76));; results_game_states.(76);;
;;game_states_meeples.(76);;lst_game_states_tables_init.(76);;lst_game_states_tables_final.(76);; print_endline "" ; print_int 76; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(77);;(List.map (fun a -> int_of_tiles a) decks_game_state.(77));; results_game_states.(77);;
;;game_states_meeples.(77);;lst_game_states_tables_init.(77);;lst_game_states_tables_final.(77);; print_endline ""; print_int 77; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(78);;(List.map (fun a -> int_of_tiles a) decks_game_state.(78));; results_game_states.(78);;
;;game_states_meeples.(78);;lst_game_states_tables_init.(78);;lst_game_states_tables_final.(78);; print_endline ""; print_int 78; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(79);;(List.map (fun a -> int_of_tiles a) decks_game_state.(79));; results_game_states.(79);;
;;game_states_meeples.(79);;lst_game_states_tables_init.(79);;lst_game_states_tables_final.(79);; print_endline ""; print_int 79; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(80);;(List.map (fun a -> int_of_tiles a) decks_game_state.(80));; results_game_states.(80);;
;;game_states_meeples.(80);;lst_game_states_tables_init.(80);;lst_game_states_tables_final.(80);; print_endline ""; print_int 80; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(81);;(List.map (fun a -> int_of_tiles a) decks_game_state.(81));; results_game_states.(81);;
;;game_states_meeples.(81);;lst_game_states_tables_init.(81);;lst_game_states_tables_final.(81);; print_endline ""; print_int 81; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(82);;(List.map (fun a -> int_of_tiles a) decks_game_state.(82));; results_game_states.(82);;
;;game_states_meeples.(82);;lst_game_states_tables_init.(82);;lst_game_states_tables_final.(82);; print_endline ""; print_int 82; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(83);;(List.map (fun a -> int_of_tiles a) decks_game_state.(83));; results_game_states.(83);;
;;game_states_meeples.(83);;lst_game_states_tables_init.(83);;lst_game_states_tables_final.(83);; print_endline ""; print_int 83; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(84);;(List.map (fun a -> int_of_tiles a) decks_game_state.(84));; results_game_states.(84);;
;;game_states_meeples.(84);;lst_game_states_tables_init.(84);;lst_game_states_tables_final.(84);; print_endline ""; print_int 84; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(85);;(List.map (fun a -> int_of_tiles a) decks_game_state.(85));; results_game_states.(85);;
;;game_states_meeples.(85);;lst_game_states_tables_init.(85);;lst_game_states_tables_final.(85);; print_endline ""; print_int 85; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(86);;(List.map (fun a -> int_of_tiles a) decks_game_state.(86));; results_game_states.(86);;
;;game_states_meeples.(86);;lst_game_states_tables_init.(86);;lst_game_states_tables_final.(86);; print_endline ""; print_int 86; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(87);;(List.map (fun a -> int_of_tiles a) decks_game_state.(87));; results_game_states.(87);;
;;game_states_meeples.(87);;lst_game_states_tables_init.(87);;lst_game_states_tables_final.(87);; print_endline ""; print_int 87; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(88);;(List.map (fun a -> int_of_tiles a) decks_game_state.(88));; results_game_states.(88);;
;;game_states_meeples.(88);;lst_game_states_tables_init.(88);;lst_game_states_tables_final.(88);; print_endline ""; print_int 88; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(89);;(List.map (fun a -> int_of_tiles a) decks_game_state.(89));; results_game_states.(89);;
;;game_states_meeples.(89);;lst_game_states_tables_init.(89);;lst_game_states_tables_final.(89);; print_endline ""; print_int 89; print_string "NNNNExtt:::::" ; print_endline "";*)

historique_game_states.(90);;(List.map (fun a -> int_of_tiles a) decks_game_state.(90));; results_game_states.(90);;
;;game_states_meeples.(90);;lst_game_states_tables_init.(90);;lst_game_states_tables_final.(90);; print_endline ""; print_int 90; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(91);;(List.map (fun a -> int_of_tiles a) decks_game_state.(91));; results_game_states.(91);;
;;game_states_meeples.(91);;lst_game_states_tables_init.(91);;lst_game_states_tables_final.(91);; print_endline ""; print_int 91; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(92);;(List.map (fun a -> int_of_tiles a) decks_game_state.(92));; results_game_states.(92);;
;;game_states_meeples.(92);;lst_game_states_tables_init.(92);;lst_game_states_tables_final.(92);; print_endline ""; print_int 92; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(93);;(List.map (fun a -> int_of_tiles a) decks_game_state.(93));; results_game_states.(93);;
;;game_states_meeples.(93);;lst_game_states_tables_init.(93);;lst_game_states_tables_final.(93);; print_endline ""; print_int 93; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(94);;(List.map (fun a -> int_of_tiles a) decks_game_state.(94));; results_game_states.(94);;
;;game_states_meeples.(94);;lst_game_states_tables_init.(94);;lst_game_states_tables_final.(94);; print_endline ""; print_int 94; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(95);;(List.map (fun a -> int_of_tiles a) decks_game_state.(95));; results_game_states.(95);;
;;game_states_meeples.(95);;lst_game_states_tables_init.(95);;lst_game_states_tables_final.(95);; print_endline ""; print_int 95; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(96);;(List.map (fun a -> int_of_tiles a) decks_game_state.(96));; results_game_states.(96);;
;;game_states_meeples.(96);;lst_game_states_tables_init.(96);;lst_game_states_tables_final.(96);; print_endline ""; print_int 96; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(97);;(List.map (fun a -> int_of_tiles a) decks_game_state.(97));; results_game_states.(97);;
;;game_states_meeples.(97);;lst_game_states_tables_init.(97);;lst_game_states_tables_final.(97);; print_endline ""; print_int 97; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(98);;(List.map (fun a -> int_of_tiles a) decks_game_state.(98));; results_game_states.(98);;
;;game_states_meeples.(98);;lst_game_states_tables_init.(98);;lst_game_states_tables_final.(98);; print_endline ""; print_int 98; print_string "NNNNExtt:::::" ; print_endline "";

historique_game_states.(99);;(List.map (fun a -> int_of_tiles a) decks_game_state.(99));; results_game_states.(99);;
;;game_states_meeples.(99);;lst_game_states_tables_init.(99);;lst_game_states_tables_final.(99);;
			
			


			(*print_string "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
			AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
			AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
			AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
			AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
			AAAAa";let deck_rand = shuffle_lst_without_fail deck_standard table in 
			(base_jeu_mcts (table,meeples) 8 8 deck_rand J1 2. 0 0 100,List.hd deck_rand);;*)