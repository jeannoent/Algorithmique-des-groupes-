
type ggraphe = int * int list * ((int*int) -> int);;


let print_list l =
	print_string "[";
    let rec aux  = function
    	|[] -> print_endline "]"
        |[x] -> print_int x; print_endline "]"
        |x::l -> print_int x;print_string ";"; aux l 
     in aux l;;   
     


(*construit un table de hash contenant 
les tuples (i,()) pour i de 0 à n-1*)
let constr_hash n =
	let res = Hashtbl.create n in
    for i=0 to n-1 do
    	Hashtbl.add res i ()
    done;
    res;;

(*vérifie si s génère le groupe de loi e à n éléments*)
let est_gen ((g,s,e):ggraphe) =
    let q = Queue.create () and n = g in
    let t = constr_hash n in
    List.iter (fun i -> 
              Queue.add i q;
              Hashtbl.remove t i)
              s;
    while not (Queue.is_empty q) do
    	let a = Queue.take q in
        for i = 0 to n-1 do 
        	if Hashtbl.mem t i && not (Hashtbl.mem t (e(a,i))) then 
            	(
                 Hashtbl.remove t i;
                 Queue.add i q; 
                )
    	done;
	done;
    Hashtbl.length t = 0;;
    
    
type smol_tree = (int list)* int list list;;

let next_subtree l max =
	let min = try List.hd l with _ -> -1 in
    let res = ref [] in
    for i=min+1 to max-1 do
    	res:= (i::l) :: !res
    done;
    !res;;
    
(*retourne le nombre de parties génératrices d'un goupe d'ordre n de loi e*)
let nb_gen n e  =
	let res= ref 0 in
    let rec aux l=
    	if est_gen (n,l,e) then
        	res:= !res + (1 lsl (n-1-List.hd l))
        else List.iter aux (next_subtree l n)
    in aux [];
    !res;;
    

(*returne a^n mod m*)    
let rec pow_mod a n m = match n with
  | 0 -> 1
  | 1 -> a
  | _ -> 
    let b = pow_mod a (n / 2) m mod m in
    (b * b mod m)* (if n mod 2 = 0 then 1 else a mod m ) mod m;;
    

(*retourne nb_gen mod n, calcul plus long mais 
permet d'éviter (en général) les overflows dans les cas où 
n>31 où n>63 selon l'architecture, où la valeur de nb_gen ne serait pas correcte*)
let nb_gen_mod n e = 
    let res= ref 0 in
    let rec aux l=
    	if est_gen (n,l,e) then
        	res:= (!res mod n) + (pow_mod 2 (n-1-List.hd l) n) mod n 
        else List.iter aux (next_subtree l n)
    in aux [];
    !res;;


let znz n (i,j) = 
    	if i <= j then (j - i) mod n
              else (n + (j-i) mod n) mod n;;
              

let%test "bijection" = nb_gen 12 (znz 12) = nb_gen 12 (abelian_epsilon [|3;4|]) 

let abelian_gen arr = 
    let n = ref 1 and m = Array.length arr in
    for i = 0 to m-1 do
          n := arr.(i)*(!n)
    done;
    nb_gen !n (abelian_epsilon arr);;

 let all_abelian n = 
    let prod = (decomp_prod n) in
    let res = ref [] in
    let rec aux l = match l with
    |[] -> ()
    |arr::d -> res := ((abelian_gen arr))::(!res);
               aux d;
    in
    aux prod
    !res;;

        
   