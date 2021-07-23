type ggraphe = int * int list * ((int*int) -> int);;

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
    
(*k pamri n*)
let comb k n =
	let res = ref 1 in
    for i = 0  to k-1 do
    	res:= !res * (n-i)
    done;
    let div = ref 1 in
    for i =2 to k do
    	div := i* !div;
    done;
    !res/ !div;;

(*génère une array de taille n+1 
contenant les sommes de i à n de i parmi n pour i de 0 à n+1*)
let gen_combs n=
	let res = Array.make 0 (n+1) in
    res.(n) <- 1;
    for i=0 to n-1 do
    	res.(i) <- res.(i+1) + comb i n
    done;
    res;;
    
let gen_card_sub_trees n =
	let res = Array.make n 0 in
    res.(n-1)<- 1;
    for i=2 to n do
   		res.(n-i) <- res.(n-i+1)+i
    done;
    res;;
    
let print_list l =
	print_string "[";
    let rec aux  = function
    	|[] -> print_endline "]"
        |[x] -> print_int x; print_endline "]"
        |x::l -> print_int x;print_string ";"; aux l 
     in aux l;;   
     
    

let nb_gen n e  =
	(*let nb_c = gen_card_sub_trees n in*)
	let res= ref 0 in
    let rec aux l=
    	if est_gen (n,l,e) then
        	res:= (!res mod n) + ((1 lsl (n-1-List.hd l)) mod n) mod n 
        else List.iter aux (next_subtree l n)
    in aux [];
    !res mod n;;
    
let znz n (i,j) = 
    	if i <= j then (j - i) mod n
              else (n + (j-i) mod n) mod n;;
              
nb_gen 26 (znz 26);;            


        
        
        






