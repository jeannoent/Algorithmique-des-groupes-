open Loi_diedral
open Loi_gp_ab
open Permutation
open Big_int
open Loi_star

type group = {order : int;law : (int * int) -> int}

let print_list_int l =
	print_string "[";
    let rec aux  = function
    	|[] -> print_endline "]"
        |[x] -> print_int x; print_string "]"
        |x::l -> print_int x;print_string ";"; aux l 
     in aux l

let print_array arr = 
    let n = Array.length arr in
    print_string "[|";
    for i=0 to n-2 do
        print_int arr.(i);print_string ";"
    done;
    print_int arr.(n-1);
    print_string "|]"

let print_list_array l =    
    print_string "[";
    let rec aux  = function
    	|[] -> print_endline "]"
        |[x] -> print_array x; print_endline "]"
        |x::l -> print_array x;print_string ";"; aux l 
     in aux l


(*construit un table de hash contenant 
les tuples (i,()) pour i de 0 à n-1*)
let constr_hash n =
	let res = Hashtbl.create n in
    for i=0 to n-1 do
    	Hashtbl.add res i ()
    done;
    res



(*vérifie si s génère le groupe de loi e à n éléments*)
let est_gen g s=
    let n = g.order and e = g.law in
    let q = Queue.create () in
    let t = Array.make n true in
    let left = ref n in
    List.iter (fun i -> 
              Queue.add i q;
              t.(i) <- false;decr left)
              s;
    while not (Queue.is_empty q) do
    	let a = Queue.take q in
        for i = 0 to n-1 do 
        	if  t.(i) && not t.(e(a,i)) then 
            	(
                 t.(i) <- false;decr left;
                 Queue.add i q; 
                )
    	done;
	done;
    !left = 0    
    
type smol_tree = (int list)* int list list;;

let next_subtree l max =
	let min = try List.hd l with _ -> max in
    let res = ref [] in
    for i=0 to min-1 do
    	res:= ((min-1-i)::l) :: !res
    done;
    !res;;
    
(*retourne le nombre de parties génératrices d'un goupe d'ordre n de loi e*)

let nb_gen ?print:(p=false) g =
    let n = g.order in
	let res= ref zero_big_int in
    let rec aux l=
        if p then print_list_int l;
    	if est_gen g l then
            (
            if p then print_endline "V";
        	res:= add_big_int !res (power_int_positive_int 2 (List.hd l))
            )
        else (
            if p then print_endline "F";
            List.iter aux (next_subtree l n)
            )   
    in aux [];
    let modn = (quomod_big_int !res (big_int_of_int n)) in
    ((string_of_big_int !res,string_of_big_int (fst modn),string_of_big_int (snd modn)),
    "{\"nodes\" : [],\"edges\" : []}")   
    




let est_abelien g = 
	let n = g.order and f = g.law in
    let res = ref true in
    	for i = 0 to n-1 do
        	for j = 0 to n-1 do
            	res := (!res)&&(f(i,j) = f(j,i))
            done;
        done;
	!res;;

let abelian_group arr = 
    {
        order = order arr;
       law = abelian_law arr;
    } 

let symmetric_group n = 
    {
        order = fac n ;
       law = sym_op n;
    } 

let diedral_group n =
    {
        order = 2*n ; 
       law = died_op n;
    }     
    

let star_group n = 
    {
        order = euler n  ;
       law = star_op n;
    }  


let pi_abelian arr = nb_gen (abelian_group arr)    

let pi_symetric n = nb_gen (symmetric_group n)      
  
let pi_diedral n = nb_gen (diedral_group n)

let pi_star n = nb_gen (star_group n)
