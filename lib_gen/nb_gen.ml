open Loi_diedral
open Loi_gp_ab
open Permutation
open Big_int
open Loi_star

type group = {order : int;law : (int * int) -> int}
type nouage = Nil  of bool | N of (nouage array) 


let print_bool = function
    |true -> print_string "true"
    |_ -> print_string "false"

let print_list print l =
	print_string "[";
    let rec aux  = function
    	|[] -> print_endline "]"
        |[x] -> print x; print_string "]"
        |x::l -> print x;print_string ";"; aux l 
     in aux l

let print_array print arr = 
    let n = Array.length arr in
    print_string "[|";
    for i=0 to n-2 do
        print arr.(i);print_string ";"
    done;
    print arr.(n-1);
    print_string "|]"

let print_list_array l =  print_list (print_array print_int) l

let rec print_nouage = function
    |Nil(b) -> print_string "Nil(";print_bool b;print_string ") "
    |N a -> print_string "N";print_array print_nouage a

let add t l =
    let nl = List.rev l in
    let rec aux t l = match t,l with
        |N(arr),h::_::_ -> aux arr.(h) (List.tl l)
        |N(arr),[n]-> arr.(n) <- N (Array.make n (Nil false))
        |_ -> failwith "wtf"
    in aux t nl
(*retourne le nombre de parties génératrices d'un goupe d'ordre n de loi e*)


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
    Hashtbl.length t = 0
    
    
let next_subtree l =
    let min = List.hd l in
    let res = ref [] in
    for i=0 to min-1 do
        res:= ((min-1-i)::l) :: !res
    done;
    !res

let nb_gen ?print:(p=false) g =
    let n = g.order in
    let nouage = N(Array.make n (Nil false)) in
	let res= ref zero_big_int in
    let rec aux nou l=
        let arr = match nou with
            |N(a) -> a
            |_ -> failwith "wut" in
        if p then print_list print_int l;
        let hd = List.hd l in
    	if est_gen g l then
            (
            if p then print_endline "V";
            arr.(hd) <- Nil true;
        	res:= add_big_int !res (power_int_positive_int 2 (List.hd l))
            )
        else (
            if p then print_endline "F";
            arr.(hd) <- N(Array.make hd (Nil false));
            List.iter (aux arr.(hd)) (next_subtree l)
            )   
    in aux nouage [];
    let modn = (quomod_big_int !res (big_int_of_int n)) in
    ((string_of_big_int !res,string_of_big_int (fst modn),string_of_big_int (snd modn)),
    "{\"nodes\" : [],\"edges\" : []}")   
    

let draw_nouage g = 
    let n = g.order in
    let nouage = N(Array.make n (Nil false)) in
	let res= ref zero_big_int in
    let rec aux nou l=
        print_nouage nou;print_newline ();
        print_list print_int l;
        print_newline ();
        let arr = match nou with
            |N(a) -> a
            |_ -> failwith "wut" in
        let hd = List.hd l in
    	if est_gen g l then
            (
            print_endline "V";
            arr.(hd) <- Nil true;
        	res:= add_big_int !res (power_int_positive_int 2 (List.hd l))
            )
        else (
            print_endline "F, ";
            print_int hd;
            arr.(hd) <- if hd = 0 then Nil false else N(Array.make hd (Nil false));
            List.iter (aux arr.(hd)) (next_subtree l)
            )   
    in 
    for i=0 to n-1 do
        aux nouage [i]
    done;
    nouage 
    


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
  
let pi_cyclic n = nb_gen (abelian_group [|n|])

let pi_diedral n = nb_gen (diedral_group n)

let pi_star n = nb_gen (star_group n)
