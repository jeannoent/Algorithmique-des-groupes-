open Nb_gen
open Loi_star
open Loi_gp_ab

let add_inv_queue q i = Queue.add i q;;

let rg n e = 
	let res = ref 0 in
    let stop = ref true in
    let q = Queue.create() in
    	for i = 0 to n-1 do
        	Queue.add [i] q
        done;
        while (not (Queue.is_empty q)) && !stop do
        	let h = Queue.pop q in
            	if est_gen(n,h,e) then
                (
                	res := List.length h;
                    stop := false;
                 )
                 else List.iter (add_inv_queue q) (next_subtree h n)
		done;
        	!res;;

let decomp_inv n =
	let p = euler n in
    let li = decomp_prod p in
    let res = ref [||] in
    let rec aux l = match l with
    |[] -> ()
    |t::d -> if rg p (epsilon_star n) = rg p (abelian_epsilon t) then
    			res := t
             else aux d
    in
    	aux li;
    !res;;

let abelian_rg arr = 
    let n = ref 0 and len = Array.length arr in
    for i = 0 to len-1 do
        n := (!n)*arr.(i)
    done;
    in
    rg (!n) (abelian_epsilon arr);;

let sym_rg n = rg (fac n) (sym_epsilon n);;

let died_rg n = rg (2*n) (died_epsilon n);;

let star_rg n = rg (euler n) (epsilon_star n);;

