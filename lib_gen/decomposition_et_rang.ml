open Nb_gen
open Loi_star
open Loi_gp_ab



let rg g =
    let n = g.order in 
	let res = ref 0 in
    let q = Queue.create() in
    Queue.add [] q;
    while not (Queue.is_empty q)  do
    	let h = Queue.pop q in
        	if est_gen g h then
            (
               	res := List.length h;
                Queue.clear q
            )
             else List.iter (fun x -> Queue.add x q) (next_subtree h n)
	done;
    	!res;;

let decomp_inv n =
	let p = euler n in
    let li = decomp_prod p in
    let res = ref [||] in
    let r = rg (star_group n) in
    let rec aux l = match l with
    |[] -> ()
    |t::d -> if r = rg (abelian_group t) then
    			res := t
             else aux d
    in
    	aux li;
    !res;;

let abelian_rg arr = 
    let n = ref 1 and len = Array.length arr in
    for i = 0 to (len-1) do
        n := (!n)*(arr.(i))
    done;
    rg (abelian_group arr);;

let sym_rg n = rg (symmetric_group n);;

let died_rg n = rg (diedral_group n);;

let star_rg n = rg (star_group n);;

