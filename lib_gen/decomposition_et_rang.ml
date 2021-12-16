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

let abelian_rg arr = rg (abelian_group arr);;

let sym_rg n = rg (symmetric_group n);;

let died_rg n = rg (diedral_group n);;

let star_rg n = rg (star_group n);;

let part_gen g f =
    let n = g.order in 
	let res = ref [] in
    let q = Queue.create() in
    Queue.add [] q;
    while not (Queue.is_empty q)  do
    	let h = Queue.pop q in
        	if est_gen g h then
            (
               	res := h;
                Queue.clear q
            )
             else List.iter (fun x -> Queue.add x q) (next_subtree h n)
	done;
    	List.map f !res;;


(*let part_gen_ab arr = 
    let n = ref 1 and l = Array.length arr in
    for i = 0 to l-1 do 
        n := (!n)*arr.(i);
    done;
    let e = abelian_epsilon arr and f = recip_order arr in
        part_gen (!n,e) f
        
let part_gen_sym n = part_gen ((fac n), sym_epsilon n) (intToPerm n)


let part_gen_died n = part_gen (2*n,died_epsilon n) (recip_order)


let part_gen_star g = part_gen (euler n, epsilon_star n) (intToStar n)*)
