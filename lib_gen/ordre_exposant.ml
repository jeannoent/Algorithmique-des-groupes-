open Nb_gen
open Loi_gp_ab
open Loi_diedral
open Loi_star
open Permutation
let order g x =
	let res = ref 1 and temp = ref x in
    let f = g.loi in
    while !temp != 0 do
    	res := !res + 1;
        temp := f(!temp,x)
	done;
    !res;;


let exponent g = 
	let n = g.order in
    let res = ref 1 in
    for i = 0 to n-1 do 
    	if (order g i) > !res then 
        	res := order g i;
    done;
    !res;;


let abelian_exponent arr = exponent (abelian_group arr)


let star_exponent n = exponent (star_group n)


let died_exponent n = exponent (diedral_group n)


let sym_exponent n = exponent (symmetric_group n)


let exponent_list g f = (*à un groupe renvoie la liste des élément dont l'ordre est égal à l'exposant*)
	let res = ref [] and e = exponent g in
    let n = g.order in
    for i = 0 to n-1 do
    	if (order g i) = e then
        	res := i::(!res);
	done;
    List.map f !res;;


let exponent_list_star n = exponent_list (star_group n) (intToStar n);;


let exponent_list_ab arr = exponent_list (abelian_group arr) (recip_order arr);;


let exponent_list_died n = exponent_list (diedral_group n) (recip_order_died)


let exponent_list_sym n = exponent_list (symmetric_group n) (intToPerm n)


