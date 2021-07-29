let loi n a b = (a+b) mod n;;

let opp_id n i = (((-i) mod n) + n) mod n;;
let id n i = i mod n;;

let action n i = match i with
|0 -> id n
|1 -> opp_id n
|_ -> failwith"nsm";;

let prod_semi_dir n (h1,k1) (h2,k2) = 
	(loi n h1 (action n k1 h2), loi 2 k1 k2);;

let prod_inv n (h,k) = (action n k (opp_id n h),k);;


let lexical_order n (i,j) = 2*i + j;;
let recip_order n k = (k/2,k mod 2);;

let diedral_op n (i,j) = lexical_order n (prod_semi_dir n (recip_order n i) (recip_order n j) );;

let semi_direct_epsilon n (i,j) = lexical_order n (prod_semi_dir n (recip_order n j) (prod_inv n (recip_order n i)) );;
