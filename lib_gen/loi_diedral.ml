let loi n a b = (a+b) mod n;;
let loi_diff n a b = (((b-a) mod n) + n) mod n;;
let id n i = i mod n;;
let opp n i = loi_diff n i 0;;
let semi_dir n f (h1,k1) (h2,k2) = ((loi n h1 (f k1 h2)),loi 2 k1 k2);;

let a n i = match i with
|0 -> id n
|1 -> opp n
|_ -> failwith"prout";;

let died_ext n (h1,k1) (h2,k2) = semi_dir (n/2) (a n) (h1,k1) (h2,k2);;

let lexical_order (i,j) = 2*i + j;;
let recip_order_died k = (k/2,k mod 2);;
 
let died_op n (i,j) = lexical_order (died_ext n (recip_order_died i) (recip_order_died j));;

let inv n (h,k) = (a (n/2) k (opp (n/2) h),k);;
let transinv n i = lexical_order (inv n (recip_order_died i));;

let died_epsilon n (i,j) = let k = transinv n i in died_op n (j,k);;

(*renter n comme argument donne des informations sur Dn, pas D2n ni Dn/2 *)