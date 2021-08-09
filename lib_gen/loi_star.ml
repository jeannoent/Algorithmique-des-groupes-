let rec pgcd a b = match b with
|0 -> a
|b -> pgcd b (a mod b);;

let euler n = 
	let r = ref 0 in
    for i = 1 to n do 
    	if pgcd n i = 1 then r := !r + 1;
    done;
    !r;;
let starToInt n i = 
	let res = ref 0 in
    for j = 0 to i-1 do
    	if pgcd j n = 1 then
        	res := !res + 1;
    done;
    !res;;
    
let mult_mod_n n (a,b) = (a*b) mod n;;


(*returne a^n*)    
let rec pow a n = match n with
  | 0 -> 1
  | 1 -> a
  | _ -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)


let inv n k = 
	let p = euler n in
	let r = pow k (p-1) in
    r mod n;;

let intToStar n i =
	let r = ref 0 in
    for k = 0 to n-1 do
    	if (pgcd n k = 1) && (starToInt n k = i) then
        	r := k;
    done;
    !r;;

let int_inv n k = starToInt n (inv n (intToStar n k));;
    
let op n (i,j) = starToInt n (mult_mod_n n ((intToStar n i),(intToStar n j) ));; 

let epsilon_star n (i,j) = op n (j,int_inv n i);;