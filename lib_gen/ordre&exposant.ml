open Nb_gen

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

let exponent_list g f = (*à un groupe renvoie la liste des élément dont l'ordre est égal à l'exposant*)
	let res = ref [] and e = exponent g in
    let n = g.order in
    for i = 0 to n-1 do
    	if (order g i) = e then
        	res := i::(!res);
	done;
    List.map f !res;;


