let decomp_prod n = 
	let res = ref [] in
    let rec aux acc_prec acc_left acc_list = 
    	if acc_left = 1 then
			res := (acc_list)::(!res)
		else
        	for i = acc_prec to acc_left do
            	if n mod i = 0 then
                	aux i (acc_left/i) (i::acc_list)
            done;
    in
    	aux 2 n [];
        !res;;
        
let lexical_order m (i,j) = m*i+j;;

let reciproque_order m k = (k/m,k mod m);;

(*calcule les [|n|] à chaque étape du calcul*)
let sums arr =
	let n = Array.length arr in
    let res = Array.make n 0 in
    res.(0) <- arr.(0);
    for i=1 to n-1 do
    	res.(i) <- res.(i-1)*arr.(i)
    done;
    res;;


let lex_order arr x = 
    let n = Array.length arr in
    let res = ref (lexical_order arr.(1) (x.(0),x.(1))) in
    for i=1 to n-2 do
    	res:= lexical_order arr.(i+1) (!res,x.(i+1))
    done;
    !res;;

let recip arr x =
	let n = Array.length arr in
    if n=1 then [|x|]
    else begin
        let res = Array.make n 0 in
        let (a1,b1) = reciproque_order arr.(n-1) x in
        let a = ref a1 and b = ref b1 in
        for i=0 to n-2 do
        	res.(n-i-1) <- !b;
            if i<> n-2 then (
            	let new1  = reciproque_order arr.(n-2-i) !a in
            	a := fst new1;
            	b := snd new1 )
        done;
        res.(0) <- !a;
        res
        end;;

let op t1 t2 arr = 
	let n = Array.length t1 in
    let t = Array.make n 0 in
    for i = 0 to n-1 do
    	t.(i) <- (t1.(i) + t2.(i)) mod arr.(i)
    done;
    t;;
    
let loi arr i j = lex_order arr (op (recip arr i) (recip arr j) arr);;

let opbis t1 t2 arr = 
	let n = Array.length t1 in
    let t = Array.make n 0 in
    for i = 0 to n-1 do
    	t.(i) <- ((((t2.(i) - t1.(i)) ) mod arr.(i)) + arr.(i)) mod arr.(i);
    done;
    t;;

let abelian_epsilon arr i j = lex_order arr (opbis (recip arr i) (recip arr j) arr);;

