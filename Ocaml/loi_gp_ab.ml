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


let lex_order arr l = 
    let rec aux l i = 
    match l with
        |[] -> failwith "empty set"
        |[x] -> x
            |n::m::r2 -> aux ((lexical_order arr.(i+1) (n,m))::r2) (i+1)
    in aux l 0;;


let l_o_t (a,b) = (*print_int a; print_newline ();*) [b;a];;

let recip arr x =
    let n = Array.length arr in
    if n=1 then [x]
    else begin
        let rec aux y i =
            if i=2 then l_o_t (reciproque_order arr.(1) y)
            else begin
                 let (a,b) = reciproque_order arr.(i-1) y in
                    (*print_int a; print_newline ();*)
                    b::(aux a (i-1))
                end
        in List.rev (aux x n)
    end;;

let op t1 t2 arr = 
    let rec aux l1 l2 i = match l1, l2 with 
        |[],[] -> []
        |h1::r1,h2::r2 -> (h1 + h2 mod arr.(i))::(aux r1 r2 (i+1))
        |_ -> failwith "error"
    in aux t1 t2 0

    
let loi arr i j = lex_order arr (op (recip arr i) (recip arr j) arr);;

let opbis t1 t2 arr = 
    let rec aux l1 l2 i = match l1, l2 with 
        |[],[] -> []
        |h1::r1,h2::r2 -> (((h2 + h1) mod arr.(i))+ arr.(i) mod arr.(i))::(aux r1 r2 (i+1))
        |_ -> failwith "error"
    in aux t1 t2 0


let abelian_epsilon arr (i,j) = lex_order arr (opbis (recip arr i) (recip arr j) arr);;


let rec check arr n i = if i=(n-1) then lex_order arr (recip arr (n-1)) = (n-1)
    else (lex_order arr (recip arr i) = i) && (check arr n (i+1))


