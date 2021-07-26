let processed_laws = ref ([||],[||])


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


let lex_order arr t = 
	let n = Array.length arr in
    let p = ref 1 and res = ref t.(n-1) in
    for i = 1 to n-1 do
    	p := (!p)*arr.(n-i);
        res := !res + (!p)*t.(n-i-1);
    done;
    !res;;

    
let recip_order arr m = 
	let n = Array.length arr in
    let t = Array.make n 0 and elt = ref m in
    t.(n-1) <- m mod arr.(n-1);
    for i = 0 to n-2 do
    	elt := (!elt - t.(n-i-1))/arr.(n-i-1);
        t.(n-i-2) <- !elt mod arr.(n-i-2);
    done;
    t;;
 
        
let op t1 t2 arr = 
    let n = Array.length arr in
    let t = Array.make n 0 in
    for i = 0 to n-1 do
      t.(i) <- (t1.(i)+t2.(i)) mod arr.(i)
    done;
    t;;
    

let loi arr i j = lex_order arr (op (recip_order arr i) (recip_order arr j) arr);;


let opbis t1 t2 arr = 
	let n = Array.length t1 in
    let t = Array.make n 0 in
    for i = 0 to n-1 do
    	t.(i) <- ((((t2.(i) - t1.(i)) ) mod arr.(i)) + arr.(i)) mod arr.(i);
    done;
    t;;


let order arr =
    let n = Array.length arr in
    let res = ref 1 in
    for i=0 to n-1 do
        res:= !res*arr.(i)
    done;
    !res;;


let process_law arr =
    let ord = order arr in
    let res = Array.make_matrix ord ord 0 in
    for i=0 to ord-1 do
        for j=0 to ord-1 do
        res.(i).(j) <- lex_order arr (opbis (recip_order arr i) (recip_order arr j) arr)
        done;
    done;
    processed_laws := (arr,res);;


let abelian_epsilon arr (i,j) = 
    if fst !processed_laws <> arr then 
        process_law arr;
    (snd !processed_laws).(i).(j)
    ;;

