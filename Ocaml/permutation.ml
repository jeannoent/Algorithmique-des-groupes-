let decomposerbase k n =
	let res = Array.make n 0 in
    let current = ref k in
    res.(0) <- k mod n;
    for i=0 to n-2 do
    	current := (!current - res.(i))/n;
        res.(i+1) <- !current mod n
    done;
    res;;
let rec fac n = match n with
|0 -> 1
|n -> n*(fac (n-1));;
 
let decomposerFact k n =
	let res = Array.make n 0 in
    let current = ref k in
    let f =  ref (fac (n-1)) in
    for i = 0 to n-2 do
    	res.(i) <- !current / !f;
        current := !current - res.(i)*(!f);
        f := !f / (n-i-1)
    done;
    res.(n-1)<- !current;
    res;;
    
let rec pop i l = match l with
	|[] -> failwith "samarchpa"
    |h::r -> if i=0 then (h,r) 
    		else (
            	let t = pop (i-1) r in
                (fst t, h::(snd t))
                );;
                
let listrange n =
	let rec aux n k =
    	if k=(n-1) then [n-1]
        else (k)::(aux n (k+1))
    in aux n 0;;

let intToPerm k n =
	let res = Array.make n 0 in
    let t = decomposerFact k n in
    let l = ref (listrange n) in
    for i=0 to n-1 do
    	let temp = pop t.(i) !l in
    	res.(i) <- fst temp;
        l := snd temp
    done;
    res;;
    
let comp t1 t2 = 
	let n = Array.length t1 in
	let t = Array.make n 0 in
    for i = 0 to n-1 do
    	t.(i) <- t2.(t1.(i))
    done;
    t;;

let inv t = 
	let n = Array.length t in
    let res = Array.make n 0 in
    for i = 0 to n-1 do
    	res.(t.(i)) <- i
    done;
    res;;


(*fonction équivalent au système de liste par compréhension [i for i in L if f(i)] en python*)    
let rec sort_out f l = match l with
    |[] -> []
    |h::r -> if f h then h::(sort_out f r) else (sort_out f r)


(*needs further testing*)
let permToDecomp t =
    let n = Array.length t in
    let l = ref [t.(n-1)] in
    let decomp = ref [] in
    for i=0 to n-2 do
        let o = n-i-2 in
        let first = sort_out (fun x -> x<t.(o)) !l  in
        l := first@[t.(o)]@(sort_out (fun x -> x>t.(o)) !l );
        decomp:= (List.length first)::!decomp
    done;
    List.rev !decomp;;

let permToInt t =   
    let dec = permToDecomp t in
    let p = ref 1 and k = ref 0 in
        let rec aux l i = match l with
        |[] -> ()
        |h::d -> p := (!p)*i;
                 k := !k + h*(!p);
                 aux d (i+1)
        in
            aux dec 1;
    !k;;


        
        

