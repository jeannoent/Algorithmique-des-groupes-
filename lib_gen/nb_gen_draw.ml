open Nb_gen
open Big_int

let str_set_oflist b l =
  let rec aux li = match li with
      |[] -> Buffer.add_char b '}'
      |[x] -> Buffer.add_string b (string_of_int  x); Buffer.add_char b '}'
      |h::l ->Buffer.add_string b (string_of_int h); Buffer.add_char b ','; aux l
in Buffer.add_char b '{';aux l


(*returns the next subtree, as well as all the nodes that compose sais subtree*)
let next_subtree_graph l max =
	let min = try List.hd l with _ -> max in
  let next = ref [] in
  let nodes = ref [] in
  for i=0 to min-1 do
    let u = (min-1-i)::l in
    next:= u :: !next;
    nodes := (l,u) :: !nodes
  done;
  (!next,!nodes)


let str_nodes b  l =
  let rec aux li = match li with
      |[] -> Buffer.add_char b ']'
      |[x] -> Buffer.add_string b "{\"data\" : {\"id\" : \"";str_set_oflist b x;Buffer.add_string b "\" }}]"
      |h::l -> Buffer.add_string b "{\"data\" : {\"id\" : \"";str_set_oflist b h;Buffer.add_string b "\" }},";aux l
  in Buffer.add_char b '[';(aux l)

let str_couple b (a1,a2) =
  Buffer.add_string b "{\"data\" : {\"source\" : \"";
  str_set_oflist b a1; 
  Buffer.add_string b "\", \"target\" : \"";
  str_set_oflist b a2;
  Buffer.add_string b "\"}}"


let str_edges b l =   
  let rec aux li = match li with
      |[] -> Buffer.add_char b ']'
      |[x] -> str_couple b x;Buffer.add_char b ']'
      |h::l -> str_couple b h;Buffer.add_char b ',';aux l
  in  Buffer.add_char b '[';aux l


let graph nodes edges = 
  let b = Buffer.create 10000 in
  Buffer.add_string b "{\"nodes\" : "
  ;str_nodes b nodes;
  Buffer.add_string b ",\"edges\": ";
  str_edges b edges;
  Buffer.add_char b '}';
  Buffer.contents b



let nb_gen_draw (g:group)  =
  let n = g.order in
	let res= ref zero_big_int in
  let nodes = ref [[]] in
  let edges = ref [] in
    let rec aux l=
    	if est_gen g l then
            (
        	res:= add_big_int !res (power_int_positive_int 2 (List.hd l))
            )
        else (
            let subtree = next_subtree_graph l n in
            nodes := (fst subtree)@ !nodes;
            edges := (snd subtree)@ !edges; 
            List.iter aux (next_subtree l)
            )   
    in for i=0 to n-1 do
      aux [i]
    done;


    let modn = (quomod_big_int !res (big_int_of_int n)) in
    ((string_of_big_int !res,string_of_big_int (fst modn),string_of_big_int (snd modn)),graph !nodes !edges)    



let abelian_gen_graph arr = 
    nb_gen_draw (abelian_group arr);;

let pi_abelian_graph arr = nb_gen_draw (abelian_group arr)    

let pi_symetric_graph n = nb_gen_draw (symmetric_group n);;       
  
let pi_cyclic_graph n = nb_gen_draw (abelian_group [|n|]);;

let pi_diedral_graph n = nb_gen_draw (diedral_group n);;

let pi_star_graph n = nb_gen_draw (star_group n);;




(*let%test "graphe" = print_string (snd (nb_gen_draw 4 (znz 4)));false*)