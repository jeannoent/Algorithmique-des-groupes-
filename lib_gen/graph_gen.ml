open Loi_diedral
open Loi_gp_ab
open Permutation
open Nb_gen
open Big_int

let str_set_oflist l =
  let rec aux li = match li with
      |[] -> "}"
      |[x] -> (string_of_int  x)^"}"
      |h::l ->(string_of_int h)^","^(aux l)
in "{"^(aux l)


(*returns the next subtree, as well as all the nodes that compose sais subtree*)
let next_subtree_graph l max =
	let min = try List.hd l with _ -> max in
  let next = ref [] in
  let nodes = ref [] in
  for i=0 to min-1 do
    next:= (i::l) :: !next;
    nodes := (l::(i::l)::[]) :: !nodes
  done;
  (!next,!nodes)




let str_nodes l =
  let rec aux li = match li with
      |[] -> "]"
      |[x] -> "\""^(str_set_oflist x)^"\"]"
      |h::l -> "\""^(str_set_oflist h)^"\""^","^(aux l)
  in "["^(aux l)


let str_edges l =   
  let rec aux li = match li with
      |[] -> "]"
      |[x] -> (str_nodes x)^"]"
      |h::l -> (str_nodes h)^","^(aux l)
  in "["^(aux l)


let graph nodes edges = 
  "{\"nodes\": "^(str_nodes nodes) ^ ",\"edges\": "^(str_edges edges)^"}"




let nb_gen_draw n e  =
	let res= ref zero_big_int in
  let nodes = ref [[]] in
  let edges = ref [] in
    let rec aux l=
    	if est_gen (n,l,e) then
            (
        	res:= add_big_int !res (power_int_positive_int 2 (List.hd l))
            )
        else (
            let subtree = next_subtree_graph l n in
            nodes := (fst subtree)@ !nodes;
            edges := (snd subtree)@ !edges; 
            List.iter aux (next_subtree l n)
            )   
    in aux [];


    let modn = (quomod_big_int !res (big_int_of_int n)) in
    ((string_of_big_int !res,string_of_big_int (fst modn),string_of_big_int (snd modn)),graph !nodes !edges)    



let abelian_gen_graph arr = 
    let n = ref 1 and m = Array.length arr in
    for i = 0 to m-1 do
          n := arr.(i)*(!n)
    done;
    nb_gen_draw !n (abelian_epsilon arr);;

let pi_symetric_graph n = nb_gen_draw (fac n) (sym_epsilon n);;       
  
let pi_cyclic_graph n = nb_gen_draw n (znz n);;

let pi_diedral_graph n = nb_gen_draw n (died_epsilon n);;




(*let%test "graphe" = print_string (snd (nb_gen_draw 4 (znz 4)));false*)