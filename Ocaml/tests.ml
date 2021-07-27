(*open Loi_gp_ab
open Nb_gen*)
(*open Permutation

exception PermError

let%test "bijection" = nb_gen 12 (znz 12) = nb_gen 12 (abelian_epsilon [|3;4|]) 

let perm_test n = 
  try
    for i=0 to (fac n)-1 do
      if permToInt( intToPerm  i n ) <> i then (print_int i;raise PermError)
    done;
    true
  with _ -> false


let%test "permutation" =  perm_test 5


let%test "gen permutation" = print_string "Nb de part gen de S4: ";print_int (nb_gen (fac 4) (sym_epsilon 4)) ; false

*)

(*let test_all_abelian n =
  let l = all_abelian n in
  let rec aux = function
    |[] -> true
    |h::r -> if not ((h mod n) = 0) then (print_int h;false)else aux r
  in aux l


  let%test_unit "all abelian" = print_list_int  (List.map (fun x -> x mod 12) (all_abelian 12))

let rec notmap a l = match l with
|[] -> false
|h::d -> ((h = a))||(notmap a d);;

let print_bool = function
  |true -> print_endline "True"
  |false -> print_endline "False"

let%test "boucle abelien" = 
  let res = ref true in
  for i = 1 to 10 do
    print_int i;print_string ": ";
    let l = List.map (fun x -> x mod i) (all_abelian i) in
    let temp = notmap 0 l in
    if not temp then res:= false;
    print_bool temp
    done;
    !res

  let%test "boucle abelien" = for i = 1 to 10 do
                                let l = List.map (fun x -> x mod i) (all_abelian i) in
                                notmap 0 l*)

