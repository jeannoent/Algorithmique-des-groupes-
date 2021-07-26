(*open Loi_gp_ab*)
open Nb_gen
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

let test_all_abelian n =
  let l = all_abelian n in
  let rec aux = function
    |[] -> true
    |h::r -> if not ((h mod n) = 0) then (print_int h;false)else aux r
  in aux l


  let%test_unit "all abelian" = print_list_int  (List.map (fun x -> x mod 12) (all_abelian 12))
