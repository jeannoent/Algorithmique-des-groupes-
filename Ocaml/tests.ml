open Loi_gp_ab
open Nb_gen
open Permutation

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