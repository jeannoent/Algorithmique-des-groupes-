(*open Loi_gp_ab*)
(*open Permutation*)

(*exception PermError*)

(*let%test "bijection" = nb_gen 12 (znz 12) = nb_gen 12 (abelian_epsilon [|3;4|])*)

(*let perm_test n = 
  try
    for i=0 to (fac n)-1 do
      if permToInt( intToPerm  i n ) <> i then (print_int i;raise PermError)
    done;
    true
  with _ -> false*)


(*let%test "permutation" =  perm_test 5*)


(*let%test "gen permutation" = print_string "Nb de part gen de S4: ";print_int (nb_gen (fac 4) (sym_epsilon 4)) ; false*)

(*let test_all_abelian n =
  let l = all_abelian n in
  let rec aux = function
    |[] -> true
    |h::r -> if not ((h mod n) = 0) then (print_int h;false)else aux r
  in aux l


(*let%test_unit "all abelian" = print_list_int  (List.map (fun x -> x mod 12) (all_abelian 12))*)

let rec notmap a l = match l with
|[] -> false
|h::d -> ((h = a))||(notmap a d);;

let print_bool = function
  |true -> print_endline "True"
  |false -> print_endline "False"

(*let%test "boucle abelien" = 
  let res = ref true in
  for i = 41 to 50 do
    print_int i;print_string ": ";
    let l = List.map (fun x -> x mod i) (all_abelian i) in
    let temp = notmap 0 l in
    if not temp then res:= false;
  print_bool temp
    done;
    !res*)

let nb_groups n = 
  let res =  ref 0 in
  for i=1 to n do    
    res:= !res + List.length (decomp_prod i)
  done;
  !res;;


(*let%test_unit "nb_groupes" = print_int (nb_groups 40)  *)

let test_mt n =
  let res = ref (Sys.time ()) in
  let _= Nb_gen.nb_gen n (znz n) in
  res:=Sys.time () -. !res;
  print_string "MT OFF: "; print_float !res;print_newline ();
  res := Sys.time ();
  let _= Nb_gen_mt.nb_gen n (znz n) in
  print_string "MT ON: "; print_float !res;print_newline ()

let%test_unit "multi-threading" = test_mt 40 *)

(*let%test_unit "big : " = print_string "nb gen znz 31: " ; print_string (Big_int.string_of_big_int (nb_gen_big 31 (znz 31)))*)

(*let fst3 (a,_,_) = a

let%test_unit "diédral" = print_string "nb gen D 2: " ; print_string  (fst3 (nb_gen ~print:true 4 (znz 4))); failwith "fuck"*)

open Decomposition_et_rang
open Nb_gen

let time f = 
  let a = Sys.time() in
  let _ = f () in
  print_float (Sys.time () -. a)

let%test_unit ""= time (fun () -> rg (star_group 128))