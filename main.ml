open Ocaml.Nb_gen
open Js_of_ocaml

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val abelian = Js.wrap_meth_callback
            (fun _ x -> abelian_gen (Js.to_array x))
        val cyclic = Js.wrap_meth_callback
            (fun _ x -> pi_cyclic(x))
        val symetric = Js.wrap_meth_callback
            (fun _ x -> pi_symetric(x))
      end);