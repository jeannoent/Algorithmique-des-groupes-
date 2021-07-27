open Ocaml
open Js_of_ocaml

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val abelian = Js.wrap_meth_callback
            (fun _ x -> abelian_gen (Js.to_array x))
      end);