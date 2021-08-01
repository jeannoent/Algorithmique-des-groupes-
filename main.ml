open Js_of_ocaml
open Lib_gen.Graph_gen

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val abelian = Js.wrap_meth_callback
            (fun _ x -> abelian_gen_graph (Js.to_array x))
        val cyclic = Js.wrap_meth_callback
            (fun _ x -> pi_cyclic_graph(x))
        val symetric = Js.wrap_meth_callback
            (fun _ x -> pi_symetric_graph(x))
        val diedral = Js.wrap_meth_callback
            (fun _ x -> pi_diedral_graph(x))
      end);

