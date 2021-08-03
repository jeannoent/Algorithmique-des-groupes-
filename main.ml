open Js_of_ocaml
open Lib_gen.Nb_gen
open Lib_gen.Graph_gen

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val abelian = Js.wrap_meth_callback
            (fun _ x -> abelian_gen (Js.to_array x))
        val cyclic = Js.wrap_meth_callback
            (fun _ x -> pi_cyclic(x))
        val symetric = Js.wrap_meth_callback
            (fun _ x -> pi_symetric(x))
        val diedral = Js.wrap_meth_callback
            (fun _ x -> pi_diedral(x))
        val star = Js.wrap_meth_callback
            (fun _ x -> pi_star(x))
        val abeliang = Js.wrap_meth_callback
            (fun _ x -> abelian_gen_graph (Js.to_array x))
        val cyclicg = Js.wrap_meth_callback
            (fun _ x -> pi_cyclic_graph(x))
        val symetricg = Js.wrap_meth_callback
            (fun _ x -> pi_symetric_graph(x))
        val diedralg = Js.wrap_meth_callback
            (fun _ x -> pi_diedral_graph(x))
        val starg = Js.wrap_meth_callback
        (fun _ x -> pi_star_graph(x))
      end);

