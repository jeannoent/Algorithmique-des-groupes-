open Js_of_ocaml
open Lib_gen.Nb_gen
open Lib_gen.Nb_gen_draw
open Lib_gen.Decomposition_et_rang

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val abelian = Js.wrap_meth_callback
            (fun _ x -> pi_abelian (Js.to_array x))
        val symetric = Js.wrap_meth_callback
            (fun _ x -> pi_symetric(x))
        val diedral = Js.wrap_meth_callback
            (fun _ x -> pi_diedral(x))
        val star = Js.wrap_meth_callback
            (fun _ x -> pi_star(x))
        val abeliang = Js.wrap_meth_callback
            (fun _ x -> pi_abelian_graph (Js.to_array x))
        val symetricg = Js.wrap_meth_callback
            (fun _ x -> pi_symetric_graph(x))
        val diedralg = Js.wrap_meth_callback
            (fun _ x -> pi_diedral_graph(x))
        val starg = Js.wrap_meth_callback
            (fun _ x -> pi_star_graph(x))
        val rabelian = Js.wrap_meth_callback
            (fun _ x -> abelian_rg (Js.to_array x))
        val rsymetric = Js.wrap_meth_callback
            (fun _ x -> sym_rg(x))
        val rdiedral = Js.wrap_meth_callback
            (fun _ x -> died_rg(x))
        val rstar = Js.wrap_meth_callback
            (fun _ x -> star_rg(x))
        val dstar = Js.wrap_meth_callback
            (fun _ x -> decomp_inv(x))
      end);

