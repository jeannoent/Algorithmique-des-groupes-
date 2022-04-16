open Js_of_ocaml
open Lib_gen.Nb_gen
open Lib_gen.Nb_gen_draw
open Lib_gen.Decomposition_et_rang

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val abelianorder = Js.wrap_meth_callback
            (fun _ x -> Lib_gen.Loi_gp_ab.order (Js.to_array x)) (* Partie ordre abelian : Done *)
        val abelian = Js.wrap_meth_callback
            (fun _ x -> pi_abelian (Js.to_array x)) (* Partie gen groupe abelien : Done *)
        val symetric = Js.wrap_meth_callback
            (fun _ x -> pi_symetric(x)) (* Partie gen groupe symetrique : Nope *)
        val diedral = Js.wrap_meth_callback
            (fun _ x -> pi_diedral(x)) (* Partie gen groupe diedral : Nope *)
        val star = Js.wrap_meth_callback
            (fun _ x -> pi_star(x)) (* Partie gen groupe star : Nope *)
        val abeliang = Js.wrap_meth_callback
            (fun _ x -> pi_abelian_graph (Js.to_array x)) (* Partie gen graphe abelien : Nope *)
        val symetricg = Js.wrap_meth_callback
            (fun _ x -> pi_symetric_graph(x)) (* Partie gen graphe symetrique : Nope *)
        val diedralg = Js.wrap_meth_callback
            (fun _ x -> pi_diedral_graph(x)) (* Partie gen graphe diedral : Nope *)
        val starg = Js.wrap_meth_callback
            (fun _ x -> pi_star_graph(x)) (* Partie gen graphe star : Nope *)
        val rabelian = Js.wrap_meth_callback
            (fun _ x -> abelian_rg (Js.to_array x)) (* Partie rang abelien : Done *)
        val rsymetric = Js.wrap_meth_callback
            (fun _ x -> sym_rg(x)) (* Partie rang symetrique : Nope *)
        val rdiedral = Js.wrap_meth_callback
            (fun _ x -> died_rg(x)) (* Partie rang diedral : Nope *)
        val rstar = Js.wrap_meth_callback
            (fun _ x -> star_rg(x)) (* Partie rang star : Nope *)
        val dstar = Js.wrap_meth_callback
            (fun _ x -> decomp_inv(x)) (* C'est quoi ca ? *)
      end);

