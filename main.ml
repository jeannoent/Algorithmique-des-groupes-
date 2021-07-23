open Js_of_ocaml

(* Code ML *)
let rec multiply = function
|[] -> 1
|h::t -> h*(multiply t)


let resolve n=
    let l = ref [] in
    while (multiply !l)<>n do
           let m = (multiply !l) in
        if m < n then l:= 2::(!l)   
        else match !l with
        |[] -> ()
        |[_] -> failwith "nop"
        |_::(h2::t) -> l:= (h2+1)::t
    done;
    !l;;

(* Gestion callback *)
let _ = 
  Js.Unsafe.global##.ocamlcallback := (object%js
        val resolve = Js.wrap_meth_callback
            (fun _ x -> Js.array (Array.of_list (resolve x)))
      end);