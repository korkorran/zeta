
open Js_of_ocaml
(* open Vdom *)

let ping () = 
  let jsfunc = Js.Unsafe.js_expr "window.versions.ping" in
  let success = (fun r -> print_endline (Ojs.string_of_js r)) in 
  let error = (fun e -> print_endline ("Error: " ^ (Ojs.string_of_js e))) in
  let (promise : Ojs.t) = Js.Unsafe.fun_call jsfunc [||] in
  ignore @@ Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success|];
  ignore @@ Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error|]
let unused = Dom_html.document##.title := Js.string "New Title"

let electron_version = Js.to_string (Js.Unsafe.js_expr "versions.electron()")