
open Js_of_ocaml
(* open Vdom *)

let await expr success error =
  try 
    let jsfunc = Js.Unsafe.js_expr expr in
    let (promise : Ojs.t) = Js.Unsafe.fun_call jsfunc [||] in
    ignore @@ Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success|];
    ignore @@ Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error|]
  with e -> print_endline ("Exception: " ^ Printexc.to_string e)

let ping () =
  let expr = "window.versions.ping" in
  let success = (fun r -> print_endline (Ojs.string_of_js r)) in 
  let error = (fun e -> print_endline ("Error: " ^ (Ojs.string_of_js e))) in
  await expr success error

let unused = Dom_html.document##.title := Js.string "New Title"

let electron_version = try
  Js.to_string (Js.Unsafe.js_expr "versions.electron()")
  with _ -> "Unknown"

let is_desktop_mode = 
  let open Tyre in
  let re = start *> int <* char '.' <&> int <* char '.' <&> int <* stop
  in execp (compile re) electron_version