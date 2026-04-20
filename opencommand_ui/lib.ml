
open Js_of_ocaml
open Js_browser

type 'msg Vdom.Cmd.t +=
  | Await of {expr: string; on_failure: (string -> 'msg); on_success: (string -> 'msg)}
  | RunCommand of {cmd: string; on_failure: (string -> 'msg); on_success: (string -> 'msg)}
  | Ping of {on_failure: (string -> 'msg); on_success: (string -> 'msg)}
  | After of int * 'msg

let await ?(args=[||]) expr ~success ~error =
  try 
    let jsfunc = Js.Unsafe.js_expr expr in
    let (promise : Ojs.t) = Js.Unsafe.fun_call jsfunc args in
    ignore @@ Js.Unsafe.meth_call promise "then" [|Js.Unsafe.inject success|];
    ignore @@ Js.Unsafe.meth_call promise "catch" [|Js.Unsafe.inject error|]
  with e -> print_endline ("Exception: " ^ Printexc.to_string e)

let ping () ~success ~error =
  let expr = "window.electronAPI.ping" in
  await expr ~success ~error


let runCommand command ~error ~success =
  let expr = "window.electronAPI.runCommand" in
  let args = [|Js.Unsafe.inject (Js.string command)|] in
(*   let success = (fun r -> print_endline (Printf.sprintf "Command '%s' output: %s" command (Ojs.string_of_js r))) in 
  let error = (fun e -> print_endline (Printf.sprintf "Error running command '%s': %s" command (Ojs.string_of_js e))) in *)
  await ~args expr ~success ~error

let cmd_handler ctx = function
  | Await {expr; on_failure; on_success} ->
      await expr ~success:(fun r -> Vdom_blit.Cmd.send_msg ctx (on_success r)) ~error:(fun e -> Vdom_blit.Cmd.send_msg ctx (on_failure e));
      true
  | RunCommand {cmd; on_failure; on_success} ->
      runCommand cmd ~error:(fun e -> Vdom_blit.Cmd.send_msg ctx (on_failure e)) ~success:(fun r -> Vdom_blit.Cmd.send_msg ctx (on_success r));
      true
  | Ping {on_failure; on_success} ->
      ping () ~success:(fun r -> Vdom_blit.Cmd.send_msg ctx (on_success r)) ~error:(fun e -> Vdom_blit.Cmd.send_msg ctx (on_failure e));
      true
  | After (n, msg) ->
      ignore (Window.set_timeout window (fun () -> Vdom_blit.Cmd.send_msg ctx msg) n);
      true
  | _ ->
      false

let () = Vdom_blit.(register (cmd {f = cmd_handler}))

let unused = Dom_html.document##.title := Js.string "New Title"

let electron_version = try
  Js.to_string (Js.Unsafe.js_expr "window.electronAPI.electron_version()")
  with _ -> "Unknown"

let is_desktop_mode = 
  let open Tyre in
  let re = start *> int <* char '.' <&> int <* char '.' <&> int <* stop
  in execp (compile re) electron_version