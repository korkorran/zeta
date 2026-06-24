  include Map.Make(String);;
  let pp pp_v fmt m =
    let bindings = bindings m in
    let pp_binding fmt (k, v) =
      Format.fprintf fmt "%s: %a" k pp_v v
    in
    List.iter (fun b ->
      pp_binding fmt b;
      Format.fprintf fmt "\n"
    ) bindings

  let to_yojson to_yojson m =
    let bindings = bindings m in
    `Assoc (List.map (fun (k, v) -> (k, to_yojson v)) bindings)

  let of_yojson of_yojson = function
    | `Assoc bindings ->
        Ok (List.fold_left (fun acc (k, v) ->
          match of_yojson v with
          | Ok v' -> add k v' acc
          | Error _e -> acc  (* You might want to handle errors differently *)
        ) empty bindings)
    | _ -> Error "Invalid JSON format for StringMap"  (* You might want to handle this case differently *)