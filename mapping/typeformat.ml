type typeformat = TUnit
                | TInt
                | TFloat
                | TBool
                | TString
                | TList of typeformat
                | TOption of typeformat
                | TTuple of typeformat list
                | TRecord of (string * typeformat) list
                | TVariant of (string * typeformat option) list


type value = VUnit
           | VInt of int
           | VFloat of float
           | VBool of bool
           | VString of string
           | VList of value list
           | VOption of value option
           | VTuple of value list
           | VRecord of (string * value) list
           | VVariant of string * value option

    
let rec validate typeformat value =
  match typeformat, value with
  | TUnit, VUnit -> true
  | TInt, VInt _ -> true
  | TFloat, VFloat _ -> true
  | TBool, VBool _ -> true
  | TString, VString _ -> true
  | TList tf, VList vs -> List.for_all (validate tf) vs
  | TOption tf, VOption v -> (match v with Some v' -> validate tf v' | None -> true)
  | TTuple tfs, VTuple vs -> List.length tfs = List.length vs && List.for_all2 validate tfs vs
  | TRecord fields, VRecord values ->
      let field_map = List.fold_left (fun acc (name, tf) -> (name, tf) :: acc) [] fields in
      List.for_all (fun (name, v) ->
        match List.assoc_opt name field_map with
        | Some tf -> validate tf v
        | None -> false) values
  | TVariant variants, VVariant (name, v_opt) ->
      (match List.assoc_opt name variants with
       | Some tf_opt ->
           (match tf_opt, v_opt with
            | Some tf, Some v -> validate tf v
            | None, None -> true
            | _, _ -> false)
       | None -> false)
  | _, _ -> false