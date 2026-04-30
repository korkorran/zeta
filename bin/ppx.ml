open Ppxlib

let generate_impl ~ctxt (rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  ignore (rec_flag, loc);
  List.concat_map (fun td ->
    match td.ptype_kind with
    | Ptype_record fields ->
      let count = List.length fields in
      let loc = td.ptype_loc in
      let binding_name = { txt = td.ptype_name.txt ^ "_field_count"; loc } in
      [
        Ast_builder.Default.pstr_value ~loc Nonrecursive
          [Ast_builder.Default.value_binding ~loc
            ~pat:(Ast_builder.Default.ppat_var ~loc binding_name)
            ~expr:(Ast_builder.Default.eint ~loc count)]
      ]
    | _ ->
      Location.raise_errorf ~loc:td.ptype_loc
        "[@@deriving count_fields] ne peut s'appliquer qu'à un type record") type_declarations

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let _ = Deriving.add "count_fields" ~str_type_decl:impl_generator
