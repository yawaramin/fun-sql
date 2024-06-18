open Ppxlib
module List = ListLabels
open Ast_builder.Default

let rec typ ~loc fname = function
  | { ptyp_desc =
        Ptyp_constr
          ( { txt = Lident (("int" | "bool" | "int64" | "float") as name); _ },
            [] );
      _
    } -> pexp_ident ~loc { loc; txt = lident name }
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string"; _ }, []); _ } ->
    pexp_ident ~loc { loc; txt = lident "text" }
  | { ptyp_desc = Ptyp_constr ({ txt = Ldot (Lident _, "t") as modident; _ }, []);
      _
    } -> pexp_ident ~loc { loc; txt = modident }
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [opt_type]); _ } ->
    eapply ~loc
      (pexp_ident ~loc { loc; txt = lident "opt" })
      [typ ~loc fname opt_type]
  | _ -> failwith ("Cannot derive type for field: " ^ fname)

let row = "row"

let field_impl i (ld : label_declaration) =
  let loc = ld.pld_loc in
  let fieldname = ld.pld_name.txt in
  ( { txt = Lident fieldname; loc },
    eapply ~loc (typ ~loc fieldname ld.pld_type) [eint ~loc i; evar ~loc row] )

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
          ptype_loc;
          _
        } ->
        let ext =
          Location.error_extensionf ~loc:ptype_loc
            "Cannot derive accessors for non-record types"
        in
        [pstr_extension ~loc ext []]
      | { ptype_name; ptype_kind = Ptype_record fields; _ } ->
        [ (try
             pstr_value ~loc Nonrecursive
               [ value_binding ~loc ~pat:(ppat_var ~loc ptype_name)
                   ~expr:
                     (eabstract ~loc
                        [ppat_var ~loc { txt = row; loc }]
                        (pexp_record ~loc (List.mapi fields ~f:field_impl) None))
               ]
           with Failure msg ->
             pstr_extension ~loc (Location.error_extensionf ~loc "%s" msg) [])
        ])
  |> List.concat

let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl
let funsql = Deriving.add ~str_type_decl "funsql"
