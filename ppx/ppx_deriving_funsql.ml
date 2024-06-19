open Ppxlib
module List = ListLabels
open Ast_builder.Default

let ret = "ret"
let row = "row"

let rec typ ~loc fname = function
  | { ptyp_desc =
        Ptyp_constr
          ( { txt = Lident (("int" | "bool" | "int64" | "float") as name); _ },
            [] );
      _
    } ->
    (* The ret decoder function has the same name as the type *)
    evar ~loc name
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string"; _ }, []); _ } ->
    (* The string decoder is called 'text' *)
    evar ~loc "text"
  | { ptyp_desc =
        (* The type is like M.t or _ M.t or (_, _) M.t and so on *)
        Ptyp_constr ({ txt = Ldot ((Lident _ as modident), "t"); _ }, _);
      _
    } ->
    (* Eg: fun i row -> M.of_string (text i row) *)
    eabstract ~loc
      [pvar ~loc "i"; pvar ~loc row]
      (eapply ~loc
         (pexp_ident ~loc { loc; txt = Ldot (modident, "of_string") })
         [eapply ~loc (evar ~loc "text") [evar ~loc "i"; evar ~loc row]])
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option"; _ }, [opt_type]); _ } ->
    (* Eg: type is string option, decoder func is 'opt text' *)
    eapply ~loc (evar ~loc "opt") [typ ~loc fname opt_type]
  | _ -> failwith ("Cannot derive type for field: " ^ fname)

let field_impl i ld =
  let loc = ld.pld_loc in
  let fieldname = ld.pld_name.txt in
  (* fieldname = typ i row *)
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
      | { ptype_kind = Ptype_record fields; _ } ->
        (* let ret = ret (fun row -> { ... }) *)
        [ (try
             pstr_value ~loc Nonrecursive
               [ value_binding ~loc
                   ~pat:(ppat_var ~loc { loc; txt = ret })
                   ~expr:
                     (eapply ~loc (evar ~loc ret)
                        [ eabstract ~loc
                            [ppat_var ~loc { txt = row; loc }]
                            (pexp_record ~loc
                               (List.mapi fields ~f:field_impl)
                               None) ]) ]
           with Failure msg ->
             pstr_extension ~loc (Location.error_extensionf ~loc "%s" msg) [])
        ])
  |> List.concat

let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl
let _ = Deriving.add ~str_type_decl "funsql"
