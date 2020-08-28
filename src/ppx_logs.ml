open Migrate_parsetree
open Ast_405

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

let gensym =
  let n = ref 999 in
  fun prefix ->
    n := !n + 1;
    Printf.sprintf "%s_%d" prefix !n

let target_expr loc level params =
  let mk_ident name =
    Exp.ident (Location.mkloc (Longident.parse name) loc) in
  let m_name = gensym "m" in
  let call = Exp.apply ~loc (mk_ident m_name) params in
  let fun_m = Exp.fun_ Nolabel None (Pat.var @@ Location.mkloc m_name loc) call in
  let level = match level with
    | Logs.App -> mk_ident "Logs.app"
    | Logs.Error -> mk_ident "Logs.err"
    | Logs.Warning -> mk_ident "Logs.warn"
    | Logs.Info -> mk_ident "Logs.info"
    | Logs.Debug -> mk_ident "Logs.debug"
  in
  Exp.apply ~loc level [Nolabel, fun_m]

let wrap_loc loc s =
  let fname, row, _col = Location.(get_pos_info loc.loc_start) in
  Printf.sprintf "%s:%d %s" fname row s

let parse_one loc level s =
  let s = wrap_loc loc s in
  let s_exp = Exp.constant ~loc (Const.string s) in
  target_expr loc level [Nolabel, s_exp]

let parse_many loc level s params =
  let s = wrap_loc loc s in
  let s_exp = Exp.constant ~loc (Const.string s) in
  let params = (Nolabel, s_exp) :: params in
  target_expr loc level params

let parse_one_or_many loc level = function
  | {
    pexp_desc =
      Pexp_constant (Pconst_string (
          fst_string, _)); _} ->
    parse_one loc level fst_string
  | {
    pexp_desc =
      Pexp_apply ({
          pexp_desc =
            Pexp_constant (Pconst_string (
                fst_string, _)); _},
          other_elts);_} ->
    parse_many loc level fst_string other_elts
  | _ -> Location.raise_errorf ~loc
           "[%%logs]: unsupported syntax"


let mapper _config _cookies =
  let expr mapper = function
    | {pexp_desc =
         Pexp_extension
           ({txt = "logs"
                 | "logs.app"
                 | "logs.err"
                 | "logs.warn"
                 | "logs.info"
                 | "logs.debug"
                   as txt; _},
            PStr
              [{pstr_desc =
                  Pstr_eval (expression, _); _}])
      ; pexp_loc = loc;_ } ->
      let level = match txt with
        | "logs.app" -> Logs.App
        | "logs.err" -> Logs.Error
        | "logs.warn" -> Logs.Warning
        | "logs.info" -> Logs.Info
        | "logs.debug" -> Logs.Debug
        | _ -> Logs.Info
      in
      parse_one_or_many loc level expression
    | e -> default_mapper.expr mapper e
  in
  {default_mapper with expr}


let () =
  Driver.register ~name:"logs" Versions.ocaml_405 mapper
