open Ppxlib

let gensym =
  let n = ref 999 in
  fun prefix ->
    n := !n + 1;
    Printf.sprintf "%s_%d" prefix !n

let level_to_string = function
  | Logs.App -> "Logs.app"
  | Logs.Error -> "Logs.err"
  | Logs.Warning -> "Logs.warn"
  | Logs.Info -> "Logs.info"
  | Logs.Debug -> "Logs.debug"

let target_expr level loc fmt_string rest =
  let open Ppxlib.Ast_builder.Make (struct let loc = loc end) in
  let m_name = gensym "m" in
  let level_fn = pexp_ident @@ Located.lident @@ level_to_string level in
  let call_ = pexp_apply (pexp_ident (Located.lident m_name)) @@
      (Nolabel, estring fmt_string)::rest
  in
  let fun_ = pexp_fun Nolabel None (pvar m_name) call_ in
  pexp_apply level_fn [Nolabel, fun_]

let expander level ~ctxt fmt_string rest =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let {Lexing.pos_fname;pos_lnum;_} = loc.loc_start in
  let fmt_string = Printf.sprintf "%s:%d %s" pos_fname pos_lnum fmt_string in
  target_expr level loc fmt_string rest


let () =
  let pattern =
    let open Ast_pattern in
    let k'' f a = f a [] in
    single_expr_payload (
      (pexp_apply (estring __) __) ||| (map ~f:k'' (estring __)))
  in
  let make_rule (name, level) =
    let extension =
      Extension.V3.declare name
        Extension.Context.expression
        pattern
        (expander level)
    in
    let rule = Context_free.Rule.extension extension in
    rule
  in
  let rules = List.map make_rule [
      "logs", Logs.App;
      "logs.app", Logs.App;
      "logs.err", Logs.Error;
      "logs.warn", Logs.Warning;
      "logs.info", Logs.Info;
      "logs.debug", Logs.Debug;
    ]
  in
  Ppxlib.Driver.register_transformation ~rules "ppx_logs"
