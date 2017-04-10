
open Asttypes
open Parsetree
module M = Ast_mapper

let exp_dict : (string, string) Hashtbl.t = Hashtbl.create 3607;;
let module_dict : (string, string) Hashtbl.t = Hashtbl.create 3607;;
let types_dict : (string, string) Hashtbl.t = Hashtbl.create 3607;;
let module_member_dict : ((string * string), string) Hashtbl.t = Hashtbl.create 3607;;
let exceptions_dict : (string, string) Hashtbl.t = Hashtbl.create 3607;;

(* Module Links *)
let get_basic_name n =
  try (Some (Hashtbl.find module_dict n))
  with | Not_found -> None
;;

let get_module_member mdl mbr =
	match get_basic_name mdl with
	| None -> None
	| Some mdl1 -> (
		try Some (mdl1, (Hashtbl.find module_member_dict (mdl1, mbr))) with
		| Not_found -> None
	)
;;

let test_mapper argv  =  {
	M.default_mapper with 
  M.expr = (
		fun mapper expr -> match expr with
    (* {pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "print_string"}}, *)
    | {pexp_desc = Pexp_apply ((
        {pexp_desc = Pexp_ident ({txt = Longident.Lident str} as p1)} as p0), x)} as pat -> (
      M.default_mapper.M.expr mapper (
      try (
        let zh = Hashtbl.find exp_dict str in
        {pat with pexp_desc = Pexp_apply (
          {p0 with pexp_desc = Pexp_ident {p1 with txt = Longident.Lident zh}}, x)}
      ) with | Not_found -> pat))
    (* {pexp_desc = Pexp_construct ({txt = Lident "Not_found"}, None)} *)
    | {pexp_desc = Pexp_construct (({txt = Longident.Lident e} as p1), x)} as p0 -> (
      M.default_mapper.M.expr mapper (
        {p0 with pexp_desc = Pexp_construct (
          {p1 with txt = Longident.Lident
            (try Hashtbl.find exceptions_dict e with | Not_found -> e)}, x)}
      )
    )
    (* {pexp_desc = Pexp_ident {txt = Ldot (Lident "List", "map")}} *)
    | {pexp_desc = Pexp_ident ({txt = Longident.Ldot (Longident.Lident mdl, mbr)} as p1)} as p0 -> (
      let mdl, mbr = match (get_module_member mdl mbr) with
        | Some n -> n 
        | None -> mdl, mbr in
      M.default_mapper.M.expr mapper (
        {p0 with pexp_desc = 
          Pexp_ident {p1 with txt = Longident.Ldot (Longident.Lident mdl, mbr)}})
      )
    (* {pexp_desc = Pexp_ident {txt = Lident "string_of_int"}} *)
    | {pexp_desc = Pexp_ident ({txt = Longident.Lident s} as p1)} as p0 -> (
      M.default_mapper.M.expr mapper (
        {p0 with pexp_desc = Pexp_ident (
          {p1 with txt = Longident.Lident
            (try Hashtbl.find exp_dict s with | Not_found -> s)})}
      )
    )
		| other -> (M.default_mapper.M.expr mapper other));
  M.pat = (
    (* {ppat_desc = Ppat_construct ({txt = Lident "End_of_file"}, _)} *)
    fun mapper p_expr -> 
      M.default_mapper.M.pat mapper (
      match p_expr with
      | {ppat_desc = Ppat_construct (({txt = Longident.Lident s} as p1), x)} as p0 ->
        {p0 with ppat_desc = Ppat_construct (
          {p1 with txt = Longident.Lident
            (try Hashtbl.find exceptions_dict s with | Not_found -> s)}, x)}
      | other -> other 
      )
  );
  M.module_expr = (
    (* {pmod_desc = Pmod_ident {txt = Lident "List"}} *)
    fun mapper m_expr -> match m_expr with
    | {pmod_desc = Pmod_ident ({txt = Longident.Lident str} as p1)} as p0 -> (
      M.default_mapper.M.module_expr mapper (
      try (
        let zh = Hashtbl.find module_dict str in
        {p0 with pmod_desc = Pmod_ident ({p1 with txt = Longident.Lident zh})}
      ) with | Not_found -> p0)
    )
    | other -> (M.default_mapper.M.module_expr mapper other));
  M.typ = (
    (* {ptyp_desc = Ptyp_constr ({txt = Lident "string"}, [])} *)
    fun mapper t_expr -> match t_expr with
    | {ptyp_desc = Ptyp_constr (({txt = Longident.Lident str} as p1), x)} as p0 -> (
      M.default_mapper.M.typ mapper (
      try (
        let zh = Hashtbl.find types_dict str in
        {p0 with ptyp_desc = Ptyp_constr ({p1 with txt = Longident.Lident zh}, x)}
      ) with | Not_found -> p0)
    )
    | other -> (M.default_mapper.M.typ mapper other));
  M.structure_item = (
    (* {pstr_desc = Pstr_exception {pext_name = {txt = "F"}; pext_kind = Pext_rebind {txt = Lident "Failure"}}} *)
    fun mapper e_expr -> 
      M.default_mapper.M.structure_item mapper (
      match e_expr with
      | {pstr_desc = Pstr_exception 
        ({pext_kind = Pext_rebind ({txt = Longident.Lident n} as p2)} as p1)} as p0 -> (
      try (
        let n = Hashtbl.find exceptions_dict n in
        {p0 with pstr_desc = Pstr_exception
          {p1 with pext_kind = Pext_rebind {p2 with txt = Longident.Lident n}}}
      ) with | Not_found -> p0)
      | other -> other)
  );
}

(* load the translation tables *)
let init () = (
  let add_to_dict dict tuples =
    (List.iter (fun (zh, en) -> Hashtbl.add dict zh en) tuples) in
  add_to_dict exp_dict
  [("截断", "truncate");
   ("提出", "raise");
   ("浮点从字符串", "float_of_string");
   ("浮点从整数", "float_of_int");
   ("整数从字符串", "int_of_string");
   ("字符串从整数", "string_of_int");
   ("字符串从浮点", "string_of_float");
   ("开始输入", "open_in");
   ("输入行", "input_line");
   ("打印字符串", "print_string");
   ("第一", "fst");
   ("第二", "snd");
   ("前任", "pred"); 
   ("继承", "succ")];
  add_to_dict module_dict
  [("卜大数组", "Bigarray"); 
   ("卜列表", "List");
   ("卜随机", "Random");
   ("卜串", "Str");
   ("卜字符串", "String");
   ("卜系统", "Sys");
   ("Unix", "Unix");];
  add_to_dict types_dict
  [("整数", "int");
   ("浮点", "float");
   ("字符串", "string");];
  add_to_dict exceptions_dict
  [("卜失败", "Failure");
   ("卜文件结束", "End_of_file");];
	List.iter (fun (a, b, c) -> Hashtbl.add module_member_dict (a, b) c)
  [("Bigarray", "整数8_无符号", "int8_unsigned");
   ("Bigarray", "c布局", "c_layout");
   ("Random", "浮点" , "float");
   ("Str", "分裂", "split");
   ("Str", "正则表达式", "regexp");
   ("Sys", "参数向量", "argv");
   ("List", "过滤", "filter");
   ("List", "折叠左", "fold_left");
   ("List", "长度", "length");
   ("List", "映射", "map");
   ("List", "倒转", "rev");
   ("Unix", "时间_秒", "tm_sec");
   ("Unix", "时间_分钟", "tm_min");
   ("Unix", "时间_小时", "tm_hour");
	 ("Unix", "时间_星期日", "tm_wday");
   ("Unix", "时间_月日", "tm_mday");
   ("Unix", "时间_月", "tm_mon");
   ("Unix", "时间_年", "tm_year");
   ("Unix", "时间_年日", "tm_yday");
   ("Unix", "时间_是日光节约时间", "tm_isdst");
	 ("Unix", "做时间", "mktime");];
);;

let () = 
  init ();
  M.register "ppx_hanma" test_mapper;
;;

