(* sys4dc.ml *)

open Base
open Cmdliner
open Decompiler

let rec mkdir_p path =
  if not (Stdlib.Sys.file_exists path) then (
    let parent = Stdlib.Filename.dirname path in
    if not (String.equal parent path) then mkdir_p parent;
    Stdlib.Sys.mkdir path 0o755)
  else if not (Stdlib.Sys.is_directory path) then
    failwith (path ^ " exists but is not a directory")

let output_printer_getter out_dir fname f =
  if String.(out_dir = "-") then (
    Stdio.printf "FILE %s\n\n" fname;
    f (CodeGen.create_printer Stdio.stdout ""))
  else
    let fname_components = String.split fname ~on:'\\' in
    let unix_fname = String.concat ~sep:"/" fname_components in
    let output_path = Stdlib.Filename.concat out_dir unix_fname in
    mkdir_p (Stdlib.Filename.dirname output_path);
    let outc = Stdio.Out_channel.create output_path in
    f (CodeGen.create_printer outc unix_fname);
    Out_channel.close outc

(* BEGIN: 新增代码 - 导出汇编指令的核心逻辑 *)

(* --- 辅助函数：安全地从数组获取元素 --- *)
let safe_get arr index =
  if index >= 0 && index < Array.length arr then Some arr.(index) else None

(* --- 辅助函数：将ID解析为名称 --- *)
(* in bin/sys4dc.ml *)

(* ... (上面的代码不变) ... *)

let string_of_instruction_with_context (current_func: CodeSection.function_t) (instr: Instructions.instruction) =
  let open Instructions in
  
  let func_name_and_args id =
    match safe_get Ain.ain.func id with
    | Some f -> 
        let arg_count = List.count (Ain.Function.arg_types f) ~f:(fun t -> not (Poly.(t = Type.Void))) in
        Printf.sprintf "%s (args: %d)" f.name arg_count
    | None -> Printf.sprintf "<invalid_func_%d>" id
  in
  let hll_name_and_args lib_id func_id =
    match safe_get Ain.ain.hll0 lib_id with
    | Some lib -> (
      match safe_get lib.functions func_id with
      | Some f -> 
        let arg_count = Array.length f.arguments in
        Printf.sprintf "%s.%s (args: %d)" lib.name f.name arg_count
      | None -> Printf.sprintf "%s.<invalid_hll_func_%d>" lib.name func_id
    )
    | None -> Printf.sprintf "<invalid_hll_lib_%d>.%d" lib_id func_id
  in
  let syscall_name_and_args id =
      match safe_get syscalls id with
      | Some s -> 
        let arg_count = List.length s.arg_types in
        Printf.sprintf "%s (args: %d)" s.name arg_count
      | None -> Printf.sprintf "<invalid_syscall_%d>" id
  in
  let global_name id =
    match safe_get Ain.ain.glob id with
    | Some g -> g.name
    | None -> Printf.sprintf "<invalid_global_%d>" id
  in
  let local_name id =
    match safe_get current_func.func.vars id with
    | Some v -> v.name
    | None -> Printf.sprintf "<invalid_local_%d>" id
  in
  let struct_name id =
    match safe_get Ain.ain.strt id with
    | Some s -> s.name
    | None -> Printf.sprintf "<invalid_struct_%d>" id
  in
  let member_name struc_id member_id =
    match safe_get Ain.ain.strt struc_id with
    | Some s -> (
        match safe_get s.members member_id with
        | Some m -> Printf.sprintf "%s.%s" s.name m.name
        | None -> Printf.sprintf "%s.<invalid_member_%d>" s.name member_id
      )
    | None -> Printf.sprintf "<invalid_struct_%d>.%d" struc_id member_id
  in
  let escape_simple s quote_char =
    s
    |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
    |> String.substr_replace_all ~pattern:(String.of_char quote_char) ~with_:("\\" ^ (String.of_char quote_char))
    |> String.substr_replace_all ~pattern:"\n" ~with_:"\\n"
    |> String.substr_replace_all ~pattern:"\r" ~with_:"\\r"
    |> String.substr_replace_all ~pattern:"\t" ~with_:"\\t"
  in
  let string_literal id =
    let table = if Ain.ain.vers = 0 then Ain.ain.msg else Ain.ain.str0 in
    match safe_get table id with
    | Some s ->
      let s_short = if String.length s > 30 then String.sub s ~pos:0 ~len:27 ^ "..." else s in
      Printf.sprintf "\"%s\"" (escape_simple s_short '"')
    | None -> Printf.sprintf "<invalid_string_%d>" id
  in
  let msg_literal id =
    match safe_get Ain.ain.msg id with
    | Some s -> Printf.sprintf "'%s'" (escape_simple s '\'')
    | None -> Printf.sprintf "<invalid_msg_%d>" id
  in

  match instr with
  | CALLFUNC id -> Printf.sprintf "CALLFUNC %-25s" (func_name_and_args id)
  | CALLHLL (lib, func, _) -> Printf.sprintf "CALLHLL %-25s" (hll_name_and_args lib func)
  | CALLMETHOD id -> Printf.sprintf "CALLMETHOD %d" id
  | CALLSYS id -> Printf.sprintf "CALLSYS %-25s" (syscall_name_and_args id)
  | SH_GLOBALREF id -> Printf.sprintf "SH_GLOBALREF %s" (global_name id)
  | SH_LOCALREF id -> Printf.sprintf "SH_LOCALREF %s" (local_name id)
  | SH_STRUCTREF id ->
      let name = match current_func.struc with
                 | Some s -> member_name s.id id
                 | None -> Printf.sprintf "<unknown_struct>.%d" id in
      Printf.sprintf "SH_STRUCTREF %s" name
  | SR_REF id -> Printf.sprintf "SR_REF %s" (struct_name id)
  | S_PUSH id -> Printf.sprintf "S_PUSH %d (%s)" id (string_literal id)
  | S_REF2 id -> Printf.sprintf "S_REF2 %ld (%s)" id (string_literal (Int32.to_int_exn id))
  | MSG id -> Printf.sprintf "MSG %d (%s)" id (msg_literal id)
  | NEW (struct_id, func_id) -> Printf.sprintf "NEW %s (%s)" (struct_name struct_id) (if func_id = -1 then "default_ctor" else func_name_and_args func_id)
  | PUSH value -> Printf.sprintf "PUSH %ld" value
  | F_PUSH value -> Printf.sprintf "F_PUSH %f" value
  | JUMP addr -> Printf.sprintf "JUMP 0x%x" addr
  | IFZ addr -> Printf.sprintf "IFZ 0x%x" addr
  | IFNZ addr -> Printf.sprintf "IFNZ 0x%x" addr
  | _ -> show_instruction instr

let stack_effect (instr: Instructions.instruction) : int =
  let open Instructions in
  match instr with
  | PUSH _ | F_PUSH _ | S_PUSH _ | PUSHLOCALPAGE | PUSHGLOBALPAGE | PUSHSTRUCTPAGE
  | DUP -> 1
  | DUP2 -> 2
  
  | POP | S_POP | SR_POP -> -1
  
  | CALLFUNC id -> (Option.value_map (safe_get Ain.ain.func id) ~default:0 ~f:(fun f ->
    let arg_count = List.count (Ain.Function.arg_types f) ~f:(fun t -> not (Poly.(t = Type.Void))) in
    (if Poly.(f.return_type = Type.Void) then 0 else 1) - arg_count))
  | CALLMETHOD _ -> -2 
  | CALLSYS id -> (Option.value_map (safe_get syscalls id) ~default:0 ~f:(fun s ->
    let arg_count = List.count s.arg_types ~f:(fun t -> not (Poly.(t = Type.Void))) in
    (if Poly.(s.return_type = Type.Void) then 0 else 1) - arg_count))
        
  | REF -> -1 (* page + slot -> value *)
  | REFREF -> -1 (* page + slot -> ref_value *)

  | ADD | SUB | MUL | DIV | MOD | AND | OR | XOR | LSHIFT | RSHIFT
  | LT | GT | LTE | GTE | NOTE | EQUALE
  | F_ADD | F_SUB | F_MUL | F_DIV | F_LT | F_GT | F_LTE | F_GTE | F_NOTE | F_EQUALE
  | S_ADD | S_NOTE | S_EQUALE | S_LT | S_GT | S_LTE | S_GTE -> -1

  | ASSIGN | PLUSA | MINUSA | MULA | DIVA | MODA | ANDA | ORA | XORA | LSHIFTA | RSHIFTA
  | F_ASSIGN | F_PLUSA | F_MINUSA | F_MULA | F_DIVA | S_ASSIGN | S_PLUSA -> -2

  | IFZ _ | IFNZ _ -> -1
  
  | _ -> 0 (* 默认保守估计，也可以根据指令集完善 *)

let rec print_asm_function pr indent (f : CodeSection.function_t) =
  let open CodeGen in
  
  let start_addr =
    match f.code with [] -> f.end_addr | { addr; _ } :: _ -> addr
  in
  let indent_str = String.make (indent * 4) ' ' in
  
  println pr "%s; FUNC %s (0x%x-0x%x)" indent_str f.func.name start_addr f.end_addr;

  let stack_depth = ref 0 in
  List.iter f.code ~f:(fun { addr; txt; _ } ->
    let stack_effect_value = stack_effect txt in
    let new_depth = !stack_depth + stack_effect_value in
    let display_effect = if stack_effect_value > 0 then Printf.sprintf "+%d" stack_effect_value else Printf.sprintf "%d" stack_effect_value in

    println pr "%s/* %08x */ %-40s ; stack: %2d -> %2d (%s)"
      indent_str
      addr
      (string_of_instruction_with_context f txt)
      !stack_depth
      new_depth
      display_effect;

    stack_depth := new_depth
  );
  
  if not (List.is_empty f.lambdas) then (
    print_newline pr;
    List.iter f.lambdas ~f:(fun lambda ->
        print_asm_function pr (indent + 1) lambda;
        print_newline pr));
  
  println pr "%s; ENDFUNC %s" indent_str f.func.name;
  ()

let export_asm files ain_path output_to_printer =
  let open CodeGen in
  let asm_sources = ref [] in
  let output_source fname f =
    asm_sources := fname :: !asm_sources;
    output_to_printer fname f
  in
  
  List.iter files ~f:(fun (fname, funcs) ->
    if not (List.is_empty funcs) then
      let asm_fname = 
        if String.is_suffix fname ~suffix:".jaf" then
          String.substr_replace_all fname ~pattern:".jaf" ~with_:".asm"
        else
          fname ^ ".asm"
      in
      output_source asm_fname (fun pr ->
        List.iter funcs ~f:(fun func ->
          print_asm_function pr 0 func;
          print_newline pr))
  );

  let project_name = Stdlib.Filename.(remove_extension @@ basename ain_path) in
  let code_for_version_check = Instructions.decode Ain.ain.code in
  let ain_minor_version = Decompile.determine_ain_minor_version code_for_version_check in
  
  output_source "main.inc" (fun pr ->
      println pr "Source = {";
      List.iter (List.rev !asm_sources) ~f:(fun src -> println pr "\t\"%s\"," src);
      println pr "}");

  let project : project_t =
    {
      name = project_name;
      output_dir = Stdlib.Filename.dirname ain_path;
      ain_minor_version = ain_minor_version;
    }
  in
  output_to_printer (project.name ^ ".pje") (fun pr ->
      print_pje pr project)
(* END: 新增代码 *)


let sys4dc output_dir inspect_function print_addr move_to_original_file dump_asm ain_file
    =
  let output_dir = Option.value output_dir ~default:"." in
  Ain.load ain_file;

  let ain_path =
    Fpath.(
      let root =
        v @@ if String.(output_dir = "-") then "." else output_dir
      in
      match relativize ~root (v ain_file) with
      | Some p -> to_string @@ normalize p
      | None -> ain_file)
  in

  if dump_asm then
    let code = Instructions.decode Ain.ain.code in
    let code = CodeSection.preprocess_ain_v0 code in
    let files =
      CodeSection.parse code
      |> CodeSection.remove_overridden_functions ~move_to_original_file
      |> CodeSection.fix_or_remove_known_broken_functions
    in
    export_asm files ain_path (output_printer_getter output_dir)
  else
    match inspect_function with
    | None ->
        let decompiled = Decompile.decompile move_to_original_file in
        Decompile.export decompiled ain_path
          (output_printer_getter output_dir)
          ~print_addr
    | Some funcname -> 
        Decompile.inspect funcname ~print_addr

let cmd =
  let doc = "Decompile an .ain file" in
  let info = Cmd.info "sys4dc" ~doc in
  let output_dir =
    let doc = "Output directory. Use '-' to print everything to stdout." in
    let docv = "DIRECTORY" in
    Cmdliner.Arg.(value & opt (some string) None & info [ "o" ] ~docv ~doc)
  in
  let inspect_function =
    let doc = "Inspect the decompilation process of a function" in
    let docv = "FUNCTION" in
    Cmdliner.Arg.(
      value & opt (some string) None & info [ "inspect" ] ~docv ~doc)
  in
  let print_addr =
    let doc = "Print addresses in decompiled code" in
    Cmdliner.Arg.(value & flag & info [ "address" ] ~doc)
  in
  let move_to_original_file =
    let doc =
      "Move the overridden functions to the files where they were originally \
       defined. Useful for mods made with AinDecompiler."
    in
    Cmdliner.Arg.(value & flag & info [ "move-to-original-file" ] ~doc)
  in
  let dump_asm =
    let doc = "Dump VM instructions with addresses instead of decompiling." in
    Cmdliner.Arg.(value & flag & info [ "dump-asm" ] ~doc)
  in
  let ain_file =
    let doc = "The .ain file to decompile" in
    let docv = "AIN_FILE" in
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)
  in
  Cmd.v info
    Term.(
      const sys4dc $ output_dir $ inspect_function $ print_addr
      $ move_to_original_file $ dump_asm $ ain_file)

let () = Stdlib.exit (Cmd.eval cmd)