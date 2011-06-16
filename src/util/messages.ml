open Cil
open Pretty
module GU = Goblintutil


exception Bailure of string
let bailwith s = raise (Bailure s)

let tracing = false  (* Hopefully, when set to false optimizations will kick in *)
let warnings = ref false
let soundness = ref true
let warn_out = ref stdout

let get_out name alternative = match !GU.dump_path with
  | Some path -> open_out (Filename.concat path (name ^ ".out"))
  | _ -> alternative

let current_loc = GU.current_loc

let print_msg msg loc = 
  if !GU.gccwarn then    
    Printf.printf "%s:%d:0: warning: %s\n" loc.file loc.line msg
  else if !Goblintutil.eclipse then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line

let print_err msg loc = 
  if !GU.gccwarn then    
    Printf.printf "%s:%d:0: error: %s\n" loc.file loc.line msg
  else if !Goblintutil.eclipse then 
    Printf.printf "WARNING /-/ %s /-/ %d /-/ %s\n%!" loc.file loc.line msg
  else
    Printf.fprintf !warn_out "%s (%s:%d)\n%!" msg loc.file loc.line


let print_group group_name errors =
  if !Goblintutil.eclipse then
    List.iter (fun (msg,loc) -> print_msg (group_name ^ ", " ^ msg) loc) errors
  else
    let f (msg,loc): doc = Pretty.dprintf "%s (%s:%d)" msg loc.file loc.line in
      ignore (Pretty.fprintf !warn_out "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)

let warn_urgent msg = 
  if not !GU.may_narrow then begin
    soundness := false;
    print_msg msg (!current_loc)
  end
  
let write msg =
  print_msg msg !current_loc

let warn_all msg = 
  if not !GU.may_narrow then begin
    if !warnings then 
      print_msg msg (!current_loc);
    soundness := false
  end
  
let worldStopped = ref false
exception StopTheWorld
let waitWhat s = 
  worldStopped := true;
  warn_urgent s;
  raise StopTheWorld
  
let report_lin_hashtbl  = Hashtbl.create 10

let report msg = 
  if not !GU.may_narrow then begin
    let loc = !current_loc in
    if (Hashtbl.mem report_lin_hashtbl (msg,loc) == false) then
      begin
        print_msg msg loc;
        Hashtbl.add report_lin_hashtbl (msg,loc) true
      end
  end

let report_error msg = 
  if not !GU.may_narrow then begin
    let loc = !current_loc in
		  print_err msg loc
  end	
		  
let warn_str_hashtbl = Hashtbl.create 10
let warn_lin_hashtbl = Hashtbl.create 10

let warn msg = 
  if not !GU.may_narrow then begin
    if (Hashtbl.mem warn_str_hashtbl msg == false) then
      begin
        warn_all msg;
        Hashtbl.add warn_str_hashtbl msg true
      end
  end
  
let warn_each msg = 
  if not !GU.may_narrow then begin
    let loc = !current_loc in
      if (Hashtbl.mem warn_lin_hashtbl (msg,loc) == false) then
        begin
        warn_all msg;
        Hashtbl.add warn_lin_hashtbl (msg,loc) true
        end
  end
  
let debug msg =
  if !GU.debug then warn msg


(* Parses a format string to generate a nop-function of the correct type. *)
let mygprintf (format : ('a, unit, doc, 'b) format4) : 'a =
  let format = (Obj.magic format : string) in
  let flen    = String.length format in
  let fget    = String.unsafe_get format in
  let rec literal acc i = 
    let rec skipChars j = 
      if j >= flen || (match fget j with '%' | '@' | '\n' -> true | _ -> false) then
        collect nil j
      else
        skipChars (succ j)
    in
    skipChars (succ i)
  and collect (acc: doc) (i: int) = 
    if i >= flen then begin
      Obj.magic (()) 
    end else begin
      let c = fget i in
      if c = '%' then begin
        let j = skip_args (succ i) in
        match fget j with
          '%' -> literal acc j 
	| ',' -> collect acc (succ j)
        | 's' | 'c' | 'd' | 'i' | 'o' | 'x' | 'X' | 'u'
        | 'f' | 'e' | 'E' | 'g' | 'G' | 'b' | 'B' -> 
            Obj.magic(fun b -> collect nil (succ j))
	| 'L' | 'l' | 'n' -> Obj.magic(fun n -> collect nil (succ (succ j)))
        | 'a' -> Obj.magic(fun pprinter arg -> collect nil (succ j))
        | 't' -> Obj.magic(fun pprinter -> collect nil (succ j))
        | c -> invalid_arg ("dprintf: unknown format %s" ^ String.make 1 c)
      end else if c = '@' then begin
        if i + 1 < flen then begin
          match fget (succ i) with
            '[' | ']' | '!' | '?' | '^' | '@' -> collect nil (i + 2)
          | '<' | '>' -> collect nil (i + 1)
          | c -> invalid_arg ("dprintf: unknown format @" ^ String.make 1 c)
        end else
          invalid_arg "dprintf: incomplete format @"
      end else if c = '\n' then begin
        collect nil (i + 1)
      end else
        literal acc i
    end
  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j
  in
  collect nil 0

let traceTag (sys : string) : Pretty.doc =
  let rec ind (i : int) : string = if (i <= 0) then "" else " " ^ (ind (i-1)) in
    (text ((ind !Trace.traceIndentLevel) ^ "%%% " ^ sys ^ ": "))

let printtrace sys d: unit = 
  fprint stderr 80 ((traceTag sys) ++ align ++ d ++ unalign); 
  flush stderr 

let gtrace f sys fmt = 
  if Trace.traceActive sys then
    gprintf (f sys) fmt
  else
    mygprintf fmt

let trace sys fmt = gtrace printtrace sys fmt

let tracei sys fmt =  
  let f sys d = printtrace sys d; Trace.traceIndent sys in
    gtrace f sys fmt

let traceu sys fmt =  
  let f sys d = printtrace sys d; Trace.traceOutdent sys in
    gtrace f sys fmt

let tracel sys fmt = 
  let loc = !current_loc in
  let docloc sys doc = 
    printtrace sys (text loc.file ++ text ":" ++ num loc.line ++ line ++ indent 2 doc) 
  in
    gtrace docloc sys fmt

let traceli sys fmt = 
  let loc = !current_loc in
  let docloc sys doc: unit = 
    printtrace sys (text loc.file ++ text ":" ++ num loc.line ++ line ++ indent 2 doc);
    Trace.traceIndent sys
  in
    gtrace docloc sys fmt
