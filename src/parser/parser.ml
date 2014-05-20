open Printf
open Ostap
open Util
open Matcher
open Sll

let ident = ostap (n:IDENT {Token.repr n})
let cnt = ostap (c:CNT {Token.repr c})
let const = ostap (c:CONST {int_of_string (Token.repr c)})

let rec make_nat = function
  | 0 -> `Ctr ("Z", [])
  | n -> `Ctr ("S", [make_nat (n - 1)])

let make_int z =
  if z < 0
  then `Ctr ("N", [make_nat (-z)])
  else (make_nat z)

let list_of_opt = function
  | Some xs -> xs
  | None    -> []

let defs_splitter xs =
  let rec helper defs fdefs gdefs =
    match defs with
    | (`FRule (fn, b) :: smth)     -> helper smth ((fn, b) :: fdefs) gdefs
    | (`GRule (gn, pn, b) :: smth) -> helper smth fdefs ((gn, pn, b) :: gdefs)
    | []                           -> (fdefs, gdefs)
  in helper xs [] []


ostap (
  program_parser[decl_parser][term_parser]:
    decls:(decl_parser)* -"." parsed_program:term_parser[decls] { parsed_program }
)
    

(*pure SLL expression parser "declarations" is a list of declarations
       (each decl is a list itself, so "declarations" is a list of lists)
      that were met before dot*)
ostap (
  pure_term[expr_parser][declarations]:
      single_expr:expr_parser {
        let (fdefs, gdefs) = List.concat declarations |> defs_splitter  in
        make_program fdefs gdefs single_expr
      }
)

(*pure SLL f and g functions definition*)
ostap (
  f_rule[expr_parser]:
    name:ident -"(" fargs:list0[ident] -")" -"=" body:expr_parser
    { [`FRule (name >$ fargs >= body)] }
  ;
  g_rule[expr_parser]:
    name:ident -"(" pname:cnt pargs:ident_ctr_args gargs:(-"," ident)* -")"
        -"=" gbody:expr_parser
    { [`GRule (name $ (pname +> pargs, gargs) => gbody)] }
  ;
  ident_ctr_args:
    pargs:(-"(" list0[ident] -")")? { list_of_opt pargs }
)

(*pure SLL function declaration*)

ostap (
  pure_decl[expr_parser]:
    f_rule[expr_parser] | g_rule[expr_parser]
)
 

ostap (
  expression[expr_parser]:
      constructor[expr_parser]
    | fun_call[expr_parser]
    | v:ident  { `Var v }
    | c:const           {make_int c} 
  ;
  args_list[expr_parser]: -"(" list0[expr_parser] -")"
  ;
  constructor[expr_parser]:
    cname:cnt  args:args_list[expr_parser]?
    { `Ctr cname (list_of_opt args) }
  ;
  fun_call[expr_parser]:
    name:ident args:args_list[expr_parser]
    { `FCall (name, args) }
)


let rec pure_expr_parser xs = expression pure_expr_parser xs
let rec pure_decl_pure_expr_parser = pure_decl pure_expr_parser
let pure_term_pure_expr_parser = pure_term pure_expr_parser
let pure_program_parser = program_parser pure_decl_pure_expr_parser pure_term_pure_expr_parser

(*
ostap (
  branch_parser[expr_parser][case_num]: 
      -"|" pat:constructor[expr_parser] -"->" body:expr_parser  {`DGdef ("caseOfFun" ^ string_of_int(case_num)
                                                                   , pat, body)}
  case_of_parser[decls][expr_parser][case_num]:
    -"case" expr:expr_parser -"of" bs:branch_parser[expr_parser][case_num + 1]+ { 
  *)   

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let ident = Str.regexp "[a-z][a-zA-Z0-9]*" in
  let const = Str.regexp "[0-9]+" in
  let cnt = Str.regexp "[A-Z][a-zA-Z0-9]*" in
  object (self)

    inherit t s

    method skip p c = skip s p c
    method getIDENT = self#get "identifier" ident
    method getCONST = self#get "constant" const
    method getCNT   = self#get "constructor" cnt

  end

let resolve_gcalls { fdefs = fdefs; gdefs = gdefs; term = term; } =
  let rec resolve_expr = function
    | `FCall (name, parg :: args) when not (Ident_map.mem name fdefs) ->
        `GCall (name, resolve_expr parg, List.map resolve_expr args)
    | `FCall (fname, fargs) -> `FCall (fname, List.map resolve_expr fargs)
    | `GCall (gname, pname, gargs) -> `GCall (gname, pname, gargs)
    | `Ctr (cname, cargs) -> `Ctr (cname, List.map resolve_expr cargs)
    | `Var name -> `Var name
  in
  let resolved_fdefs = Ident_map.map (fun { fargs = fargs; fbody = fbody; } ->
    { fargs = fargs; fbody = resolve_expr fbody } ) fdefs
  in
  let resolved_gdefs = Ident_map.map (fun gpdefs ->
    Ident_map.map (fun { pargs = pargs; gargs = gargs; gbody = gbody; } ->
      { pargs = pargs; gargs = gargs; gbody = resolve_expr gbody; })
      gpdefs) gdefs
  in
  { fdefs = resolved_fdefs; gdefs = resolved_gdefs; term = resolve_expr term; }

let parse source_text cont =
  Combinators.unwrap (pure_program_parser (new lexer source_text))
    (fun program -> cont (resolve_gcalls program))
    (fun reason ->
      printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Desc reason))

let example =
    "add(Z, x) = x\n"
  ^ "add(S(x), y) = S(add(x, y))\n"
  ^ ".\n"
  ^ "add(S(Z), S(S(Z)))"

let big_example = string_of_program string_of_pure Arithm.program

let verbose_test () =
  Combinators.unwrap (pure_program_parser (new lexer big_example))
    (fun program ->
      printf "Parsed program:\n%s\n" (string_of_program string_of_pure program))
    (fun reason ->
      printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Desc reason))

let big_test () =
  Combinators.unwrap (pure_program_parser (new lexer big_example))
    (fun program ->
       let result = Interpret.run (resolve_gcalls program) in
       printf "%s\n" (string_of_pure result))
    (fun _ -> ())
