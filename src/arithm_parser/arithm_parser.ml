open Ostap
open Util
open Matcher
open Sll
open Str
open Parser
open Printf
open Arithm

let rec num_expression s = 
  expr (fun x -> x)
    [|
      `Lefta, [(ostap ("+")), (fun x y -> `GCall ("add", x, [y])); (ostap ("-")), (fun x y -> `FCall ("sub", [x; y]))]; 
      `Lefta, [(ostap ("*")), (fun x y -> `FCall ("mul", [x; y])); (ostap ("/")), (fun x y -> `FCall ("div", [x; y])); (ostap ("%")), (fun x y -> `FCall ("mod", [x; y]))]
    |]    
    primary
  s
and ostap (
  primary: 
   -"-" p:primary   {`GCall ("neg", p, [])}
  | -"(" num_expression -")"    
  | camlp5_sucks
  )
and camlp5_sucks x = expression num_expression x

ostap (
  arithm_term[expr_parser][declarations]:
      single_expr:expr_parser {
        let (fdefs, gdefs) = List.concat declarations |> defs_splitter  in
        make_program (Arithm.fdefs @ fdefs) (Arithm.gdefs @ gdefs) single_expr
      }
)

let rec arithm_expr_parser xs =  num_expression xs
let rec pure_decl_arithm_expr_parser = pure_decl arithm_expr_parser
let arithm_term_arithm_expr_parser = arithm_term arithm_expr_parser
let arithm_program_parser = program_parser pure_decl_arithm_expr_parser arithm_term_arithm_expr_parser

let parse source_text cont =
  Combinators.unwrap (arithm_program_parser (new lexer source_text))
    (fun program -> cont (resolve_gcalls program))
    (fun reason ->
      printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Desc reason))

let example =
    "add(Z, x) = x\n"
  ^ "add(S(x), y) = S(add(x, y))\n"
  ^ "f(x) = x + 3\n"
  ^ "g(x,y,z) = f(x+y) * (0 + z)"
  ^ ".\n"
  ^ "f(4)"

let verbose_test () =
  Combinators.unwrap (arithm_program_parser (new lexer example))
    (fun program ->
      printf "Parsed program:\n%s\n" (string_of_program string_of_pure program))
    (fun reason ->
      printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Desc reason))

let interpret_test () =
  Combinators.unwrap (arithm_program_parser (new lexer example))
    (fun program ->
       let result = Interpret.run (resolve_gcalls program) in
       printf "%s\n" (string_of_pure result))
    (fun _ -> ())


