open Core

let parse_and_print lexbuf =
  List.iter ~f:(fun value ->
      printf "%a\n" Syntax.output_tuple value;
    ) (Parser.prog Lexer.read lexbuf)

let loop filename () =
  let inx = In_channel.create filename in
  parse_and_print (Lexing.from_channel inx);
  In_channel.close inx

let () =
  Command.basic_spec ~summary:"Test"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
