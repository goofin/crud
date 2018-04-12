open Core
open Out_channel

type tuple
type other

type 'a value =
  | Ident : string           -> other value
  | List  : tuple value list -> other value
  | Tuple : other value list -> tuple value

let rec output_other outc = function
  | Ident ident -> printf "%s" ident
  | List tuples ->
    let last = List.length tuples - 1 in
    output_string outc "(";
    List.iteri ~f:(fun i tuple ->
        output_tuple outc tuple;
        if i <> last then output_string outc ", ";
      ) tuples;
    output_string outc ")";


and output_tuple outc (Tuple idents_or_lists) =
  let last = List.length idents_or_lists - 1 in
  List.iteri ~f:(fun i value ->
      output_other outc value;
      if i <> last then output_string outc " ";
    ) idents_or_lists;
;;

let output_value (type any) outc (value: any value) =
  match value with
  | Ident _ -> output_other outc value
  | List _  -> output_other outc value
  | Tuple _ -> output_tuple outc value
