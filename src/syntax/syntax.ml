open Core
open Out_channel

type tuple
type other

type 'a value =
  | Ident : string           -> other value
  | List  : tuple value list -> other value
  | Tuple : other value list -> tuple value

let output_joined outc delim fn values =
  let last = List.length values - 1 in
  List.iteri ~f:(fun i value ->
    fn outc value;
    if i <> last then output_string outc delim;
  ) values

let rec output_other outc = function
  | Ident ident -> printf "%s" ident
  | List tuples ->
    output_string outc "(";
    output_joined outc ", " output_tuple tuples;
    output_string outc ")";

and output_tuple outc (Tuple others) =
  output_joined outc " " output_other others

let output_value (type any) outc (value: any value) =
  match value with
  | Ident _ -> output_other outc value
  | List _  -> output_other outc value
  | Tuple _ -> output_tuple outc value
