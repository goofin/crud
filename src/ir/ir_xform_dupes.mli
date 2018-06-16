type t

val create : string -> t
val check : t -> string -> Crud_ast.Annotate.location -> unit
