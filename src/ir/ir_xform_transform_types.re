open Core;
open Crud_ast;
open Ir_xform_hashes;

type field =
  | Field(field_data)
  | Rel(rel_data)
and field_data = {
  f_parent: ref(model),
  f_name: string,
  f_type: Syntax.Model.Field.type_,
  f_column: option(string),
  f_nullable: bool,
  f_updatable: bool,
  f_autoinsert: bool,
  f_autoupdate: bool,
  f_length: option(int),
}
and rel_data = {
  r_parent: ref(model),
  r_name: string,
  r_model: ref(model),
  r_field: ref(field),
  r_kind: Syntax.Model.Rel.kind,
  r_column: option(string),
  r_nullable: bool,
  r_updatable: bool,
}
and model = {
  m_name: string,
  m_fields: StringHash.t(ref(field)),
  m_table: option(string),
  m_key: list(ref(field)),
  m_unique: list(list(ref(field))),
  m_index: list(list(ref(field))),
};

type t = {
  models: StringHash.t(ref(model)),
  fields: FieldHash.t(ref(field)),
};
