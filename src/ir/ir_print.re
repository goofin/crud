open Core;
open Crud_ast;
open Ir_xform_hashes;

let dprintf = (~depth=0) => {
  printf("%s", String.make(depth, '\t'));
  printf;
};

let print_field = (~depth=0, field) =>
  switch (field^) {
  | Ir_xform_transform.Field(field) =>
    dprintf(~depth, "field: %s.%s\n", field.f_parent^.m_name, field.f_name)
  | Rel(rel) =>
    let field_name =
      switch (rel.r_field^) {
      | Field({f_name}) => f_name
      | Rel({r_name}) => r_name
      };
    dprintf(~depth, "rel: %s.%s\n", rel.r_parent^.m_name, rel.r_name);
    dprintf(~depth, "\tmodel: %s\n", rel.r_model^.m_name);
    dprintf(~depth, "\tfield: %s\n", field_name);
  };

let print_model = (~depth=0, model) => {
  dprintf(~depth, "model: %s\n", model^.Ir_xform_transform.m_name);
  StringHash.iter(model^.m_fields, ~f=print_field(~depth=depth + 1));
  dprintf(~depth=depth + 1, "key:\n");
  List.iter(model^.m_key, ~f=print_field(~depth=depth + 2));
  List.iter(
    model^.m_unique,
    ~f=unique => {
      dprintf(~depth=depth + 1, "unique:\n");
      List.iter(unique, ~f=print_field(~depth=depth + 2));
    },
  );
  List.iter(
    model^.m_index,
    ~f=index => {
      dprintf(~depth=depth + 1, "index:\n");
      List.iter(index, ~f=print_field(~depth=depth + 2));
    },
  );
};

let print = ({Ir_xform_transform.models, fields}) => {
  StringHash.iter(models, ~f=print_model);
  FieldHash.iter(fields, ~f=print_field);
};
