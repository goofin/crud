open Core;
open Crud_ast;
open Ir_xform_hashes;

let dprintf = (~depth=0) => {
  printf("%s", String.make(depth, '\t'));
  printf;
};

let print_field = (~depth=0, field) =>
  switch (field^) {
  | Ir_xform_transform.Model.Field(field) =>
    dprintf(~depth, "field: %s.%s\n", field.parent^.name, field.name)
  | Rel(rel) =>
    let field_name =
      switch (rel.field^) {
      | Field({name}) => name
      | Rel({name}) => name
      };
    dprintf(~depth, "rel: %s.%s\n", rel.parent^.name, rel.name);
    dprintf(~depth, "\tmodel: %s\n", rel.model^.name);
    dprintf(~depth, "\tfield: %s\n", field_name);
  };

let print_model = (~depth=0, model) => {
  dprintf(~depth, "model: %s\n", model^.Ir_xform_transform.Model.name);
  StringHash.iter(model^.fields, ~f=print_field(~depth=depth + 1));
  dprintf(~depth=depth + 1, "key:\n");
  List.iter(model^.key, ~f=print_field(~depth=depth + 2));
  List.iter(
    model^.unique,
    ~f=unique => {
      dprintf(~depth=depth + 1, "unique:\n");
      List.iter(unique, ~f=print_field(~depth=depth + 2));
    },
  );
  List.iter(
    model^.index,
    ~f=index => {
      dprintf(~depth=depth + 1, "index:\n");
      List.iter(index, ~f=print_field(~depth=depth + 2));
    },
  );
};

let print = ({Ir_xform_transform.models, fields}) => StringHash.iter(models, ~f=print_model);
