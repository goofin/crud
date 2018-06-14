open Core
open Crud_ast
open Ir_xform_hashes

include Ir_xform_types

let check_duplicate_strings strings =
  let seen = StringHash.create () in
  List.iter strings ~f:(
    fun { Annotate.node = node; loc } ->
      match StringHash.find seen node with
      | Some prev_loc -> raise @@ Ir_error.Exn (Duplicate ("value", prev_loc, loc))
      | None -> StringHash.set seen node loc
  )

let rec find_and_xform_field defs t model field =
  match Ir_xform_defs.find_field defs (!model).Model.name field with
  | Field field -> xform_field defs t model field
  | Rel rel -> xform_rel defs t model rel

and find_and_xform_model defs t model =
  xform_model defs t (Ir_xform_defs.find_model defs model)

and xform_model defs t model =
  let { Syntax.Model.name = { node = name }; entries } = model in

  let model = lazy begin
    let entries_set = Ir_xform_dupes.create "model entry" in
    let fields_set = Ir_xform_dupes.create "field" in

    let model = ref
        { Model.name = name
        ; fields = StringHash.create ()
        ; table = None
        ; key = []
        ; unique = []
        ; index = []
        ; cruds = []
        } in
    StringHash.set t.models name model;

    List.iter entries ~f:(
      fun { Annotate.loc; node } ->
        match node with
        | Syntax.Model.Table table ->
          Ir_xform_dupes.check entries_set "table" loc;
          model := { !model with table = Some table.node }

        | Key key ->
          Ir_xform_dupes.check entries_set "key" loc;
          check_duplicate_strings key;
          let fields = List.map key (find_and_xform_field defs t model) in
          model := { !model with key = fields }

        | Unique unique ->
          check_duplicate_strings unique;
          let fields = List.map unique (find_and_xform_field defs t model) in
          model := { !model with unique = fields :: !model.unique }

        | Index index ->
          check_duplicate_strings index;
          let fields = List.map index (find_and_xform_field defs t model) in
          model := { !model with index = fields :: !model.index }

        | Field ({ name = { node = field_name } } as field) ->
          Ir_xform_dupes.check fields_set field_name loc;
          let field = xform_field defs t model field in
          StringHash.set !model.fields field_name field

        | Rel ({ name = { node = rel_name } } as rel) ->
          Ir_xform_dupes.check fields_set rel_name loc;
          let rel = xform_rel defs t model rel in
          StringHash.set !model.fields rel_name rel
    );

    model
  end in

  match StringHash.find t.models name with
  | Some model -> model
  | None -> force model

and xform_field defs t parent field =
  let { Syntax.Field.name = { node = name }; type_ = { node = type_ }; attrs } = field in

  let field = lazy begin
    let entries_set = Ir_xform_dupes.create "field attribute" in

    let field_ = ref
        { Field.parent = parent
        ; name
        ; type_
        ; column = None
        ; nullable = false
        ; updatable = false
        ; autoinsert = false
        ; autoupdate = false
        ; length = None
        } in
    let field = ref @@ Model.Field !field_ in
    FieldHash.set t.fields (!parent.name, name) field;

    List.iter attrs ~f:(
      fun { Annotate.loc; node } ->
        begin match node with
          | Syntax.Field.Column { node = col } ->
            Ir_xform_dupes.check entries_set "column" loc;
            field_ := { !field_ with column = Some col }

          | Nullable ->
            Ir_xform_dupes.check entries_set "nullable" loc;
            field_ := { !field_ with nullable = true }

          | Updatable ->
            Ir_xform_dupes.check entries_set "updatable" loc;
            field_ := { !field_ with updatable = true }

          | Autoinsert ->
            Ir_xform_dupes.check entries_set "autoinsert" loc;
            field_ := { !field_ with autoinsert = true }

          | Autoupdate ->
            Ir_xform_dupes.check entries_set "autoupdate" loc;
            field_ := { !field_ with autoupdate = true }

          | Length { node = length; loc } ->
            Ir_xform_dupes.check entries_set "length" loc;
            match Int.of_string length with
            | length -> field_ := { !field_ with length = Some length }
            | exception _ -> raise @@ Ir_error.Exn (Invalid loc)
        end;

        field := Field !field_;
    );

    field
  end in

  match FieldHash.find t.fields (!parent.name, name) with
  | Some field -> field
  | None -> force field

and xform_rel defs t parent rel =
  let { Syntax.Rel.name = { node = name }; model; field; kind = { node = kind }; attrs } = rel in

  let rel = lazy begin
    let entries_set = Ir_xform_dupes.create "relation attribute" in
    let model = find_and_xform_model defs t model in
    let field = find_and_xform_field defs t model field in

    let rel_ = ref
        { Rel.parent = parent
        ; name
        ; model
        ; field
        ; kind
        ; column = None
        ; nullable = false
        ; updatable = false
        } in
    let rel = ref @@ Model.Rel !rel_ in
    FieldHash.set t.fields (!parent.name, name) rel;

    List.iter attrs ~f:(
      fun { Annotate.loc; node } ->
        begin match node with
          | Syntax.Rel.Column { node = col } ->
            Ir_xform_dupes.check entries_set "column" loc;
            rel_ := { !rel_ with column = Some col }

          | Nullable ->
            (Ir_xform_dupes.check entries_set "nullable" loc;
             rel_ := { !rel_ with nullable = true })

          | Updatable ->
            (Ir_xform_dupes.check entries_set "updatable" loc;
             rel_ := { !rel_ with updatable = true })
        end;

        rel := Rel !rel_;
    );

    rel
  end in

  match FieldHash.find t.fields (!parent.name, name) with
  | Some rel -> rel
  | None -> force rel

and xform_crud defs t crud =
  let { Syntax.Crud.model; entries } = crud in
  let model = find_and_xform_model defs t model in

  let crud = lazy begin
    let crud = ref
        { Crud.model
        ; entries = []
        } in
    StringHash.set t.cruds !model.name crud;

    List.iter entries ~f:(
      fun { Annotate.loc; node } ->
        match node with
        | Syntax.Crud.Create create ->
          let create = xform_create defs t crud create in
          crud := { !crud with entries = create :: !crud.entries }

        | Read read ->
          let read = xform_read defs t crud read in
          crud := { !crud with entries = read :: !crud.entries }

        | Update update ->
          let update = xform_update defs t crud update in
          crud := { !crud with entries = update :: !crud.entries }

        | Delete delete ->
          let delete = xform_delete defs t crud delete in
          crud := { !crud with entries = delete :: !crud.entries }
    );

    crud
  end in

  match StringHash.find t.cruds !model.name with
  | Some crud -> crud
  | None -> force crud

and xform_query defs t model = function
  | Syntax.Query.Term term -> xform_query_term defs t model term

  | And { left_query = { node = left_query }; right_query = { node = right_query } } ->
    Query.And { b_left = xform_query defs t model left_query
              ; b_right = xform_query defs t model right_query
              }

  | Or { left_query = { node = left_query }; right_query = { node = right_query } } ->
    Query.Or { b_left = xform_query defs t model left_query
             ; b_right = xform_query defs t model right_query
             }

and xform_query_term defs t model term =
  let { Syntax.Query.left_val = { node = left_val }
      ; op = { node = op }
      ; right_val = { node = right_val }
      } = term in
  let left_val = xform_query_value defs t model left_val in
  let right_val = xform_query_value defs t model right_val in
  Query.Term { t_left = left_val
             ; t_op = op
             ; t_right = right_val
             }

and xform_query_value defs t model = function
  | Syntax.Query.Placeholder ->
    Query.Placeholder

  | Literal { node = literal } ->
    Literal literal

  | Call ({ node = fn }, { node = value }) ->
    Call (fn, xform_query_value defs t model value)

  | Field field ->
    let field = find_and_xform_field defs t model field in
    Field field

  | Join (model, { node = query }, field) ->
    let model = find_and_xform_model defs t model in
    let field = find_and_xform_field defs t model field in
    Join (model, xform_query defs t model query, field)

and xform_create defs t parent create =
  let { Syntax.Create.attrs } = create in
  let attrs_set = Ir_xform_dupes.create "attribute" in

  let create = ref { Create.parent
                   ; raw = false
                   ; suffix = None
                   } in

  List.iter attrs ~f:(
    fun { Annotate.loc; node } ->
      match node with
      | Syntax.Create.Raw ->
        Ir_xform_dupes.check attrs_set "raw" loc;
        create := { !create with raw = true }

      | Suffix { node = suffix } ->
        Ir_xform_dupes.check attrs_set "suffix" loc;
        create := { !create with suffix = Some suffix }
  );

  Crud.Create !create

and xform_read defs t parent read =
  let { Syntax.Read.kind = { node = kind }; query; attrs } = read in
  let attrs_set = Ir_xform_dupes.create "attribute" in
  let query = Option.map query ~f:(
      fun query -> xform_query defs t !parent.model query.node
    ) in

  let read = ref { Read.parent
                 ; kind
                 ; query
                 ; suffix = None
                 ; order_by = None
                 } in

  List.iter attrs ~f:(
    fun { Annotate.loc; node } ->
      match node with
      | Syntax.Read.Suffix { node = suffix } ->
        Ir_xform_dupes.check attrs_set "suffix" loc;
        read := { !read with suffix = Some suffix }

      | OrderBy direction ->
        Ir_xform_dupes.check attrs_set "orderby" loc;
        read := { !read with order_by = Some direction }
  );

  Crud.Read !read

and xform_update defs t parent update =
  let { Syntax.Update.attrs; query = { node = query } } = update in
  let attrs_set = Ir_xform_dupes.create "attribute" in
  let query = xform_query defs t !parent.model query in

  let update = ref { Update.parent
                   ; query
                   ; suffix = None
                   } in

  List.iter attrs ~f:(
    fun { Annotate.loc; node } ->
      match node with
      | Syntax.Update.Suffix { node = suffix } ->
        Ir_xform_dupes.check attrs_set "suffix" loc;
        update := { !update with suffix = Some suffix }
  );

  Crud.Update !update

and xform_delete defs t parent delete =
  let { Syntax.Delete.attrs; query = { node = query } } = delete in
  let attrs_set = Ir_xform_dupes.create "attribute" in
  let query = xform_query defs t !parent.model query in

  let delete = ref { Delete.parent
                   ; query
                   ; suffix = None
                   } in

  List.iter attrs ~f:(
    fun { Annotate.loc; node } ->
      match node with
      | Syntax.Delete.Suffix { node = suffix } ->
        Ir_xform_dupes.check attrs_set "suffix" loc;
        delete := { !delete with suffix = Some suffix }
  );

  Crud.Delete !delete


let xform_def defs t { Annotate.node = def } =
  match def with
  | Syntax.Model model -> ignore @@ xform_model defs t model
  | Syntax.Crud crud -> ignore @@ xform_crud defs t crud

let xform_defs defs =
  let t =
    { models = StringHash.create ()
    ; fields = FieldHash.create ()
    ; cruds = StringHash.create ()
    } in

  match List.iter defs ~f:(xform_def (Ir_xform_defs.create defs) t) with
  | () -> Ok t
  | exception Ir_error.Exn err -> Error err
