module Defs = Ir_xform_defs
module Dupes = Ir_xform_dupes
module Hashes = Ir_xform_hashes
module Transform = Ir_xform_transform

type t = Transform.t

let transform_defs = Transform.xform_defs
