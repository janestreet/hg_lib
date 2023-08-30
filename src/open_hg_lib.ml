module Command_server = Command_server
module Hg = Hg
include Hg_private.Public

(** The string that represents the tip revision. *)
let tip = "tip"

module Expert = struct
  module Hg_lib_factory = Hg_lib_factory
  module Hg_private = Hg_private
end
