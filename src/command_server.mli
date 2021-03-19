open! Core
open! Async

(** a connection to an hg command server *)
type t

module Ssh : sig
  type t = {
    host : string;
    user : string option;
    options : string list;
  }
end

(** [create addr] starts an hg command server at that location *)
val create
  :  ?env:Process.env
  -> ?hg_binary:string
  -> ?config:(string * string) list
  -> accepted_encodings:[ `Ascii | `Utf8 ] list
  -> Ssh.t option
  -> t Or_error.t Deferred.t

(** [destroy t] closes stdin on the hg process and waits for it to exit *)
val destroy : t -> unit Deferred.t

(** [run_command t ~cwd args] uses [t] to run "hg $args" in [cwd] *)
val run_command :
  t -> cwd:string -> string list -> Process.Output.t Or_error.t Deferred.t
