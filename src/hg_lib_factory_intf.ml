open Hg_private
open Async

module type Arg = sig
  (** A type that is intended to be used to add additional arguments to every function.
      E.g. the provided default uses this type to represent flags that can be passed to
      any hg command, like "--cwd". *)
  module With_args : sig
    type 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  (** The output type of an hg call, such as [Deferred.t], [Or_error.t], etc. *)
  module Output : sig
    type 'a t

    val return : 'a -> 'a t
  end

  (** [run] should be a function that runs hg with the command line arguments [args].

      The [handle_output] function passed to [run] will be a function that can parse the
      output of the particular hg command being run -- for example, `hg push` exits with a
      different status depending on whether there are any changesets to push, so the
      [handle_output] function provided for the "push" commands will recognize which exit
      codes correspond to [`Ok] and which ones correspond to [`Nothing_to_push].

      Giving [run] this type, rather than a type that just returns a [Process.Output.t],
      makes it more flexible and allows it to supply more complete information in error
      cases. For example, if [run] adds additional arguments (as happens in the top-level
      instantiation of this functor), [run] can add them to the error provided by
      [handle_output].

      It's not even necessary for [run] to call [handle_output], if the type ['a Output.t]
      doesn't reference ['a]. An example of this is running hg in the foreground with
      [Unix.fork_exec] and [Unix.waitpid], and using [unit Deferred.t] for the output
      type. *)
  val run
    : (args:string list
       -> handle_output:(Process.Output.t -> 'a Or_simple_error.t)
       -> unit
       -> 'a Output.t)
        With_args.t
end

module type Make_s = functor (_ : Arg) -> sig
  module type S
end

module type Hg_env = sig
  val hg_binary : string
  val hgrc_path : string
  val hg_user : string Lazy.t
  val hg_config_options : (string * string) list
end

type 'a with_global_args =
  ?repository:string
  -> ?cwd:string
  -> ?config:(string * string) list
  -> ?env:Process.env
  -> 'a

type 'a with_global_args_remote =
  server:Command_server.t
  -> ?repository:string
  -> ?cwd:string
  -> ?config:(string * string) list
  -> 'a

module type Hg_lib_factory = sig
  module type Arg = Arg
  module type Hg_env = Hg_env

  type nonrec 'a with_global_args = 'a with_global_args
  type nonrec 'a with_global_args_remote = 'a with_global_args_remote

  module Simple :
    Arg with type 'a With_args.t = 'a with_global_args with type 'a Output.t = 'a

  module Async :
    Arg
    with type 'a With_args.t = 'a with_global_args
    with type 'a Output.t = 'a Deferred.Or_error.t

  (** Same as Async, but with the following changes to fix the hg environment:
      - hardwire a particular version of hg as stated by [hg_binary]
      - set [HGUSER] to [hg_user]
      - set [HGRCPATH] to [hgrc_path]. hg will now only load this file and the [.hg/hgrc]
        for the repo. *)
  module Fixed_hg_environment (_ : Hg_env) :
    Arg
    with type 'a With_args.t = 'a with_global_args
    with type 'a Output.t = 'a Deferred.Or_error.t

  module Remote :
    Arg
    with type 'a With_args.t = 'a with_global_args_remote
    with type 'a Output.t = 'a Deferred.Or_error.t

  (** To satisfy this functor, define a signature [S] for your hg library with respect to
      the abstract type constructors ['a with_args] and ['a output]. Then generate the
      interface for your library as follows:

      {[
        module Make_s (A : Hg_lib_factory.Arg) = struct
          module type S =
            S
            with type 'a with_args := 'a A.With_args.t
            with type 'a output := 'a A.Output.t
        end

        module type Hg = Hg_lib_factory.Make_lib(Make_s).S
      ]}

      This is necessary because a module type passed to a functor must either be fully
      abstract or fully concrete -- you can't say the functor input has a module type [S]
      which has types ['a with_args] and ['a output] unless you fully specify [S]. We want
      [S] to be different for different callers, so we have to do this workaround. *)
  module Make_lib (M : Make_s) : sig
    module type S = sig
      module Make (A : Arg) : M(A).S
      module Simple : M(Simple).S
      module Async : M(Async).S
      module Fixed_hg_environment (E : Hg_env) : M(Fixed_hg_environment(E)).S
      module Remote : M(Remote).S
    end
  end
end
