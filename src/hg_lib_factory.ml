open Core
include Hg_lib_factory_intf

module With_global_args = struct
  (* other global flags mainly change the output (e.g. --verbose) or don't matter for most
     commands in this module (e.g. --noninteractive) *)
  type 'a t =
    ?repository:string
    -> ?cwd:string
    -> ?config:(string * string) list
    -> ?env:Async.Process.env
    -> 'a

  let map t ~f =
    fun ?repository ?cwd ?config ?env ->
    f (t ?repository ?cwd ?config ?env)

  let prepend_to_args ~repository ~cwd ~config args =
    List.concat [
      (match repository with
       | None -> []
       | Some repo -> ["--repository"; repo]);
      (match cwd with
       | None -> []
       | Some cwd -> ["--cwd"; cwd]);
      (match config with
       | None -> []
       | Some config ->
         List.concat_map config ~f:(fun (key, data) ->
           ["--config"; key ^ "=" ^ data]));
      args;
    ]
end

module With_global_args_remote = struct
  type 'a t =
    server:Command_server.t
    -> ?repository:string
    -> ?cwd:string
    -> ?config:(string * string) list
    -> 'a

  let map t ~f =
    fun ~server ?repository ?cwd ?config ->
    f (t ~server ?repository ?cwd ?config)

  (* This doesn't take [~cwd] because [Command_server.run] itself handles [~cwd]. *)
  let prepend_to_args ~repository ~config args =
    List.concat [
      (match repository with
       | None -> []
       | Some repo -> ["--repository"; repo]);
      (match config with
       | None -> []
       | Some config ->
         List.concat_map config ~f:(fun (key, data) ->
           ["--config"; key ^ "=" ^ data]));
      args;
    ]
end

let handle_output_with_args ~args handle_output (output : Async.Process.Output.t) =
  Hg_private.Or_simple_error.tag (handle_output output)
    "hg error" args [%sexp_of: string list]

let handle_output_exn ~args handle_output output =
  Or_error.ok_exn (handle_output_with_args ~args handle_output output)

module Simple = struct
  module With_args = With_global_args

  module Output = struct
    type 'a t = 'a
    let return = Fn.id
  end

  let run ?repository ?cwd ?config ?env ~args ~handle_output () =
    let args =
      With_global_args.prepend_to_args ~repository ~cwd ~config args
    in
    let {Unix.Process_info.stdin; stdout; stderr; pid} =
      match env with
      | None -> Unix.create_process ~prog:"hg" ~args
      | Some env -> Unix.create_process_env ~prog:"hg" ~args ~env ()
    in
    let stdout_s = In_channel.input_all (Unix.in_channel_of_descr stdout) in
    let stderr_s = In_channel.input_all (Unix.in_channel_of_descr stderr) in
    let exit_status = Unix.waitpid pid in
    Unix.close stdin;
    Unix.close stdout;
    Unix.close stderr;
    handle_output_exn ~args handle_output {exit_status; stdout=stdout_s; stderr=stderr_s}
end

open Async (* do this before locally redefining Async *)

module Async = struct
  module With_args = With_global_args

  module Output = struct
    type 'a t = 'a Or_error.t Deferred.t
    let return x = return (Ok x)
  end

  let run ?repository ?cwd ?config ?env ~args ~handle_output () =
    let args =
      With_global_args.prepend_to_args ~repository ~cwd ~config args
    in
    Process.create ?env ~prog:"hg" ~args ()
    >>=? fun process ->
    Process.collect_output_and_wait process
    >>| fun output ->
    handle_output_with_args ~args handle_output output
end

module Fixed_hg_environment (E : Hg_env) = struct
  module With_args = With_global_args

  module Output = struct
    type 'a t = 'a Or_error.t Deferred.t
    let return x = return (Ok x)
  end

  let run ?repository ?cwd ?config ?env ~args ~handle_output () =
    let config = Option.map config ~f:(fun config -> E.hg_config_options @ config) in
    let args =
      With_global_args.prepend_to_args ~repository ~cwd ~config args
    in
    let env =
      let tuples =
        [ "HGRCPATH", E.hgrc_path
        ; "HG_USER" , E.hg_user
        ]
      in
      match env with
      | None -> `Extend tuples
      | Some (`Extend envs) -> `Extend (tuples @ envs)
      | Some (`Override l) ->
        `Override (List.map tuples ~f:(fun (x, y) -> (x, Some y)) @ l)
      | Some (`Replace envs) -> `Replace (tuples @ envs)
      | Some (`Replace_raw envs) ->
        let env_strings = List.map tuples ~f:(fun (key, value) -> key ^ "=" ^ value) in
        `Replace_raw (env_strings @ envs)
    in
    if false then begin
      Log.Global.debug !"[%{sexp:Process.env}] %s %{sexp:string list}"
        env E.hg_binary args
    end;
    Process.create ~env ~prog:E.hg_binary ~args ()
    >>=? fun process ->
    Process.collect_output_and_wait process
    >>| fun output ->
    handle_output_with_args ~args handle_output output
end

module Remote = struct
  module With_args = With_global_args_remote

  module Output = Deferred.Or_error

  let run ~server ?repository ?(cwd=".") ?config ~args ~handle_output () =
    let args =
      With_global_args_remote.prepend_to_args ~repository ~config args
    in
    Command_server.run_command server ~cwd args
    >>=? fun output ->
    return (handle_output_with_args ~args handle_output output)
end

module Make_lib (M : Make_s) = struct
  module type S = sig
    module Make (A : Arg) : M(A).S
    module Simple : M(Simple).S
    module Async : M(Async).S
    module Fixed_hg_environment (E : Hg_env) : M(Fixed_hg_environment(E)).S
    module Remote : M(Remote).S
  end
end
