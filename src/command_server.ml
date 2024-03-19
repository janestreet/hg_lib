open Core
open Async
module Def_error = Deferred.Or_error

module Channel_IO : sig
  val run_command : Process.t -> string list -> Process.Output.t Def_error.t

  val read
    :  Reader.t
    -> [ `Message of [ `Error | `Output ] * string | `Result of int ] Def_error.t
end = struct
  let read_length child_stdout =
    let buf = Bytes.create 4 in
    Reader.really_read child_stdout buf
    >>| function
    | `Eof len ->
      Or_error.error
        "read_length: eof"
        (Bytes.To_string.sub ~pos:0 ~len buf)
        String.sexp_of_t
    | `Ok -> Ok (Binary_packing.unpack_unsigned_32_int_big_endian ~buf ~pos:0)
  ;;

  let read child_stdout =
    Reader.read_char child_stdout
    >>= function
    | `Eof -> Deferred.Or_error.error_string "unexpected eof while reading channel"
    | `Ok channel_char ->
      let channel =
        match channel_char with
        | 'o' -> Ok `Output
        | 'e' -> Ok `Error
        | 'r' -> Ok `Result
        (* these are part of the spec, but unsupported by this implementation *)
        (* | 'I' | 'L' | 'd' *)
        | _ -> Or_error.error "unsupported channel" channel_char Char.sexp_of_t
      in
      (match channel with
       | Error _ as err -> return err
       | Ok channel ->
         read_length child_stdout
         >>=? fun len ->
         let buf = Bytes.create len in
         Reader.really_read child_stdout buf
         >>| (function
         | `Eof len ->
           Or_error.error
             "eof while reading message"
             (channel, len, Bytes.To_string.sub buf ~pos:0 ~len)
             [%sexp_of: [ `Output | `Error | `Result ] * int * string]
         | `Ok ->
           (match channel with
            | (`Output | `Error) as channel ->
              Ok (`Message (channel, Bytes.to_string buf))
            | `Result ->
              Ok (`Result (Binary_packing.unpack_signed_32_int_big_endian ~buf ~pos:0)))))
  ;;

  let read_full child_stdout =
    let flatten outputs =
      let stdouts, stderrs =
        List.partition_map outputs ~f:(fun (channel, text) ->
          match channel with
          | `Output -> First text
          | `Error -> Second text)
      in
      String.concat stdouts, String.concat stderrs
    in
    let rec loop acc =
      read child_stdout
      >>=? function
      | `Message (channel, text) -> loop ((channel, text) :: acc)
      | `Result exit_code ->
        let stdout, stderr = flatten (List.rev acc) in
        let exit_status =
          if exit_code = 0 then Ok () else Error (`Exit_non_zero exit_code)
        in
        Def_error.return { Process.Output.stdout; stderr; exit_status }
    in
    loop []
  ;;

  let send_command child_stdin args =
    let command = String.concat args ~sep:"\000" in
    let buf = Bytes.create 4 in
    Binary_packing.pack_unsigned_32_int_big_endian ~buf ~pos:0 (String.length command);
    try_with
      ~run:`Schedule (* consider [~run:`Now] instead; see: https://wiki/x/ByVWF *)
      ~rest:`Log
      (* consider [`Raise] instead; see: https://wiki/x/Ux4xF *)
      (fun () ->
      Writer.write child_stdin "runcommand\n";
      Writer.write_bytes child_stdin buf;
      Writer.write child_stdin command;
      Writer.flushed child_stdin)
    >>| function
    | Ok _ as ok -> ok
    | Error exn ->
      Or_error.error
        "unable to write command; process is probably dead!"
        (args, exn)
        [%sexp_of: string list * exn]
  ;;

  let run_command process args =
    let child_stdin = Process.stdin process in
    send_command child_stdin args >>=? fun () -> read_full (Process.stdout process)
  ;;
end

type t = Process.t Throttle.Sequencer.t

let valid_hello ~accepted_encodings hello =
  let accepted_encodings =
    List.map accepted_encodings ~f:(function
      | `Ascii -> "ascii"
      | `Utf8 -> "UTF-8")
  in
  let attrs =
    List.filter_map (String.split ~on:'\n' hello) ~f:(fun line ->
      Option.map (String.lsplit2 ~on:':' line) ~f:(fun (name, data) ->
        String.strip name, String.strip data))
  in
  let check key ~f =
    match List.Assoc.find attrs ~equal:String.equal key with
    | None ->
      Or_error.error_s
        [%message "key not in attrs" (key : string) (attrs : (string * string) list)]
    | Some value -> f value
  in
  Or_error.combine_errors_unit
    [ check "capabilities" ~f:(fun vals ->
        let capabilities = String.split ~on:' ' vals in
        let is_runcommand value = String.equal "runcommand" (String.strip value) in
        if List.exists capabilities ~f:is_runcommand
        then Ok ()
        else
          Or_error.error_s
            [%message
              "capabilities don't include runcommand" (capabilities : string list)])
    ; check "encoding" ~f:(fun encoding ->
        if List.mem ~equal:String.equal accepted_encodings encoding
        then Ok ()
        else
          Or_error.error_s
            [%message
              "encoding unacceptable; this can be caused by incorrect locale settings, \
               check the output of the `locale` command"
                (accepted_encodings : string list)
                (encoding : string)])
    ]
;;

let%test _ =
  Result.is_ok
    (valid_hello
       ~accepted_encodings:[ `Utf8 ]
       "capabilities: getencoding runcommand\nencoding: UTF-8")
;;

let%test _ =
  Result.is_ok
    (valid_hello
       ~accepted_encodings:[ `Ascii ]
       "capabilities: getencoding runcommand\nencoding: ascii\n")
;;

let%test _ =
  Result.is_error
    (valid_hello
       ~accepted_encodings:[ `Utf8 ]
       "capabilities: getencoding runcommand\nencoding: ascii\n")
;;

let%test _ =
  Result.is_error
    (valid_hello
       ~accepted_encodings:[ `Ascii ]
       "capabilities: getencoding runcommand\nencoding: UTF-8")
;;

let%test _ =
  Result.is_error
    (valid_hello
       ~accepted_encodings:[ `Ascii ]
       "capabilities: getencoding\nencoding: ascii\n")
;;

let%expect_test "report both errors" =
  let open Expect_test_helpers_core in
  show_raise (fun () ->
    valid_hello
      ~accepted_encodings:[ `Utf8 ]
      "capabilities: getencoding\nencoding: ascii\n"
    |> ok_exn);
  [%expect
    {|
    (raised (
      ("capabilities don't include runcommand" (capabilities (getencoding)))
      ("encoding unacceptable; this can be caused by incorrect locale settings, check the output of the `locale` command"
       (accepted_encodings (UTF-8))
       (encoding ascii))))
    |}];
  return ()
;;

module Ssh = struct
  type t =
    { host : string
    ; user : string option
    ; options : string list
    }
end

let create ?env ?(hg_binary = "hg") ?config ~accepted_encodings ssh =
  let config =
    Option.value_map config ~default:[] ~f:(fun config ->
      List.concat_map config ~f:(fun (key, data) -> [ "--config"; key ^ "=" ^ data ]))
  in
  let prog, extra_args =
    match ssh with
    | None -> hg_binary, []
    | Some { Ssh.host; user; options } ->
      let user_string =
        match user with
        | None -> ""
        | Some user -> user ^ "@"
      in
      "ssh", options @ [ user_string ^ host; "--"; hg_binary ]
  in
  let args = extra_args @ [ "serve"; "--cmdserver"; "pipe" ] @ config in
  (match ssh with
   | None ->
     (* When running a local server, start it in the user's home directory. This makes it
        consistent with running a remote server. *)
     Monitor.try_with_or_error ~here:[%here] Sys.home_directory >>|? Option.return
   | Some _ -> return (Ok None))
  >>=? fun working_dir ->
  Process.create ?env ?working_dir ~prog ~args ()
  >>=? fun process ->
  let hello_result =
    Channel_IO.read (Process.stdout process)
    >>=? function
    | `Message (`Error, error) ->
      Deferred.Or_error.error_s [%message "replied on error channel" (error : string)]
    | `Result result ->
      Deferred.Or_error.error_s
        [%message "replied on result channel, expecting output channel" (result : int)]
    | `Message (`Output, text) -> return (valid_hello ~accepted_encodings text)
  in
  Deferred.Or_error.tag_arg
    hello_result
    "parsing hello from command server failed"
    (prog, args)
    [%sexp_of: string * string list]
  >>= function
  | Ok () -> Deferred.Or_error.return (Throttle.Sequencer.create process)
  | Error _ as err ->
    Process.send_signal process Signal.term;
    Process.collect_output_and_wait process
    >>| fun output ->
    Or_error.tag_arg err "Process output" output Process.Output.sexp_of_t
;;

let run_command t ~cwd args =
  Throttle.enqueue t (fun process ->
    Channel_IO.run_command process ("--cwd" :: cwd :: args))
;;

let destroy t =
  Throttle.enqueue t (fun process ->
    Deferred.ignore_m (Process.collect_output_and_wait process))
;;
