open Core
module Time = Time_unix

module Node = struct
  module Public = struct
    type t =
      { global_id : string
      ; local_revision : int
      }
    [@@deriving sexp, fields, compare]
  end
  include Public
end

module Changeset_info = struct
  module Public = struct
    type t =
      { node        : Node.t
      ; parents     : [ `Zero | `One of Node.t | `Two of Node.t * Node.t ]
      ; author      : string
      ; time        : Time.t
      ; tags        : string list
      ; description : string
      }
    [@@deriving sexp, fields, compare]

    let to_hg_style_string t =
      let node { Node.global_id; local_revision } =
        sprintf "%i:%s" local_revision (String.sub global_id ~pos:0 ~len:12)
      in
      let line label value =
        sprintf "%-13s%s" (label ^ ":") value
      in
      let parents =
        match t.parents with
        | `Zero -> []
        | `One p -> [ p ]
        | `Two (p1, p2) -> [ p1; p2 ]
      in
      [ [ line "changeset" (node t.node) ]
      ; List.map t.tags ~f:(fun tag -> line "tag" tag)
      ; List.map parents ~f:(fun parent -> line "parent" (node parent))
      ; [ line "user" t.author
        ; line "date" (Time.to_string t.time)
        ; line "summary" t.description
        ]
      ]
      |> List.concat
      |> String.concat ~sep:"\n"
    ;;
  end

  include Public

  let template =
    [ "{node} {rev}"
    ; "{p1.node} {p1.rev}"
    ; "{p2.node} {p2.rev}"
    ; "{author|emailuser}"
    ; "{date|hgdate}"
    ; "{tags}"
    ; "{desc|tabindent}"
    ; ""
    ]
    |> String.concat ~sep:"\\n"
  ;;

  let time_of_hgtime hgdate =
    (* The format is '<unix timestamp of commit> <timezone offset of commit in
       seconds>'. We only need the second part if we want to know the where the commit
       happened. *)
    match String.split hgdate ~on:' ' with
    | [ unix_timestamp; _zone_offset_in_seconds ] ->
      Time.of_span_since_epoch (Time.Span.of_sec (Float.of_string unix_timestamp))
    | _ ->
      failwithf "Bad hgdate value '%s' hgdate" hgdate ()

  let%test_unit _ =
    [%test_result: Time.t]
      (time_of_hgtime "1429736177 14400")
      ~expect:(Time.of_string "2015-04-22 16:56:17-04:00")

  let%test_unit _ =
    [%test_result: Time.t]
      (time_of_hgtime "1429715393 -3600")
      ~expect:(Time.of_string "2015-04-22 16:09:53+01:00")

  let of_templated_stdout stdout =
    (* Every log entry has a newline appended to it. So [String.split ~on:'\n'] will
       necessarily give us an extra empty chunk. This is true even for empty input.
       Using [String.split_lines] would maybe be nicer, but it also splits on \r\n so it
       loses information. *)
    let lines =
      String.split ~on:'\n' stdout
      |> List.rev
      |> (function
        | "" :: rest -> rest
        | _lines -> failwith "Does not end with a newline character")
      |> List.rev
    in
    let rec aux acc lines =
      match lines with
      | [] -> List.rev acc
      | node :: p1 :: p2 :: author :: time :: tags :: first_desc :: tl ->
        let end_desc, remainder =
          List.split_while tl ~f:(fun line ->
            (* tabindent does *not* change blank lines to "\t", which is a little
               annoying *)
            String.is_empty line || Char.equal line.[0] '\t')
        in
        let description =
          String.concat ~sep:"\n"
            (first_desc :: (List.map end_desc ~f:String.strip))
        in
        let node_of_string str =
          let (global_id, local_revision) = String.lsplit2_exn ~on:' ' str in
          let local_revision = Int.of_string local_revision in
          { Node.global_id; local_revision }
        in
        let node = node_of_string node in
        let parents =
          let p1_node = node_of_string p1 in
          let p2_node = node_of_string p2 in
          match p1_node.local_revision, p2_node.local_revision with
          | -1, -1 -> `Zero
          | _, -1 -> `One p1_node
          | _, _ -> `Two (p1_node, p2_node)
        in
        let changeset =
          { node
          ; parents
          ; author
          ; time = time_of_hgtime time
          ; tags =
              String.split ~on:' ' tags
              |> List.filter ~f:(fun s -> not (String.is_empty s))
          ; description
          }
        in
        aux (changeset :: acc) remainder
      | _ -> failwith "Unexpected number of lines"
    in
    Or_error.tag_arg
      (Or_error.try_with (fun () -> aux [] lines))
      "Malformed output" stdout sexp_of_string
  ;;

  let%test_unit _ =
    let stdout =
      "0123456789abcdef0123456789abcdef01234567 4\n\
       fedcba9876543210fedcba9876543210fedcba98 2\n\
       123454321098767890abcdefedcba12345432109 3\n\
       username\n\
       1609662832 18000\n\
       first-tag second-tag third-tag\n\
       rebase to [123454321098] with ancestor [fedcba987654]\n\
       fedcba9876543210fedcba9876543210fedcba98 2\n\
       fedcbabcdef678909876543212345fedcbabcdef 1\n\
       0000000000000000000000000000000000000000 -1\n\
       username\n\
       1609511270 18000\n\
       \n\
       some commit description\n\
      "
    in
    let t1 =
      { node =
          { global_id = "0123456789abcdef0123456789abcdef01234567"
          ; local_revision = 4
          }
      ; parents =
          `Two
            ( { global_id = "fedcba9876543210fedcba9876543210fedcba98"
              ; local_revision = 2
              }
            , { global_id = "123454321098767890abcdefedcba12345432109"
              ; local_revision = 3
              } )
      ; author = "username"
      ; time = Time.of_string "2021-01-03 03:33:52-05:00"
      ; tags = [ "first-tag"; "second-tag"; "third-tag" ]
      ; description = "rebase to [123454321098] with ancestor [fedcba987654]"
      }
    in
    let t2 =
      { node =
          { global_id = "fedcba9876543210fedcba9876543210fedcba98"
          ; local_revision = 2
          }
      ; parents =
          `One
            { global_id = "fedcbabcdef678909876543212345fedcbabcdef"
            ; local_revision = 1
            }
      ; author = "username"
      ; time = Time.of_string "2021-01-01 09:27:50-05:00"
      ; tags = []
      ; description = "some commit description"
      }
    in
    [%test_result: t list Or_error.t]
      (of_templated_stdout stdout)
      ~expect:(Ok [ t1; t2 ]);
    [%test_result: string]
      ([t1; t2]
       |> List.map ~f:to_hg_style_string
       |> String.concat ~sep:"\n\n")
      ~expect:
        "changeset:   4:0123456789ab\n\
         tag:         first-tag\n\
         tag:         second-tag\n\
         tag:         third-tag\n\
         parent:      2:fedcba987654\n\
         parent:      3:123454321098\n\
         user:        username\n\
         date:        2021-01-03 03:33:52.000000-05:00\n\
         summary:     rebase to [123454321098] with ancestor [fedcba987654]\n\
         \n\
         changeset:   2:fedcba987654\n\
         parent:      1:fedcbabcdef6\n\
         user:        username\n\
         date:        2021-01-01 09:27:50.000000-05:00\n\
         summary:     some commit description"
  ;;

  let%test_unit "heads in empty repo" =
    let stdout =
      "0000000000000000000000000000000000000000 -1\n\
       0000000000000000000000000000000000000000 -1\n\
       0000000000000000000000000000000000000000 -1\n\
       \n\
       0 0\n\
       \n\
       \n\
      "
    in
    [%test_result: t list Or_error.t]
      (of_templated_stdout stdout)
      ~expect:(Ok [
        { node =
            { global_id      = "0000000000000000000000000000000000000000"
            ; local_revision = -1
            }
        ; parents = `Zero
        ; author = ""
        ; time = Time.epoch
        ; tags = []
        ; description = ""
        }
      ])

  let%test_unit "empty stdout" =
    let stdout = "" in
    [%test_result: t list Or_error.t] (of_templated_stdout stdout) ~expect:(Ok [])

end

module Bookmark = struct
  module Public = struct
    type t =
      { active      : bool
      ; name        : string
      ; revision_id : string
      } [@@deriving sexp, fields]
  end
  include Public

  let of_lines = function
    | ["no bookmarks set"] -> []
    | bookmark_lines ->
      List.map bookmark_lines ~f:(fun line ->
        let active = Char.equal line.[1] '*' in
        (* get rid of the space where the asterisk may appear *)
        let line = String.slice line 3 0 in
        let raw_name, revision = String.rsplit2_exn line ~on:' ' in
        let name = String.strip raw_name in
        let _, revision_id = String.rsplit2_exn revision ~on:':' in
        {active; name; revision_id})
  ;;
end

module Tag = struct
  module Public = struct
    type t =
      { tag          : string
      ; revision_num : int
      ; revision_id  : string
      } [@@deriving sexp, fields]
  end
  include Public

  let of_line line =
    let tag =
      String.split line ~on:' '
      |> List.filter ~f:(fun l -> not (String.is_empty l))
    in
    match tag with
    | []         -> None
    | [tag; rev] ->
      let revision_num, revision_id = String.rsplit2_exn rev ~on:':' in
      let revision_num = Int.of_string revision_num in
      Some {tag; revision_num; revision_id}
    | _  -> failwithf "unexpected hg output: '%s'" line ()
end

module File_status = struct
  module Public = struct
    type t =
      | Modified of string
      | Added of string
      | Removed of string
      | Copied of { src : string; dst : [ `New_file of string | `Overwritten of string ] }
      | Missing of string
      | Not_tracked of string
    [@@deriving sexp]
  end
  include Public
end

(** See "hg help dates". *)
module Time_with_utc_offset = struct
  type t =
    { time       : Time.t
    ; utc_offset : Time.Span.t
    }

  let of_time_with_zone ~zone time = { time; utc_offset = Time.utc_offset ~zone time }

  let to_string t =
    let secs_since_epoch =
      Time.to_span_since_epoch t.time |> Time.Span.to_sec |> Float.iround_towards_zero_exn
    in
    let secs_west_of_utc =
      t.utc_offset |> Time.Span.neg |> Time.Span.to_sec |> Float.iround_towards_zero_exn
    in
    sprintf "%d %d" secs_since_epoch secs_west_of_utc
  ;;

  let%expect_test "to_string" =
    (*
       $ TZ=Etc/Utc date --date '2001-02-03 04:05:06' +%s
       981173106
    *)
    let time = Time.of_string_with_utc_offset "2001-02-03 04:05:06Z" in
    of_time_with_zone ~zone:Time.Zone.utc time |> to_string |> print_endline;
    [%expect {| 981173106 0 |}];
    of_time_with_zone ~zone:(Time.Zone.of_string "America/New_York") time
    |> to_string
    |> print_endline;
    [%expect {| 981173106 18000 |}]
  ;;
end

module Date_param = struct
  module Public = struct
    module Time_point = struct
      type t =
        | Date of Date.t
        | Time of Time.t
      [@@deriving sexp]
    end

    type t =
      | Exact of Time_point.t
      | On_or_before of Time_point.t
      | On_or_after of Time_point.t
      | Inclusive_range of { from : Time_point.t; to_ : Time_point.t }
    [@@deriving sexp]
  end

  include Public

  module Time_point = struct
    include Time_point

    let to_string  = function
      | Date date -> Date.to_string date
      | Time time -> String.concat [Time.to_sec_string ~zone:Time.Zone.utc time; " UTC"]
    ;;
  end

  let to_string = function
    | Exact time -> sprintf !"%{Time_point}" time
    | On_or_before time -> sprintf !"<%{Time_point}" time
    | On_or_after time -> sprintf !">%{Time_point}" time
    | Inclusive_range { from; to_ } -> sprintf !"%{Time_point} to %{Time_point}" from to_
  ;;

  let%expect_test "Date serializes correctly" =
    let from_date = Date.create_exn ~y:2005 ~m:Month.Apr ~d:30 |> Time_point.Date in
    let to_date = Date.create_exn ~y:2017 ~m:Month.Dec ~d:1 |> Time_point.Date in
    let from_time = Time_point.Time Time.epoch in
    let to_time =
      Time.of_date_ofday
        ~zone:(Time.Zone.of_utc_offset ~hours:(-4))
        (Date.create_exn ~y:2019 ~m:Mar ~d:28)
        (Time.Ofday.create ~hr:14 ~min:32 ~sec:20 ~ms:412 ~us:15 ~ns:14 ())
      |> Time_point.Time
    in
    to_string (Inclusive_range {from = from_date; to_ = to_date}) |> print_endline;
    [%expect {| 2005-04-30 to 2017-12-01 |}];
    to_string (Inclusive_range {from = from_time; to_ = to_time}) |> print_endline;
    [%expect {| 1970-01-01 00:00:00 UTC to 2019-03-28 18:32:20 UTC |}];
    to_string (Inclusive_range {from = from_date; to_ = to_time}) |> print_endline;
    [%expect {| 2005-04-30 to 2019-03-28 18:32:20 UTC |}];
    to_string (Exact to_time) |> print_endline;
    [%expect {| 2019-03-28 18:32:20 UTC |}];
    to_string (Exact to_date) |> print_endline;
    [%expect {| 2017-12-01 |}];
    to_string (On_or_before to_time) |> print_endline;
    [%expect {| <2019-03-28 18:32:20 UTC |}];
    to_string (On_or_before to_date) |> print_endline;
    [%expect {| <2017-12-01 |}];
    to_string (On_or_after from_time) |> print_endline;
    [%expect {| >1970-01-01 00:00:00 UTC |}];
    to_string (On_or_after from_date) |> print_endline;
    [%expect {| >2005-04-30 |}]
  ;;
end

module Destination = struct
  module Public = struct
    type _ t =
      | String : string t
      | File : string -> unit t
  end

  include Public

  let handle_output (type a) (t : a t) (o : Async.Process.Output.t) : a =
    match t with
    | File _ -> ()
    | String -> o.stdout
end

(** This is just a wrapper around [Or_error.t].  It exists because it is for very simple
    errors (e.g. "unexpected exit status") which you should tag to produce better errors
    containing, e.g., the final list of arguments passed to hg. *)
module Or_simple_error = struct
  module Public = struct
    type 'a t = 'a Or_error.t
    let tag = Or_error.tag_arg
    let simple_error = Fn.id
  end

  include Public

  let create = Fn.id
end

(* Helpful functions for implementing hg command wrappers. *)
module Command_helpers = struct
  let repeated name = function
    | None   -> []
    | Some l -> List.concat_map l ~f:(fun arg -> [name; arg])

  let bookmarks_args = repeated "--bookmark"
  let branches_args  = repeated "--branch"
  let excludes_args  = repeated "--exclude"
  let includes_args  = repeated "--include"
  let keywords_args  = repeated "--keyword"
  let options_args   = repeated "--option"
  let revs_args      = repeated "--rev"

  let with_arg' name to_string = function
    | None     -> []
    | Some arg -> [name; to_string arg]

  let with_arg name = with_arg' name Fn.id

  let remotecmd_args = with_arg "--remotecmd"
  let rev_args       = with_arg "--rev"
  let ssh_args       = with_arg "--ssh"
  let template_args  = with_arg "--template"

  let date_range_args = with_arg' "--date" Date_param.to_string
  let time_args       = with_arg' "--date" Time_with_utc_offset.to_string

  let limit_args      = with_arg' "--limit" Int.to_string
  let unified_args    = with_arg' "--unified" Int.to_string

  let no_arg name = function
    | None    -> []
    | Some () -> [name]

  let after_args    = no_arg "--after"
  let force_args    = no_arg "--force"
  let forget_args   = no_arg "--forget"
  let insecure_args = no_arg "--insecure"
  let all_args      = no_arg "--all"

  (* common output handlers *)
  let non_0_exit_error output =
    Or_error.error "non-zero exit status" output Async.Process.Output.sexp_of_t

  let unexpected_exit_error output =
    Or_error.error "unexpected exit status" output Async.Process.Output.sexp_of_t

  let expect_0 (o : Async.Process.Output.t) =
    match o.exit_status with
    | Ok ()   -> Ok ()
    | Error _ -> non_0_exit_error o

  let expect_0_stdout (o : Async.Process.Output.t) =
    match o.exit_status with
    | Ok ()   -> Ok o.stdout
    | Error _ -> non_0_exit_error o

  let expect_0_stdout_list (o : Async.Process.Output.t) =
    Or_error.map (expect_0_stdout o) ~f:String.split_lines
end

module Public = struct
  module Node = Node.Public
  module Changeset_info = Changeset_info.Public
  module Bookmark = Bookmark.Public
  module Tag = Tag.Public
  module File_status = File_status.Public
  module Date_param = Date_param.Public
  module Destination = Destination.Public
  module Or_simple_error = Or_simple_error.Public
end
