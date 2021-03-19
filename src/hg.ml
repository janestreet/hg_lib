open Core
open Hg_lib_factory
include Hg_intf

module Make (A : Arg) = struct
  open Hg_private
  open Command_helpers

  let return = A.Output.return

  let command name f ~handle_output =
    let handle_output o = Or_simple_error.create (handle_output o) in
    A.With_args.map A.run ~f:(fun run ->
      f (fun argses ->
        run ~args:(name :: List.concat argses) ~handle_output ()))

  (* commands *)

  let add =
    command "add" (fun run ?includes ?excludes which_files ->
      match which_files with
      | `These_files [] -> return ()
      | _ ->
        run [
          includes_args includes;
          excludes_args excludes;
          (match which_files with
           | `All_files -> []
           | `These_files files -> files);
        ])
      ~handle_output:expect_0

  let annotate =
    command "annotate"
      (fun run ?rev ?user ?file ?date ?number ?changeset ?skip ?ignore_space_change
        ?ignore_blank_lines ?ignore_space_at_eol ?includes ?excludes ?template filename ->
        run
          [ rev_args rev
          ; no_arg "--user" user
          ; no_arg "--file" file
          ; no_arg "--date" date
          ; no_arg "--quiet" date (* Force YYYY-MM-DD *)
          ; no_arg "--number" number
          ; no_arg "--changeset" changeset
          (* As of 2021-01, --skip is an experimental flag and only shows up in the
             verbose help output. *)
          ; repeated "--skip" skip
          ; no_arg "--ignore-space-change" ignore_space_change
          ; no_arg "--ignore-blank-lines" ignore_blank_lines
          ; no_arg "--ignore-space-at-eol" ignore_space_at_eol
          ; includes_args includes
          ; excludes_args excludes
          ; template_args template
          ; [ filename ]
          ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () ->
          o.stdout
          |> String.split ~on:'\n'
          |> List.filter ~f:(Fn.non String.is_empty)
          |> Ok
        | Error _ -> unexpected_exit_error o)

  let archive =
    command "archive"
      (fun
        run ?no_decode ?prefix ?rev ?type_ ?subrepos ?includes ?excludes ~destination ()
        ->
          run [
            no_arg "--no-decode" no_decode;
            with_arg "--prefix" prefix;
            rev_args rev;
            with_arg "--type" type_;
            no_arg "--subrepos" subrepos;
            includes_args includes;
            excludes_args excludes;
            [destination];
          ])
      ~handle_output:expect_0

  let bookmarks =
    command "bookmarks" (fun run () -> run [])
      ~handle_output:(fun output ->
        Or_error.map (expect_0_stdout_list output) ~f:Bookmark.of_lines)

  let change_bookmark =
    command "bookmarks" (fun run ?force ~name which_flags ->
      run [
        force_args force;
        [name];
        (match which_flags with
         | `Set_rev rev -> ["--rev"; rev]
         | `Inactive    -> ["--inactive"]
         | `Delete      -> ["--delete"]
         | `Rename old  -> ["--rename"; old]
         | `Current     -> []
        );
      ])
      ~handle_output:expect_0

  let bundle =
    command "bundle"
      (fun run ?force ?revs ?branches ?bases ?all ?compression_type ?ssh ?remotecmd
        ?insecure ?destination bundle_file ->
        run [
          force_args force;
          revs_args revs;
          branches_args branches;
          ssh_args ssh;
          remotecmd_args remotecmd;
          insecure_args insecure;
          with_arg "--type" compression_type;
          repeated "--base" bases;
          no_arg "--all" all;
          [bundle_file];
          Option.to_list destination
        ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () -> Ok `Ok
        | Error (`Exit_non_zero 1) -> Ok `Nothing_to_bundle
        | Error _ -> unexpected_exit_error o)

  let clone =
    command "clone"
      (fun run ~source ?destination ?update ?revs ?branches ?pull ?uncompressed
        ?ssh ?remotecmd ?insecure () ->
        let update =
          match update with
          | Some `No_update -> ["--noupdate"]
          | Some `Rev rev -> ["--updaterev"; rev]
          | None -> []
        in
        run [
          update;
          revs_args revs;
          branches_args branches;
          no_arg "--pull" pull;
          no_arg "--uncompressed" uncompressed;
          ssh_args ssh;
          remotecmd_args remotecmd;
          insecure_args insecure;
          [source];
          Option.to_list destination;
        ])
      ~handle_output:expect_0

  let cat (type a) ~(destination : a Destination.t) =
    command "cat" (fun run ?includes ?excludes ?rev ?template path ->
      run [
        includes_args includes;
        excludes_args excludes;
        revs_args (Option.map ~f:List.return rev);
        template_args template;
        [path];
        (match destination with
         | Destination.String -> []
         | Destination.File path -> ["--output"; path]
        );
      ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () -> Ok (`Ok (Destination.handle_output destination o))
        | Error (`Exit_non_zero 1) -> Ok `No_such_file
        | Error _ -> unexpected_exit_error o
      )

  let commit =
    command "commit"
      (fun run ?addremove ?allow_commit_without_bookmark ?includes ?excludes ~message
        ?date ?user ?files () ->
        match files with
        | Some [] -> return `Nothing_changed
        | _ ->
          run [
            no_arg "--addremove" addremove;
            no_arg "--allow-commit-without-bookmark" allow_commit_without_bookmark;
            includes_args includes;
            excludes_args excludes;
            ["--message"; message];
            date_args date;
            with_arg "--user" user;
            Option.value files ~default:[];
          ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () -> Ok `Ok
        | Error (`Exit_non_zero 1) -> Ok `Nothing_changed
        | Error _ -> unexpected_exit_error o)

  let config =
    command "config"
      (fun run ?untrusted ?names () ->
         run [
           no_arg "--untrusted" untrusted;
           Option.value names ~default:[];
         ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () ->
          List.map (String.split ~on:'\n' (String.strip o.stdout)) ~f:(fun line ->
            match String.lsplit2 line ~on:'=' with
            | Some (key, data) -> Ok (key, data)
            | None -> Or_error.error "malformed config line" line String.sexp_of_t)
          |> Or_error.combine_errors
        | Error _ -> non_0_exit_error o)

  let copy =
    command "copy" (fun run ?forget ?after ?force ?includes ?excludes src dst ->
      run [
        forget_args forget;
        after_args after;
        force_args force;
        includes_args includes;
        excludes_args excludes;
        [src; dst];
      ])
      ~handle_output:expect_0

  let diff =
    command "diff"
      (fun run ?revs ?change ?text ?git ?reverse ?ignore_all_space ?ignore_space_change
        ?ignore_blank_lines ?unified ?stat ?includes ?excludes ?subrepos ?files () ->
        match files with
        | Some [] -> return ""
        | _ ->
          run [
            revs_args revs;
            with_arg "--change" change;
            no_arg "--text" text;
            no_arg "--git" git;
            no_arg "--reverse" reverse;
            no_arg "--ignore-all-space" ignore_all_space;
            no_arg "--ignore-space-change" ignore_space_change;
            no_arg "--ignore-blank-lines" ignore_blank_lines;
            unified_args unified;
            no_arg "--stat" stat;
            includes_args includes;
            excludes_args excludes;
            no_arg "--subrepos" subrepos;
            Option.value files ~default:[];
          ])
      ~handle_output:expect_0_stdout

  let extdiff =
    command "extdiff" (fun run ?revs ?change ?includes ?excludes ?program ?options ?files () ->
      match files with
      | Some [] -> return ""
      | _ ->
        run [
          revs_args revs;
          with_arg "--change" change;
          includes_args includes;
          excludes_args excludes;
          with_arg "--program" program;
          options_args options;
          [ "--config"; "extensions.extdiff=" ];
          Option.value files ~default:[];
        ])
      ~handle_output:expect_0_stdout

  let files =
    command "files" (fun run ?rev ?includes ?excludes ?subrepos which_files ->
      match which_files with
      | `These_files [] -> return []
      | _ ->
        run [
          rev_args rev;
          includes_args includes;
          excludes_args excludes;
          no_arg "--subrepos" subrepos;
          (match which_files with
           | `All_files -> []
           | `These_files files ->
             files);
        ])
      ~handle_output:expect_0_stdout_list

  let heads =
    command "heads"
      (fun run ?rev ?topo ?closed () ->
         run [
           with_arg "--rev" rev;
           no_arg "--topo" topo;
           no_arg "--closed" closed;
           ["--template"; Changeset_info.template]
         ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ ->
          (* 1 is documented as "no matching heads found," but I think this only comes up
             if you're closing branch heads, which we don't really do. Rather than making
             everyone explicitly deal with an error case that's very unlikely to come up,
             we'll just report it as a generic error. *)
          non_0_exit_error o
        | Ok () -> Changeset_info.of_templated_stdout o.stdout)

  let id =
    command "log" (* yes, you read that right *)
      (fun run ?(rev = ".") () ->
         run [
           ["--rev"; rev];
           ["--template"; "{node}"];
         ])
      ~handle_output:expect_0_stdout

  let init =
    command "init"
      (fun run ?ssh ?remotecmd ?insecure ?dest () ->
         run [
           ssh_args ssh;
           remotecmd_args remotecmd;
           insecure_args insecure;
           Option.to_list dest
         ])
      ~handle_output:expect_0

  let is_repo =
    command "root"
      (fun run () -> run [])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () -> Ok true
        | Error (`Exit_non_zero 255) -> Ok false
        | Error _ -> unexpected_exit_error o)

  let log =
    command "log"
      (fun run ?follow ?date ?copies ?keywords ?revs ?removed ?users ?branches ?prune_revs
        ?limit ?no_merges ?includes ?excludes ?files () ->
        match files with
        | Some [] -> return []
        | _ ->
          run [
            no_arg "--follow" follow;
            date_range_args date;
            no_arg "--copies" copies;
            keywords_args keywords;
            revs_args revs;
            no_arg "--removed" removed;
            repeated "--user" users;
            branches_args branches;
            repeated "--prune" prune_revs;
            limit_args limit;
            no_arg "--no-merges" no_merges;
            includes_args includes;
            excludes_args excludes;
            ["--template"; Changeset_info.template];
            Option.value files ~default:[];
          ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ -> non_0_exit_error o
        | Ok () -> Changeset_info.of_templated_stdout o.stdout)

  let manifest =
    command "manifest"
      (fun run ?rev ?all () ->
         run [
           rev_args rev;
           all_args all;
         ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ -> non_0_exit_error o
        | Ok () ->
          Ok (String.split ~on:'\n' o.stdout
              |> List.filter ~f:(fun line -> not (String.is_empty line))))

  let merge =
    command "merge"
      (fun run ?(tool = ":merge3") ?allow_commit_without_bookmark target ->
         let rev =
           match target with
           | `Unique_other_head -> None
           | `Rev rev -> Some rev
         in
         run [
           rev_args rev;
           ["--tool"; tool];
           no_arg "--allow-commit-without-bookmark" allow_commit_without_bookmark;
         ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error (`Exit_non_zero 1) -> Ok `Unresolved_files
        | Error _ -> unexpected_exit_error o
        | Ok () -> Ok `Ok)

  let out =
    command "out"
      (fun run ?force ?revs ?limit ?no_merges ?ssh ?remotecmd ?insecure ?remote_path () ->
         run [
           no_arg "--force" force;
           revs_args revs;
           limit_args limit;
           no_arg "--no-merges" no_merges;
           ssh_args ssh;
           remotecmd_args remotecmd;
           insecure_args insecure;
           ["--quiet"];
           ["--template"; Changeset_info.template];
           Option.to_list remote_path;
         ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error (`Exit_non_zero 1) -> Ok []
        | Error _ -> unexpected_exit_error o
        | Ok () -> Changeset_info.of_templated_stdout o.stdout)

  let pull =
    command "pull"
      (fun run ?update ?force ?revs ?bookmarks ?branches ?ssh ?remotecmd ?insecure ?rebase
        ?remote_path () ->
        run [
          no_arg "--update" update;
          force_args force;
          revs_args revs;
          bookmarks_args bookmarks;
          branches_args branches;
          ssh_args ssh;
          remotecmd_args remotecmd;
          insecure_args insecure;
          no_arg "--rebase" rebase;
          Option.to_list remote_path;
        ])
      ~handle_output:expect_0

  let purge =
    command "purge"
      (fun run ?abort_on_err ?all ?dirs ?files ?includes ?excludes () ->
         run [
           no_arg "--abort-on-err" abort_on_err;
           no_arg "--all" all;
           no_arg "--dirs" dirs;
           no_arg "--files" files;
           includes_args includes;
           excludes_args excludes;
           [ "--config"; "extensions.purge=" ];
         ])
      ~handle_output:expect_0

  let push =
    command "push"
      (fun run ?force ?revs ?bookmarks ?branches ?new_branch ?ssh ?remotecmd ?insecure
        ?remote_path () ->
        run [
          force_args force;
          revs_args revs;
          bookmarks_args bookmarks;
          branches_args branches;
          no_arg "--new-branch" new_branch;
          ssh_args ssh;
          remotecmd_args remotecmd;
          insecure_args insecure;
          Option.to_list remote_path;
        ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Ok () -> Ok `Ok
        | Error (`Exit_non_zero 1) -> Ok `Nothing_to_push
        | Error _ -> unexpected_exit_error o)

  let remove =
    command "remove" (fun run ?after ?force ?includes ?excludes files ->
      run [
        after_args after;
        force_args force;
        includes_args includes;
        excludes_args excludes;
        files;
      ])
      ~handle_output:expect_0

  let rename =
    command "rename" (fun run ?after ?force ?includes ?excludes src dst ->
      run [
        after_args after;
        force_args force;
        includes_args includes;
        excludes_args excludes;
        [src; dst];
      ])
      ~handle_output:expect_0

  let mark_resolved =
    command "resolve" (fun run files ->
      match files with
      | `These_files [] -> return ()
      | _ ->
        let files =
          (* This command also accepts a [--all] flag, but it's not necessary with
             [--mark]. *)
          match files with
          | `All_files -> []
          | `These_files x -> x
        in
        run [["--mark"]; files])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ -> non_0_exit_error o
        | Ok () ->
          (* The case where you specify a file path that doesn't exist still exits with
             status 0, weirdly, so we have to check stderr. *)
          if String.is_empty o.stderr
          then Ok ()
          else unexpected_exit_error o)
  ;;

  let revert =
    command "revert" (fun run ?date ?rev ?no_backup ?includes ?excludes files ->
      match files with
      | `These_files [] -> return ()
      | _ ->
        (* For safety, [hg revert] with no file arguments and without [--all] does not
           revert your entire repo. We instead use a variant type, so we should add
           [--all] automatically. *)
        let files, all =
          match files with
          | `All_files -> [], Some ()
          | `These_files x -> x, None
        in
        run [
          all_args all;
          date_args date;
          rev_args rev;
          no_arg "--no-backup" no_backup;
          includes_args includes;
          excludes_args excludes;
          files;
        ])
      ~handle_output:expect_0

  let share =
    command "share" (fun run ?noupdate ?bookmarks ~src ~dst () ->
      run [
        no_arg "--noupdate" noupdate;
        no_arg "--bookmarks" bookmarks;
        [src];
        [dst];
      ])
      ~handle_output:expect_0

  let status =
    command "status" (fun run ?rev ?rev2 ?change ?includes ?excludes ?subrepos () ->
      run [
        with_arg "--rev" rev;
        with_arg "--rev" rev2;
        with_arg "--change" change;
        [ "--copies" ];
        includes_args includes;
        excludes_args excludes;
        no_arg "--subrepos" subrepos;
      ])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ -> non_0_exit_error o
        | Ok () ->
          let entry_map (acc : File_status.t list) str =
            match String.split str ~on:' ' with
            | [ "" ]            -> acc
            | [ "M"; filename ] -> (Modified filename) :: acc
            | [ "A"; filename ] -> (Added filename) :: acc
            | [ "R"; filename ] -> (Removed filename) :: acc
            | [ "!"; filename ] -> (Missing filename) :: acc
            | [ "?"; filename ] -> (Not_tracked filename) :: acc
            | [ ""; ""; src ]   ->
              (match acc with
               | Added    dst :: acc -> Copied { src; dst = `New_file dst } :: acc
               | Modified dst :: acc -> Copied { src; dst = `Overwritten dst } :: acc
               | _ -> failwithf "unexpected hg output: '%s'" str ())
            | _ -> failwithf "unexpected hg output: '%s'" str ()
          in
          o.stdout
          |> String.split ~on:'\n'
          |> List.fold ~init:[] ~f:entry_map
          |> List.rev
          |> Ok)

  let root =
    command "root" (fun run () -> run [])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ -> non_0_exit_error o
        | Ok () -> Ok (String.strip o.stdout))

  let tags =
    command "tags" (fun run () -> run [])
      ~handle_output:(fun o ->
        match o.exit_status with
        | Error _ -> non_0_exit_error o
        | Ok () ->
          Ok (List.filter_map (String.split o.stdout ~on:'\n') ~f:Tag.of_line))

  let unbundle =
    command "unbundle"
      (fun run ?update path ->
         run [
           no_arg "--update" update;
           [path]
         ])
      ~handle_output:expect_0

  let update =
    command "update" (fun run ?clean ?check ?date ?rev () ->
      run [
        no_arg "--clean" clean;
        no_arg "--check" check;
        date_args date;
        rev_args rev;
      ])
      ~handle_output:expect_0
end

module Simple = Make (Simple)
module Async = Make (Async)
module Fixed_hg_environment (E : Hg_env) = Make (Fixed_hg_environment (E))
module Remote = Make (Remote)
