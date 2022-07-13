open Hg_private

module type S = sig
  type 'a with_args
  type 'a output

  val add
    : (?includes:string list
       -> ?excludes:string list
       -> [ `All_files | `These_files of string list ]
       -> unit output)
        with_args

  val addremove
    : (?includes:string list
       -> ?excludes:string list
       -> ?similarity:int
       -> [ `All_files | `These_files of string list ]
       -> unit output)
        with_args

  val annotate
    : (?rev:string
       -> ?user:unit
       -> ?file:unit
       -> ?date:unit
       -> ?number:unit
       -> ?changeset:unit
       -> ?skip:string list
       -> ?ignore_space_change:unit
       -> ?ignore_blank_lines:unit
       -> ?ignore_space_at_eol:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> ?template:string
       -> string
       -> string list output)
        with_args

  val archive
    : (?no_decode:unit
       -> ?prefix:string
       -> ?rev:string
       -> ?type_:string
       -> ?subrepos:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> destination:string
       -> unit
       -> unit output)
        with_args

  (* Calls `hg bookmarks` with no argument (which lists all bookmarks). *)
  val bookmarks : (unit -> Bookmark.t list output) with_args

  (* Calls `hg bookmarks` with an anonymous argument (which creates or modifies a bookmark
     with that name). *)
  val change_bookmark
    : (?force:unit
       -> name:
            string
       (* These variants correspond to flags with similar names. [`Current] means none of
          those flags.

          Most combinations of these flags are invalid or useless (e.g. hg disallows passing
          --rev and --delete together, and --rev does the same thing whether or not you pass
          --inactive), so this disallows combinations.
       *)
       -> [ `Set_rev of string | `Current | `Inactive | `Delete | `Rename of string ]
       -> unit output)
        with_args

  val bundle
    : (?force:unit
       -> ?revs:string list
       -> ?branches:string list
       -> ?bases:string list
       -> ?all:unit
       -> ?compression_type:string (* corresponds to [--type] *)
       -> ?ssh:string
       -> ?remotecmd:string
       -> ?insecure:unit
       -> ?destination:string
       -> string
       -> [ `Ok | `Nothing_to_bundle ] output)
        with_args

  val clone
    : (source:string
       -> ?destination:string
       -> ?update:[ `Rev of string | `No_update ]
       -> ?revs:string list
       -> ?branches:string list
       -> ?pull:unit
       -> ?uncompressed:unit
       -> ?ssh:string
       -> ?remotecmd:string
       -> ?insecure:unit
       -> unit
       -> unit output)
        with_args

  val cat
    (* [destination] cannot go inside the [with_args] parameter due to an issue with the
       value restriction.  This should have little effect on users. *)
    :  destination:'dst Destination.t
    -> (?includes:string list
        -> ?excludes:string list
        -> ?rev:string
        -> ?template:string
        -> string
        -> [ `Ok of 'dst | `No_such_file ] output)
         with_args

  val commit
    : (?addremove:unit
       -> ?allow_commit_without_bookmark:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> message:string
       -> ?time:Time.t (** Defaults to the current time. *)
       -> ?zone:Time.Zone.t
       (** Mercurial commit times require an explicit UTC offset. Defaults to the local
           time zone. *)
       -> ?user:string
       -> ?files:string list
       -> unit
       -> [ `Ok | `Nothing_changed ] output)
        with_args

  val config
    : (?untrusted:unit -> ?names:string list -> unit -> (string * string) list output)
        with_args

  val copy
    : (?forget:unit
       -> ?after:unit
       -> ?force:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> string
       -> string
       -> unit output)
        with_args

  val diff
    : (?revs:string list
       -> ?change:string
       -> ?text:unit
       -> ?git:unit
       -> ?reverse:unit
       -> ?ignore_all_space:unit
       -> ?ignore_space_change:unit
       -> ?ignore_blank_lines:unit
       -> ?unified:int
       -> ?stat:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> ?subrepos:unit
       -> ?files:string list
       -> unit
       -> string output)
        with_args

  val extdiff
    : (?revs:string list
       -> ?change:string
       -> ?includes:string list
       -> ?excludes:string list
       -> ?program:string
       -> ?options:string list
       -> ?files:string list
       -> unit
       -> string output)
        with_args

  val files
    : (?rev:string
       -> ?includes:string list
       -> ?excludes:string list
       -> ?subrepos:unit
       -> [ `All_files | `These_files of string list ]
       -> string list output)
        with_args

  val heads
    : (?rev:string
       -> ?topo:unit
       -> ?closed:unit
       -> ?include_files_in_changeset_info:unit
       -> unit
       -> Changeset_info.t list output)
        with_args

  (* Despite the name, this actually calls `hg log --rev $rev --template {node}` to get
     the 40-character revision hash. The expectation is that this is what most
     programmatic calls to `hg id` actually want from it. *)
  val id : (?rev:string -> unit -> string output) with_args

  val init
    : (?ssh:string
       -> ?remotecmd:string
       -> ?insecure:unit
       -> ?dest:string
       -> unit
       -> unit output)
        with_args

  (* Not a real hg command -- this just runs `hg root` and returns true on success and
     false on error. *)
  val is_repo : (unit -> bool output) with_args

  val log
    : (?follow:unit
       -> ?date:Date_param.t
       -> ?copies:unit
       -> ?keywords:string list
       -> ?revs:string list
       -> ?removed:unit
       -> ?users:string list
       -> ?branches:string list
       -> ?prune_revs:string list
       -> ?limit:int
       -> ?no_merges:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> ?files:string list
       -> ?include_files_in_changeset_info:unit
       -> unit
       -> Changeset_info.t list output)
        with_args

  val manifest : (?rev:string -> ?all:unit -> unit -> string list output) with_args

  val merge
    : (?tool:string
       -> ?allow_commit_without_bookmark:unit
       -> [ `Unique_other_head | `Rev of string ]
       -> [ `Ok | `Unresolved_files ] output)
        with_args

  val out
    : (?force:unit
       -> ?revs:string list
       -> ?limit:int
       -> ?no_merges:unit
       -> ?ssh:string
       -> ?remotecmd:string
       -> ?insecure:unit
       -> ?remote_path:string
       -> ?include_files_in_changeset_info:unit
       -> unit
       -> Changeset_info.t list output)
        with_args

  val pull
    : (?update:unit
       -> ?force:unit
       -> ?revs:string list
       -> ?bookmarks:string list
       -> ?branches:string list
       -> ?ssh:string
       -> ?remotecmd:string
       -> ?insecure:unit
       -> ?rebase:unit
       -> ?remote_path:string
       -> unit
       -> unit output)
        with_args

  val purge
    : (?abort_on_err:unit
       -> ?all:unit
       -> ?dirs:unit
       -> ?files:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> unit
       -> unit output)
        with_args

  val push
    : (?force:unit
       -> ?revs:string list
       -> ?bookmarks:string list
       -> ?branches:string list
       -> ?new_branch:unit
       -> ?ssh:string
       -> ?remotecmd:string
       -> ?insecure:unit
       -> ?remote_path:string
       -> unit
       -> [ `Ok | `Nothing_to_push ] output)
        with_args

  val remove
    : (?after:unit
       -> ?force:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> string list
       -> unit output)
        with_args

  val rename
    : (?after:unit
       -> ?force:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> string
       -> string
       -> unit output)
        with_args

  (** Calls [hg resolve] with [--mark] to mark files as resolved. *)
  val mark_resolved
    : ([ `All_files | `These_files of string list ] -> unit output) with_args

  val revert
    : (?date:Date_param.t
       -> ?rev:string
       -> ?no_backup:unit
       -> ?includes:string list
       -> ?excludes:string list
       -> [ `All_files | `These_files of string list ]
       -> unit output)
        with_args

  val root : (unit -> string output) with_args

  val share
    : (?noupdate:unit
       -> ?bookmarks:unit
       -> src:string
       -> dst:string
       -> unit
       -> unit output)
        with_args

  (* This gives the default output of `hg status`, when not specifying flags like
     `--clean`, which allows it to have a return type reflecting this. If you want to
     specify more flags, please add another function rather than modifying this one.

     By default, this shows changes between the current revision and the working
     directory. Passing [~rev] shows the changes between that revision and the working
     directory. Passing both [~rev] and [~rev2] shows the changes between those revisions.
  *)
  val status
    : (?rev:string
       -> ?rev2:string
       -> ?change:string
       -> ?includes:string list
       -> ?excludes:string list
       -> ?subrepos:unit
       -> unit
       -> File_status.t list output)
        with_args

  val tags : (unit -> Tag.t list output) with_args
  val unbundle : (?update:unit -> string -> unit output) with_args

  val update
    : (?clean:unit
       -> ?check:unit
       -> ?date:Date_param.t
       -> ?rev:string
       -> unit
       -> unit output)
        with_args
end

module Make_s (A : Hg_lib_factory.Arg) = struct
  module type S =
    S with type 'a with_args := 'a A.With_args.t with type 'a output := 'a A.Output.t
end

module type Hg = sig
  module type S = S

  include Hg_lib_factory.Make_lib(Make_s).S
end
