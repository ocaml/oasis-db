
(** Storage of packages and package's versions on disk 
  
    This module handle operations on the hierarchy of packages and package's
    versions available in oasis-db. 

    There are two flavors of this module. One is a read-only storage where you 
    cannot add element and one is a read-write.

    @author Sylvain Le Gall
  *)

(* Read-only storage *)
type 'a t constraint 'a = #ODBFilesystem.std_ro

(* Read-write storage *)
type rw_t = ODBFilesystem.std_rw t

type pkg_str = string
type ver_str = string

type filename = string
type dirname = string

exception FileDoesntExist 

module Pkg :
sig
  type file_type = 
      [ `Other of string 
      | `PluginData of string]

  type key = 
      [ `Str of pkg_str 
      | `Pkg of ODBPkg.t 
      | `PkgVer of ODBPkgVer.t]

  (** All available packages
    *)
  val elements : 'a t -> ODBPkg.t list Lwt.t
  
  (** Create a package 
    *)
  val create :
    ctxt:ODBContext.t -> rw_t -> ODBPkg.t ->
    (* TODO: rewrite *)
    (CalendarLib.Calendar.t *
     [> `Pkg of pkg_str * [> `Created ] ] * ODBPkg.t)
    Lwt.t

  (** Check the existence of a package
   *)
  val mem : 'a t -> key -> bool Lwt.t
                                     
  (** Get a specific package
    *)
  val find : 'a t -> key -> ODBPkg.t Lwt.t
  
  (** Get the directory name of a package 
    *)
  val dirname : 'a t -> key -> ODBTypes.dirname Lwt.t

  (** Get the relative filename of a package file_type
    *)
  val filename : 'a t -> key -> file_type -> string Lwt.t

  (** Open a package file_type for writing
    *)
  val with_file_out : 
    rw_t -> 
    key -> 
    file_type ->
    (Lwt_io.output Lwt_io.channel -> 'a Lwt.t) -> 
    'a Lwt.t

  (** Open a package file_type for reading 
    *)
  val with_file_in : 
    ?catch:(exn -> 'b Lwt.t) ->
    'a t -> 
    key -> 
    file_type -> 
    (Lwt_io.input Lwt_io.channel -> 'b Lwt.t) -> 
    'b Lwt.t

  (** Test package file_type existence
    *)
  val file_exists: 'a t -> key -> file_type -> bool Lwt.t
end

module PkgVer:
sig
  type file_type =
    [ `OASIS
    | `OASISPristine
    | `Other of ODBTypes.filename
    | `PluginData of string
    | `Tarball ]

  type key = 
      [ `Str of pkg_str * ver_str
      | `StrVer of pkg_str * OASISVersion.t
      | `PkgVer of ODBPkgVer.t]

  (** All available version of a package, beginning with the older
      one. You can add an extra elements, if needed.
    *)
  val elements :
    ?extra:ODBPkgVer.t ->
    'a t -> Pkg.key -> 
    ODBPkgVer.t list Lwt.t

  (** Check the existence of a package's version
   *)
  val mem : 'a t -> key -> bool Lwt.t

  (** Get a specific version
    *)
  val find : 'a t -> key -> ODBPkgVer.t Lwt.t

  (** Get the latest version
    *)
  val latest :
    ?extra:ODBPkgVer.t -> 
    'a t -> 
    Pkg.key -> 
    ODBPkgVer.t Lwt.t

  (** Create a package version 
    *)
  val create :
    ctxt:ODBContext.t -> rw_t ->
    ODBPkgVer.t ->
    Unix.file_descr ->
    (ODBTypes.date *
     [> `Pkg of pkg_str * [> `VersionCreated of ODBTypes.version ] ] *
     ODBPkgVer.t)
    Lwt.t

  (** Get the directory name of package's version
    *)
  val dirname : 'a t -> key -> dirname Lwt.t

  (** Get the the relative filename of a package's version file_type
    *)
  val filename : 'a t -> key -> file_type -> filename Lwt.t

  (** Open a package's version file_type for writing
    *)
  val with_file_out : 
    rw_t -> 
    key -> 
    file_type ->
    (Lwt_io.output Lwt_io.channel -> 'a Lwt.t) -> 
    'a Lwt.t

  (** Open a package's version file_type for reading
    *)
  val with_file_in :
    ?catch:(exn -> 'b Lwt.t) ->
    'a t ->
    key ->
    file_type ->
    (Lwt_io.input Lwt_io.channel -> 'b Lwt.t) -> 
    'b Lwt.t

  (** Test package's version file_type existence
    *)
  val file_exists: 'a t -> key -> file_type -> bool Lwt.t

  (** Get oasis *)
  val oasis: 'a t -> key -> (OASISTypes.package option) Lwt.t

  (** Get oasis status *)
  val oasis_status: 'a t -> key -> [`OK | `Not_found | `Error] Lwt.t
end

(** Create the datastructure, using the content of the filesystem 
  *)
val create :
  ctxt:ODBContext.t ->
  'a ->
  (timestamp:CalendarLib.Calendar.t -> ODBLog.event -> unit Lwt.t) ->
  ODBLog.t list -> 'a t Lwt.t

(** Access to the underlying filesystem 
  *)
val fs : 'a t -> 'a

(** Convert a read-write filesystem to a read-only one 
  *)
val to_ro : 'a t -> ODBFilesystem.std_ro t
