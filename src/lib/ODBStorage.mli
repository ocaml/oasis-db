
(** Storage of packages and package's versions on disk 
  
    This module handle operations on the hierarchy of packages and package's
    versions available in oasis-db. 

    There are two flavors of this module. One is a read-only storage where you 
    cannot add element and one is a read-write.

    @author Sylvain Le Gall
  *)

type 'a t constraint 'a = #ODBVFS.read_only

(* Read-only storage *)
type 'a read_only = (#ODBVFS.read_only as 'a) t

(* Read-write storage *)
type 'a read_write = (#ODBVFS.read_write as 'a) t

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
  val elements : 'a read_only -> ODBPkg.t list Lwt.t
  
  (** Create a package 
    *)
  val create : 'a read_write -> ODBPkg.t -> ODBPkg.t Lwt.t

  (** Check the existence of a package
   *)
  val mem : 'a read_only -> key -> bool Lwt.t
                                     
  (** Get a specific package
    *)
  val find : 'a read_only -> key -> ODBPkg.t Lwt.t
  
  (** Get the directory name of a package 
    *)
  val dirname : 'a read_only -> key -> dirname

  (** Get the relative filename of a package file_type
    *)
  val filename : 'a read_only -> key -> file_type -> string Lwt.t

  (** Open a package file_type for writing
    *)
  val with_file_out : 
    'a read_write -> 
    key -> 
    file_type ->
    (Lwt_io.output Lwt_io.channel -> 'a Lwt.t) -> 
    'a Lwt.t

  (** Open a package file_type for reading 
    *)
  val with_file_in : 
    ?catch:(exn -> 'b Lwt.t) ->
    'a read_only -> 
    key -> 
    file_type -> 
    (Lwt_io.input Lwt_io.channel -> 'b Lwt.t) -> 
    'b Lwt.t

  (** Test package file_type existence
    *)
  val file_exists: 'a read_only -> key -> file_type -> bool Lwt.t
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
  val elements : 'a read_only -> Pkg.key -> ODBPkgVer.t list Lwt.t

  (** Check the existence of a package's version
   *)
  val mem : 'a read_only -> key -> bool Lwt.t

  (** Get a specific version
    *)
  val find : 'a read_only -> key -> ODBPkgVer.t Lwt.t

  (** Replace a version 
    *)
  val replace : 'a read_write -> key -> ODBPkgVer.t -> unit Lwt.t

  (** Get the latest version
    *)
  val latest : 'a read_only -> Pkg.key -> ODBPkgVer.t Lwt.t

  (** Create a package version 
    *)
  val create : 'a read_write -> ODBPkgVer.t -> Lwt_io.input_channel -> ODBPkgVer.t Lwt.t

  (** Get the directory name of package's version
    *)
  val dirname : 'a read_only -> key -> dirname 

  (** Get the relative filename of a package's version file_type
    *)
  val filename : 'a read_only -> key -> file_type -> filename Lwt.t

  (** Open a package's version file_type for writing
    *)
  val with_file_out : 
    'a read_write -> 
    key -> 
    file_type ->
    (Lwt_io.output Lwt_io.channel -> 'b Lwt.t) -> 
    'b Lwt.t

  (** Open a package's version file_type for reading
    *)
  val with_file_in :
    ?catch:(exn -> 'b Lwt.t) ->
    'a read_only ->
    key ->
    file_type ->
    (Lwt_io.input Lwt_io.channel -> 'b Lwt.t) -> 
    'b Lwt.t

  (** Test package's version file_type existence
    *)
  val file_exists: 'a read_only -> key -> file_type -> bool Lwt.t

  (** Get oasis *)
  val oasis: 'a read_only -> key -> (OASISTypes.package option) Lwt.t

  (** Get oasis status *)
  val oasis_status: 'a read_only -> key -> [`OK | `Not_found | `Error] Lwt.t

  (** Create a package's version using another version *)
  val derive: 'a read_write -> key -> OASISVersion.t -> unit Lwt.t

  (** Rebuild tarball using _oasis *)
  val update_tarball: 'a read_write -> key -> unit Lwt.t

  (** Remove a package's version *)
  val remove: 'a read_write -> key -> unit Lwt.t
end

type watch = CalendarLib.Calendar.t -> ODBLog.event -> unit Lwt.t

(** Create the datastructure, using the content of the filesystem 
  *)
val create_read_only : ctxt:ODBContext.t -> 'a -> 'a read_only Lwt.t

(** Create the datastructure, using the content of the filesystem 
  *)
val create_read_write : ctxt:ODBContext.t -> ?watchers:(watch list) -> 'a -> 'a read_write Lwt.t

(** Go through all packages and generate matching events for watchers
  *)
val scan : 'a read_only -> unit Lwt.t

(** Access to the underlying filesystem 
  *)
val fs : 'a t -> 'a

(** Convert a read-write filesystem to a read-only one 
  *)
val to_ro : 'a read_write -> ODBVFS.read_only read_only
