
open ODBGettext
open ODBTypes
open CalendarLib

TYPE_CONV_PATH "ODBLog"

type pkg_event =
    [ `Created
    | `Deleted
    | `UscanChanged
    | `Rated
    | `Commented
    | `VersionCreated of version
    | `VersionDeleted of version
    ] with sexp 

type sys_event_level =
    [ `Fatal
    | `Error
    | `Warning
    | `Notice 
    | `Info 
    | `Debug
    ] with sexp

type sys_event = 
    [ `Started
    | `Stopped
    | `Failure of string 
    | `Message of sys_event_level * string
    ] with sexp

type event =
    [ `Pkg of string * pkg_event
    | `Sys of string * sys_event
    ] with sexp 

type t = 
    {
      log_id:        int; 
      log_timestamp: date;
      log_event:     event;
    } 

let to_string t = 
  let spf fmt = 
    Printf.sprintf fmt 
  in
  let sov =
    OASISVersion.string_of_version 
  in
    match t.log_event with 
      | `Pkg (pkg, se) ->
          begin
            match se with 
              | `Created ->
                  spf "package %s created" pkg
              | `Deleted ->
                  spf "package %s deleted" pkg
              | `UscanChanged ->
                  spf "uscan of package %s changed" pkg
              | `Rated ->
                  spf "package %s rated" pkg 
              | `Commented ->
                  spf "package %s commented" pkg 
              | `VersionCreated ver ->
                  spf "package %s version %s created" pkg (sov ver)
              | `VersionDeleted ver ->
                  spf "package %s version %s deleted" pkg (sov ver)
          end

      | `Sys (sys, se) ->
          begin
            match se with 
              | `Started ->
                  spf "Subsystem %s started" sys
              | `Stopped ->
                  spf "Subsystem %s stopped" sys
              | `Failure msg ->
                  spf "Subsystem %s failed with message '%s'" sys msg
              | `Message (lvl, str) ->
                  begin
                    let lvl_str =
                      match lvl with
                        | `Fatal   -> "Fatal"
                        | `Error   -> "Error"
                        | `Warning -> "Warning"
                        | `Notice  -> "Notice"
                        | `Info    -> "Info" 
                        | `Debug   -> "Debug"
                    in
                      spf "%s: %s" lvl_str str
                  end
          end
