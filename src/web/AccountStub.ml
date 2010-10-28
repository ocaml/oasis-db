
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
open ODBGettext
open SQL

let cookiename = "account_ext_token" 

let () = 
  Random.self_init ()

let guess_user_id sp db = 
  let token = 
    try 
      let cookies = 
        Eliom_sessions.get_cookies sp
      in
      let res =
        Netencoding.Url.decode
          (Ocsigen_lib.String_Table.find 
             "account_ext_token"
             cookies)
      in
        Some res
    with Not_found ->
      None
  in
     begin
       match token with 
         | Some token ->
             PGSQL(db) 
               "SELECT user_id FROM account_ext WHERE token = $token AND expire > NOW ()"
             >>= fun lst ->
               begin
                 let rec find_non_opt =
                   function
                     | (Some e) :: _ -> Some (e, token)
                     | None :: tl -> find_non_opt tl
                     | [] -> None
                 in
                   return (find_non_opt lst)
               end

         | None ->
             return None
     end
     >>= fun res ->
     (* GC stuff *)
     PGSQL(db) 
       "DELETE FROM account_ext WHERE expire <= NOW ()"
     >>= fun () ->
     return res


let template ctnt =
  html
    (head (title (pcdata "3rd party login")) [])
    (body ctnt)
        
let default_redirect =
  register_new_service
    ~path:["account_ext_noredirect"]
    ~get_params:(string "message")
    (fun sp message () ->
       return 
         (template [p [pcdata message]]))

let redirect ~sp ?cookies msg =
  function 
    | Some url ->
        Eliom_predefmod.String_redirection.send
          ~sp
          ?cookies
          (uri_of_string url)

    | None ->
        Eliom_predefmod.Redirection.send
          ~sp
          ~options:`Temporary
          ?cookies
          (preapply default_redirect msg)

let users = 
  [
 (* id,  user_name, realname,   tz,     lang *)
    0l,  "admin1",  "Mr Foo",   "CEST",    1l;
    1l,  "admin2",  "Mr Bar",   "GMT",     1l;
    2l,  "user1",   "Foo user", "GMT",     1l;
    3l,  "user2",   "Bar user", "GMT",     1l;
  ]

let missing_params =
  Eliom_predefmod.Redirection.register_new_service
    ~path:["account_ext"]
    ~get_params:unit
    (fun sp () () ->
       return (preapply default_redirect "Missing parameters"))

let mk_cookie token = 
  Set 
    (None, 
     Some ((Unix.gettimeofday ()) +. 3600.),
     cookiename,
     token,
     false)

let login = 
  Eliom_predefmod.Any.register_new_post_service
    ~post_params:(int32 "user_id" ** string "action" ** opt (string "redirect"))
    ~fallback:missing_params
    (fun sp () (user_id, (action, redirect_opt)) -> 
       Lwt_pool.use SQL.pool
         (fun db ->
            (* Create a token *)
            let token = 
              let str = 
                String.make 30 '\000'
              in
                for i = 0 to (String.length str) - 1 do
                  str.[i] <- Char.chr(Random.int 256)
                done;
                Netencoding.Base64.encode str
            in
              PGSQL(db)
                "DELETE FROM account_ext WHERE user_id = $user_id"
              >>= fun () ->
              begin
                let (_, user_name, realname, tz, lang) = 
                  List.find 
                    (fun (id, _, _, _, _) -> id = user_id)
                    users
                in
                PGSQL(db) 
                  "INSERT INTO account_ext (token, user_id, expire, user_name, realname, timezone, language) \
                    VALUES ($token, $user_id, NOW () + interval '1 hour', $user_name, $realname, $tz, $lang)"
              end
              >>= fun () -> 
              redirect 
                ~sp 
                ~cookies:[mk_cookie token]
                "You are now logged in." 
                redirect_opt))

let action_no_userid sp _ redirect_opt act = 
  let dflt msg = 
    Eliom_predefmod.Xhtml.send sp (template [p [pcdata msg]])
  in
    match act with 
      | "login" ->
          let f = 
            post_form
              ~sp
              ~service:login
              (fun (user_id_nm, (action_nm, redirect_opt_nm)) ->
                 [p
                    [string_input ~input_type:`Hidden ~name:action_nm ~value:act ();
                     pcdata "Action: "; 
                     pcdata act; 
                     br ();

                     string_input ~input_type:`Hidden ~name:redirect_opt_nm ?value:redirect_opt ();
                     pcdata "Redirect: "; 
                     pcdata (match redirect_opt with | Some url -> url | None -> "(none)");
                     br ();
                     
                     begin
                       match users with 
                         | hd :: tl ->
                             let to_option selected (id, _, realname, _, _) = 
                               Option ([], id, Some (pcdata realname), selected)
                             in
                               int32_select ~name:user_id_nm
                                 (to_option true hd)
                                 (List.map (to_option false) tl)
                         | [] ->
                             pcdata ""
                     end;

                     string_input ~input_type:`Submit ~value:"Login" ();
                   ]
                 ]
              ) 
              ()
          in
            Eliom_predefmod.Xhtml.send sp (template [f])

      | "new" ->
          dflt "This page should display account creation of \
                http://forge.ocamlcore.org, but this is just a stub for test. \
                Use one of the fake account already created (no password required)."

      | "lost_passwd" ->
          dflt
            "This page should display lost password form of \
            http://forge.ocamlcore.org, but this is just a stub for test."

      | str ->
          dflt (Printf.sprintf "Unknown action: %s" str)

let action_userid sp db user_id token redirect_opt =
  function
    | "login" ->
        begin
          PGSQL(db) 
            "UPDATE account_ext SET expire = NOW () + interval '1 hour' WHERE user_id = $user_id"
          >>= fun () ->
          redirect 
            ~sp
            ~cookies:[mk_cookie token]
            "You are already logged in!"
            redirect_opt
        end

    | "logout" ->
        begin
          PGSQL(db)
            "DELETE FROM account_ext WHERE user_id = $user_id"
          >>= fun () ->
          redirect 
            ~sp
            ~cookies:[Unset (None, cookiename)]
            "You are now logged out."
            redirect_opt
        end

    | "manage" ->
        begin
          Eliom_predefmod.Xhtml.send
            ~sp
            (template 
               [p 
                  [pcdata "This page should display account as found on \
                     http://forge.ocamlcore.org, but this is just a stub for test."]])
        end

    | act ->
        begin
          action_no_userid sp db redirect_opt act
        end

let main =
  Eliom_predefmod.Any.register_new_service
    ~path:["account_ext"]
    ~get_params:(string "action" ** opt (string "redirect"))
    (fun sp (action, redirect_opt) () ->
       Lwt_pool.use SQL.pool
         (fun db ->
            guess_user_id sp db 
            >>= 
            begin
              function
                | Some (user_id, token) -> 
                    action_userid sp db user_id token redirect_opt action
                      
                | None -> 
                    action_no_userid sp db redirect_opt action
            end))
