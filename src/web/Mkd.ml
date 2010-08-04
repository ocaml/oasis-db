
open Lwt
open Markdown
open XHTML.M

let mkd_dir = "src/web/mkd"

let load nm = 
  LwtExt.IO.with_file_content 
    (FilePath.concat mkd_dir (nm^".mkd")) 
  >>= fun txt ->
  return 
    (MarkdownHTML.to_html
       ~render_link:(fun href ->  
                       a ~a:[a_href (uri_of_string href.href_target)]
                         [pcdata href.href_desc])
       ~render_img:(fun image -> 
                      img
                        ~src:(uri_of_string image.img_src)
                        ~alt:image.img_alt
                        ())
       ~render_pre:(fun ~kind s -> pre [pcdata s])
       (Markdown.parse_text txt))


let protect txt = 
  MarkdownHTML.to_html
    ~render_link:(fun _ -> i [pcdata "link removed"])
    ~render_img:(fun _ -> i [pcdata "image removed"])
    ~render_pre:(fun ~kind s -> pre [pcdata s])
    (Markdown.parse_text txt)
