
open XHTML.M

let to_html txt =
  MarkdownHTML.to_html
    ~render_link:
    (fun href -> 
       XHTML.M.a
         ~a:[a_href (uri_of_string href.Markdown.href_target)]
         [pcdata href.Markdown.href_desc])
    ~render_img:(fun _ -> i [pcdata "image removed"])
    ~render_pre:(fun ~kind s -> pre [pcdata s])
    (Markdown.parse_text txt)
