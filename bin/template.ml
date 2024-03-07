let colour_of_tag tag : string = match tag with
  | "c0" -> "\x1b[30m" | "c1" -> "\x1b[31m" | "c2" -> "\x1b[32m" | "c3" -> "\x1b[33m"
  | "c4" -> "\x1b[34m" | "c5" -> "\x1b[35m" | "c6" -> "\x1b[36m" | "c7" -> "\x1b[37m"
  | _ -> ""

let font_of_tag tag : string = match tag with
  | "c" -> "\x1b[0m" | "b" -> "\x1b[1m" | "d" -> "\x1b[2m" | "i" -> "\x1b[3m" | "u" -> "\x1b[4m"
  | _ -> ""
  

let process_line ~logo ?(style=false) line : (string * string) list =
  let line = Str.global_replace (Str.regexp "$") "\x1b[0m" line in (* clear styling at each \n *)
  let line_split = (String.split_on_char '#' line) in
  let rec process_chunk ?(style=false) chunk : string * string =
    if String.ends_with ~suffix:"\\" chunk then (* "\#" handling *)
      let chunk_trimmed = (String.sub chunk 0 (String.length chunk - 1)) in
      "", chunk_trimmed ^ "#"
    else if chunk = "cauto" then (* cauto handling *)
      let distro = if logo <> "" then logo else Files.retrieve_key "/etc/os-release" '=' "id" in
      process_chunk (Distros.distro_of_id distro).colour
    else if colour_of_tag chunk <> "" then
      "", colour_of_tag chunk
    else if font_of_tag chunk <> "" then
      "", font_of_tag chunk
    else if (String.contains chunk ':') && (not style) then
      let pair = String.split_on_char ':' chunk in
      let source = List.hd pair in
      let word = List.tl pair |> List.hd in
      source, word
    else
      "", chunk
  in List.map (process_chunk ~style:style) line_split
