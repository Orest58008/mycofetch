let find array elem =
  let index_opt = Array.find_index (fun x -> x = elem) array in
  match index_opt with | Some i -> i | None -> 0

(* Config processing *)
let process_line ?(style=false) line : (string * string) list =
  let line = Str.global_replace (Str.regexp "$") "\x1b[0m" line in (* clear styling at each \n *)
  let fonts = [|"c"; "b"; "d"; "i"; "u"|] in (* clear, bold, dim, italic, underlined *)
  let colours = Array.init 8 (fun x -> "c" ^ string_of_int x) in
  let line_split = (String.split_on_char '#' line) in
  let rec process_chunk ?(style=false) chunk : string * string =
    if String.ends_with ~suffix:"\\" chunk then (* "\#" handling *)
      let chunk_trimmed = (String.sub chunk 0 (String.length chunk - 1)) in
      "", (chunk_trimmed ^ "#")
    else if chunk = "cauto" then (* cauto handling *)
      let distro = Files.retrieve_key "/etc/os-release" '=' "id" |> Distros.distro_of_id in
      process_chunk distro.color
    else if Array.mem chunk colours then
      let colour_num = find colours chunk in
      let colour = "\x1b[3" ^ (string_of_int colour_num) ^ "m" in
      "", colour
    else if Array.mem chunk fonts then
      let font_num = find fonts chunk in
      let font = "\x1b[3" ^ (string_of_int font_num) ^ "m" in
      "", font
    else if (String.contains chunk ':') && (not style) then
      let pair = String.split_on_char ':' chunk in
      let source = List.hd pair in
      let word = List.tl pair |> List.hd in
      source, word
    else
      "", chunk
  in List.map (process_chunk ~style:style) line_split
