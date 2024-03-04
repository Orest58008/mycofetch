let find array elem =
  let index_opt = Array.find_index (fun x -> x = elem) array in
  match index_opt with | Some i -> i | None -> 0

(* Config processing *)
let process_line line : string list * string list =
  let line = Str.global_replace (Str.regexp "$") "\x1b[0m" line in (* clear styling at each '\n' *)
  let line = Str.global_replace (Str.regexp "#+") "#" line in (* replace any number of '#' with one '#' *)
  let fonts = [|"c"; "b"; "d"; "i"; "u"|] in (* clear, bold, dim, italic, underlined *)
  let colours = Array.init 8 (fun x -> "c" ^ string_of_int x) in
  let line_split = (String.split_on_char '#' line) in
  let rec process_split split sources values = (
      match split with
      | [] -> List.rev sources, List.rev values
      | word::rest ->
         if String.ends_with ~suffix:"\\" word && List.length rest > 0 then (* '\#' processing *)
           let word_trimmed = (String.sub word 0 (String.length word - 1)) in
           process_split ((word_trimmed ^ "#")::rest) sources values
         else if word = "cauto" then (* automatic colour detection *)
           let os = Files.retrieve_file "/etc/os-release" '=' in
           let id = List.assoc "id" os in
           let distro = Distros.distro_of_id id in
           process_split (distro.color::rest) sources values
         else if Array.mem word colours then (* colour processing *)
           let colour_num = find colours word in
           let colour = "\x1b[3" ^ (string_of_int colour_num) ^ "m" in
           process_split rest (""::sources) (colour::values)
         else if Array.mem word fonts then (* font processing *)
           let font_num = find fonts word in
           let font = "\x1b[" ^ (string_of_int font_num) ^ "m" in
           process_split rest (""::sources) (font::values)
         else if (String.contains word ':') then (* pair processing *)
           let word_list = String.split_on_char ':' word in
           let source = List.hd word_list in
           let value = List.tl word_list |> List.hd in
           process_split rest (source::sources) (value::values)
         else
           process_split rest (""::sources) (word::values)
    ) in process_split line_split [] []

let style_line line : string list =
  let line = Str.global_replace (Str.regexp "$") "\x1b[0m" line in (* clear styling at each '\n' *)
  let fonts = [|"c"; "b"; "d"; "i"; "u"|] in (* clear, bold, dim, italic, underlined *)
  let colours = Array.init 8 (fun x -> "c" ^ string_of_int x) in
  let line_split = String.split_on_char '#' line  in
  let rec process_split split values = (
      match split with
      | [] -> List.rev values
      | word::rest ->
         if String.ends_with ~suffix:"\\" word && List.length rest > 0 then (* '\#' processing*)
           let word_trimmed = (String.sub word 0 (String.length word - 1)) in
           process_split ((word_trimmed ^ "#")::rest) values
         else if word = "cauto" then (* automatic colour detection *)
           let os = Files.retrieve_file "/etc/os-release" '=' in
           let id = List.assoc "id" os in
           let distro = Distros.distro_of_id id in
           process_split (distro.color::rest) values
         else if Array.mem word colours then (* colour processing *)
           let colour_num = find colours word in
           let colour = "\x1b[3" ^ (string_of_int colour_num) ^ "m" in
           process_split rest (colour::values)
         else if Array.mem word fonts then (* font processing *)
           let font_num = find fonts word in
           let font = "\x1b[" ^ (string_of_int font_num) ^ "m" in
           process_split rest (font::values)
         else
           process_split rest (word::values)
    ) in process_split line_split []
