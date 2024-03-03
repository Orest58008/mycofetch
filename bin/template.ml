let find array elem =
  let index_opt = Array.find_index (fun x -> x = elem) array in
  match index_opt with | Some i -> i | None -> 0

(* Config processing *)
let process_line ?(style=false) line : string list * string list =
  let fonts = [|"c"; "b"; "d"; "i"; "u"|] in (* clear, bold, dim, italic, underlined *)
  let colours = Array.init 8 (fun x -> "c" ^ string_of_int x) in
  let line_split = String.split_on_char '#' line  in
  let rec process_split split sources values = (
      match split with
      | [] -> (List.rev sources, List.rev values)
      | word::rest when String.ends_with ~suffix:"\\" word && List.length rest > 0
        -> (* "\#" processing *)
         let word_trimmed = (String.sub word 0 (String.length word - 1)) in
         process_split ((word_trimmed ^ "#")::rest) sources values
      | word::rest when Array.mem word colours -> (* colour processing *)
         let colour_num = find colours word in
         let colour = "\x1b[3" ^ (string_of_int colour_num) ^ "m" in
         process_split rest (""::sources) (colour::values)
      | word::rest when Array.mem word fonts -> (* font processing *)
         let font_num = find fonts word in
         let font = "\x1b[" ^ (string_of_int font_num) ^ "m" in
         process_split rest (""::sources) (font::values)
      | word::rest when ((String.contains word ':') && (not style)) -> (* pair processing *)
         let word_list = String.split_on_char ':' word in
         let source = List.hd word_list in
         let value = List.tl word_list |> List.hd in
         process_split rest (source::sources) (value::values)
      | word::rest -> (* ordinary word processing *)
         process_split rest (""::sources) (word::values)
    ) in process_split line_split [] []
