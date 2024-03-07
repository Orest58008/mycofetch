(* Files *)
let strip str : string =
  let len = String.length str in
  let str = String.mapi (fun i x -> if x = '"' && (i = 0 || i = len - 1) then ' ' else x) str in
  let str = String.trim str in str

let read_lines path : string list =
  In_channel.open_text path |> In_channel.input_lines

let read_line path : string =
  In_channel.open_text path |> In_channel.input_line
  |> Option.fold ~none:"" ~some:(fun x -> x)

let read_file path : string =
  In_channel.open_text path |> In_channel.input_all

let retrieve_file path sep : (string * string) list =
  List.map (fun line ->
      let line_split = if line <> ""
                       then String.split_on_char sep line
                       else ["";""] in
      let key = List.hd line_split |> String.lowercase_ascii |> strip in
      let data = List.tl line_split |> List.hd |> strip in
      (key, data) ) (read_lines path)

let retrieve_key path sep target : string =
  let rec process_lines lines = (
      match lines with
      | [] -> raise (Failure (path ^ " does not contain " ^ target))
      | line::rest -> let line_split = String.split_on_char sep line in
                      let key = List.hd line_split |> String.lowercase_ascii in
                      let data = List.tl line_split |> List.hd |> strip in
                      if key = target then data else process_lines rest
    ) in process_lines (read_lines path)
