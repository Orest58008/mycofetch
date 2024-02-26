(* Files *)
let strip str : string =
  let str = Str.global_replace (Str.regexp {|^"\|"$|}) "" str in
  let str = Str.global_replace (Str.regexp {|^ +\| +$|}) "" str in
  str

let read_lines path : string list =
  In_channel.open_text path |> In_channel.input_lines

let read_line path : string =
  In_channel.open_text path |> In_channel.input_all

let process_file path sep : (string * string) list =
  let rec process_lines lines = (
    match lines with
    | [] -> []
    | line::rest -> let line_split = String.split_on_char sep line in
                    let key = List.hd line_split |> String.lowercase_ascii in
                    let data = List.tl line_split |> List.hd |> strip in
                    (key, data)::(process_lines rest)
    ) in process_lines (read_lines path)

let process_file_key path sep target : (string * string) =
  let rec process_lines lines = (
      match lines with
      | [] -> raise (Failure (path ^ " does not contain " ^ target))
      | line::rest -> let line_split = String.split_on_char sep line in
                      let key = List.hd line_split |> String.lowercase_ascii in
                      let data = List.tl line_split |> List.hd |> strip in
                      if key = target then (key, data) else process_lines rest
    ) in process_lines (read_lines path)
