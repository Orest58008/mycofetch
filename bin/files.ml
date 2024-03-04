(* Files *)
let strip str : string =
  let str = Str.global_replace (Str.regexp {|^"\|"$|}) "" str in
  let str = Str.global_replace (Str.regexp {|^ +\| +$|}) "" str in
  let str = Str.global_replace (Str.regexp {|^	+\|	+$|}) "" str in
  str

let read_lines path : string list =
  In_channel.open_text path |> In_channel.input_lines

let read_file path : string =
  In_channel.open_text path |> In_channel.input_all

let retrieve_file path sep : (string * string) list =
  let rec process_lines lines = (
    match lines with
    | [] -> []
    | line::rest -> let line_split = if line <> ""
                                     then String.split_on_char sep line
                                     else ["";""] in
                    let key = List.hd line_split |> String.lowercase_ascii |> strip in
                    let data = List.tl line_split |> List.hd |> strip in
                    (key, data)::(process_lines rest)
    ) in process_lines (read_lines path)

let retrieve_key path sep target : string =
  let rec process_lines lines = (
      match lines with
      | [] -> raise (Failure (path ^ " does not contain " ^ target))
      | line::rest -> let line_split = String.split_on_char sep line in
                      let key = List.hd line_split |> String.lowercase_ascii in
                      let data = List.tl line_split |> List.hd |> strip in
                      if key = target then data else process_lines rest
    ) in process_lines (read_lines path)
