(* Deduping function *)
let rec dedup list : 'a list =
  match list with | [] -> []
                  | h::t -> let t_dedup = dedup t in
                            if List.mem h t_dedup then t_dedup else h::t_dedup
                       
(* Process config *)
let template_path = (Sys.getenv "HOME") ^ "/mftemplate"
let sources, words = Files.read_file template_path |> Template.process_line
let fetched = List.filter (fun x ->
                  try let _ = Fetch.fetch x in true
                  with Invalid_argument _ -> false) sources
              |> dedup |> List.map (fun x -> x, Fetch.fetch x)

(* Print everything *)
let logo_counter = ref 0
let result = Files.read_file template_path |> String.length |> Buffer.create
let handle source word =
  match source with
  | "" -> Buffer.add_string result word
  | "distro" -> let os = Files.retrieve_file "/etc/os-release" '=' in
                let id = List.assoc "id" os in
                let distro = Distros.distro_of_id id in
                (match word with
                 | "pm_name" -> Buffer.add_string result distro.pm.name
                 | "pm_command" -> Buffer.add_string result distro.pm.command
                 | "pm_count" -> let count = Unix.open_process_in distro.pm.command
                                             |> In_channel.input_lines |> List.length in
                                 Buffer.add_string result (string_of_int count)
                 | "color" -> Buffer.add_string result distro.color
                 | "logo_tiny" -> Buffer.add_string result distro.logo_tiny
                 | "logo" -> (distro.logo.(!logo_counter) ^ "\x1b[0m")
                             |> Template.style_line |> String.concat ""
                             |> Buffer.add_string result;
                             logo_counter := !logo_counter + 1
                 | _ -> raise (Invalid_argument word)
                )
  | "env" -> Buffer.add_string result (Sys.getenv word)
  | source when List.mem_assoc source fetched ->
     Buffer.add_string result (Hashtbl.find (List.assoc source fetched) word)
  | _ -> raise (Invalid_argument (source ^ ":" ^ word))

let () = List.iter2 handle sources words
let () = Buffer.output_buffer stdout result
