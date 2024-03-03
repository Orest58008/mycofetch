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
let () = List.iter2 (fun source word ->
             match source with
             | "" -> print_string word
             | "distro" -> let os = Files.retrieve_file "/etc/os-release" '=' in
                           let id = List.assoc "id" os in
                           let distro = Distros.distro_of_id id in
                           (match word with
                            | "pm_name" -> print_string distro.pm.name
                            | "pm_command" -> print_string distro.pm.command
                            | "color" -> print_string distro.color
                            | "logo_tiny" -> print_string distro.logo_tiny
                            | "logo" -> (distro.logo.(!logo_counter) ^ "\x1b[0m")
                                        |> Template.style_line |> String.concat ""
                                        |> print_string;
                                        logo_counter := !logo_counter + 1
                            | _ -> raise (Invalid_argument word)
                           )
             | source when List.mem_assoc source fetched ->
                print_string (Hashtbl.find (List.assoc source fetched) word)
             | _ -> print_string word
           ) sources words
