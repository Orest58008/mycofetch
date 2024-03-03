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
let () = List.iter2 (fun source word ->
             match source with
             | "" -> print_string word
             | source when List.mem_assoc source fetched ->
                print_string (Hashtbl.find (List.assoc source fetched) word)
             | _ -> print_string word
           ) sources words
