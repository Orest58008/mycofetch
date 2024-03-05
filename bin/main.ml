(* Deduping function *)
let rec dedup list : 'a list =
  match list with | [] -> []
                  | h::t -> let t_dedup = dedup t in
                            if List.mem h t_dedup then t_dedup else h::t_dedup

(* Get and process template *)
let template = (Sys.getenv "HOME") ^ "/.mftemplate" |> Files.read_file
let sources, words = Template.process_line template

(* Prefetch pm_count if needed *)
let pm_count = if List.mem "pm_count" words
               then let id = Files.retrieve_file "/etc/os-release" '=' |> List.assoc "id" in
                    let distro = Distros.distro_of_id id in
                    Domain.spawn (fun () -> Unix.open_process_in distro.pm.command
                                            |> In_channel.input_lines |> List.length )
               else Domain.spawn (fun () -> 0)

(* Fetch info *)
let fetched = List.filter (fun x ->
                  try let _ = Fetch.fetch x in true
                  with Invalid_argument _ -> false) sources
              |> dedup |> List.map (fun x -> x, Fetch.fetch x)

(* Print everything *)
let logo_n = ref 1
let result = String.length template |> Buffer.create
let result_append = Buffer.add_string result
let handle source word =
  match source with
  | "" -> result_append word
  | "distro" -> let id = Files.retrieve_file "/etc/os-release" '=' |> List.assoc "id" in
                let distro = Distros.distro_of_id id in
                (match word with
                 | "pm_name" -> result_append distro.pm.name
                 | "pm_command" -> result_append distro.pm.command
                 | "pm_count" -> let count = Domain.join pm_count in
                                 result_append (string_of_int (count + 1))
                 | "color" -> result_append distro.color
                 | "logo_tiny" -> result_append distro.logo_tiny
                 | "logo" -> let logo_line = if !logo_n < Array.length distro.logo
                                             then (distro.logo.(!logo_n) ^ "\x1b[0m")
                                             else (distro.logo.(0) ^ "\x1b[0m]") in
                             Template.style_line logo_line |> String.concat "" |> result_append;
                             logo_n := !logo_n + 1
                 | _ -> raise (Invalid_argument ("distro" ^ ":" ^ word))
                )
  | "env" -> result_append (Sys.getenv word)
  | source when List.mem_assoc source fetched ->
     let value = Hashtbl.find_opt (List.assoc source fetched) word in (
         match value with
         | Some v -> result_append v
         | None -> raise (Invalid_argument (source ^ ":" ^ word))
       )
  | _ -> raise (Invalid_argument (source ^ ":" ^ word))

(*
let () = List.iter (fun (x, y) -> print_endline ("\n" ^ (String.capitalize_ascii x));
                                  Hashtbl.iter (fun a b -> print_endline (a ^ ":" ^ b)) y)
           fetched
 *)
           
let () = List.iter2 handle sources words
let () = Buffer.output_buffer stdout result
