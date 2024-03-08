(* Local config *)
let config: Config.t = Config.get_config (Sys.argv |> Array.to_list) Config.empty

(* Print help and exit *)
let () = if config.help then Config.print_help ()

(* Get and process template *)
let template = if config.template <> "" && config.marshal_path <> ""
               then config.template else config.template_path |> Files.read_file
let template_processed = if config.marshal_path <> "" && not config.marshal_compile
                         then In_channel.open_text config.marshal_path |> Marshal.from_channel
                         else Template.process_line ~logo:config.logo template
let () = if config.marshal_compile
         then Marshal.to_channel (Out_channel.open_text config.marshal_path) template_processed []

(* Prefetch pm_count if needed *)
let pm_count = if List.mem ("distro", "pm_count") template_processed
               then let id = Files.retrieve_file "/etc/os-release" '=' |> List.assoc "id" in
                    let distro = Distros.distro_of_id id in
                    Domain.spawn (fun () -> Unix.open_process_in distro.pm.command
                                            |> In_channel.input_lines |> List.length )
               else Domain.spawn (fun () -> 0)

(* Fetch everything *)
let sources = List.map (fun (x,_) -> x) template_processed 
let fetched = List.filter (fun x ->
                  try let _ = Fetch.fetch x in true
                  with Invalid_argument _ -> false) sources
              |> (fun xs -> let uniq_cons x xs = if List.mem x xs then xs else x::xs
                            in List.fold_right uniq_cons xs [])
              |> List.map (fun x -> x, Domain.spawn (fun() -> Fetch.fetch x))

(* Print everything *)
let logo_n = ref 1
let result = String.length template |> Buffer.create
let result_append = Buffer.add_string result

let handle (source, word) : unit =
  match source with
  | "" -> result_append word
  | "distro" -> let id = Files.retrieve_file "/etc/os-release" '=' |> List.assoc "id" in
                let distro = Distros.distro_of_id id in
                (match word with
                 | "pm_name" -> result_append distro.pm.name
                 | "pm_command" -> result_append distro.pm.command
                 | "pm_count" -> let count = Domain.join pm_count in
                                 result_append (string_of_int (count + 1))
                 | "colour" -> result_append distro.colour
                 | "logo_tiny" -> result_append distro.logo_tiny
                 | "logo" -> let logo = if config.logo <> ""
                                        then (Distros.distro_of_id config.logo).logo
                                        else distro.logo in
                             let logo_line = if !logo_n < Array.length logo
                                             then logo.(!logo_n) ^ "\x1b[0m"
                                             else logo.(0) ^ "\x1b[0m]" in
                             Template.process_line ~logo:config.logo ~style:true logo_line
                             |> List.map (fun (_, y) -> y) |> String.concat "" |> result_append;
                             logo_n := !logo_n + 1
                 | _ -> raise (Invalid_argument ("distro" ^ ":" ^ word))
                )
  | "env" -> result_append (Sys.getenv word)
  | source when List.mem_assoc source fetched ->
     let value = Hashtbl.find_opt (List.assoc source fetched |> Domain.join) word in (
         match value with
         | Some v -> result_append v
         | None -> raise (Invalid_argument (source ^ ":" ^ word))
       )
  | _ -> raise (Invalid_argument (source ^ ":" ^ word))
           
let () = List.iter handle template_processed
let () = Buffer.output_buffer stdout result
