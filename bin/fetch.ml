(* Fetching *)
let rec dedup list =
  match list with
  | [] -> []
  | h::t -> if List.mem h (dedup t)
            then dedup t else h::(dedup t)

let fetch_array sources : (string * (string, string) Hashtbl.t) list =
  let fetch source : (string, string) Hashtbl.t = (
      match source with
      | "os" -> Files.retrieve_file "/etc/os-release" '='
                |> List.to_seq |> Hashtbl.of_seq
      | "kernel" -> let kernel_files = ["arch"; "hostname"; "osrelease"; "ostype"] in
                    let get_file f = Files.read_file ("/proc/sys/kernel/" ^ f) in
                    List.map (fun f -> (f, get_file f)) kernel_files
                    |> List.to_seq |> Hashtbl.of_seq
      | "board" -> let board_files = ["board_name"; "board_vendor"; "board_version"] in
                   let get_file f = Files.read_file ("/sys/devices/virtual/dmi/id/" ^ f) in
                   List.map (fun f -> (f, get_file f)) board_files
                   |> List.to_seq |> Hashtbl.of_seq
      | "host" -> [("name", Files.read_file "/etc/hostname")]
                                                  |> List.to_seq |> Hashtbl.of_seq
      | "uptime" -> let uptime_file = Files.read_file "/proc/uptime" in
                    let uptime = String.split_on_char ' ' uptime_file
                                 |> List.hd |> float_of_string |> int_of_float in
                    let hours = uptime / 3600 in
                    let mins = (uptime - hours * 3600) / 60 in
                    let secs = uptime - hours * 3600 - mins * 60 in
                    ["hours", string_of_int hours;
                     "mins", string_of_int mins;
                     "secs", string_of_int secs] |> List.to_seq |> Hashtbl.of_seq
      | "distro" -> let id = Files.retrieve_key "/etc/os-release" '=' "id" in
                    Distros.distro_of_id id
      (* cpu, mem *)
      | other -> raise (Invalid_argument other)
    ) in
  List.init (Array.length sources) (fun i -> (sources.(i), fetch sources.(i)))
