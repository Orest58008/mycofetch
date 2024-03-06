(* Fetching *)
let strip str : string =
  let str = Str.global_replace (Str.regexp "\n") "" str in
  str

let fetch source : (string, string) Hashtbl.t =
    match source with
    | "os" -> Files.retrieve_file "/etc/os-release" '='
              |> List.to_seq |> Hashtbl.of_seq
    | "kernel" -> let kernel_files = ["arch"; "hostname"; "osrelease"; "ostype"] in
                  let get_file f = Files.read_file ("/proc/sys/kernel/" ^ f) in
                  List.map (fun f -> (f, get_file f |> strip)) kernel_files
                  |> List.to_seq |> Hashtbl.of_seq
    | "board" -> let board_files = ["name"; "vendor"; "version"] in
                 let get_file f = Files.read_file ("/sys/devices/virtual/dmi/id/board_" ^ f) in
                 List.map (fun f -> (f, get_file f |> strip)) board_files
                 |> List.to_seq |> Hashtbl.of_seq
    | "host" -> [("name", Files.read_file "/etc/hostname" |> strip)]
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
    | "cpu" -> let processor = ref "" in
               let cpu = Hashtbl.create 20 in
               let () = List.iter (fun (key, data) ->
                            match key with
                            | "processor" -> processor := data
                            | "vendor_id" -> Hashtbl.add cpu "vendor" data
                            | "model" -> Hashtbl.add cpu "model" data
                            | "model name" -> Hashtbl.add cpu "name" data
                            | "siblings" -> Hashtbl.add cpu "threads" data
                            | "cpu cores" -> Hashtbl.add cpu "cores" data
                            | "cpu mhz" -> let ghz = float_of_string data in
                                           Hashtbl.add cpu ("mhz_" ^ !processor) data;
                                           Hashtbl.add cpu ("ghz_" ^ !processor)
                                             (string_of_float ghz);
                            | _ -> ()
                          ) (Files.retrieve_file "/proc/cpuinfo" ':') in cpu
    | "mem" -> let memtotal, memfree, swaptotal, swapfree = ref 0, ref 0, ref 0, ref 0 in
               let mem = Hashtbl.create 18 in
               let mem_add name value =
                 Hashtbl.add mem (name ^ "_kb") (string_of_int value);
                 Hashtbl.add mem (name ^ "_mb") (string_of_int (value / 1024));
                 Hashtbl.add mem (name ^ "_gb") (string_of_int (value / 1048576)); in
               let () = List.iter (fun (key, data) ->
                            let data = Str.global_replace (Str.regexp ".kB") "" data
                                       |> int_of_string in
                            match key with
                            | "memtotal" -> memtotal := data; mem_add "memtotal" data
                            | "memfree" -> memfree := data; mem_add "memfree" data
                            | "swaptotal" -> swaptotal := data; mem_add "swaptotal" data
                            | "swapfree" -> swapfree := data; mem_add "swapfree" data
                            | _ -> ()
                          ) (Files.retrieve_file "/proc/meminfo" ':') in
               let memused = !memtotal - !memfree in mem_add "memused" memused;
               let swapused = !swaptotal - !swapfree in mem_add "swapused" swapused;
               mem
    | other -> raise (Invalid_argument other)
