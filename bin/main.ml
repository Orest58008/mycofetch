let biba = Fetch.fetch_array [|"os";"kernel";"board";"host";"uptime";"distro"|]
let () = Hashtbl.find (List.assoc "uptime" biba) "hours" |> print_endline
let () = Hashtbl.find (List.assoc "uptime" biba) "mins" |> print_endline
let () = Hashtbl.find (List.assoc "uptime" biba) "secs" |> print_endline
let () = List.init 7 ( fun x ->
                       let (_, words) =
                         Hashtbl.find (List.assoc "distro" biba) ("logo_" ^ (string_of_int x))
                         |> Processing.process_line ~style:true in
                       String.concat "" words ) |> List.iter print_endline
