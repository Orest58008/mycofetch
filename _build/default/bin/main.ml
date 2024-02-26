let biba = Fetch.fetch_array [|"os";"kernel";"board";"host";"uptime"|]
let () = Hashtbl.find (List.assoc "uptime" biba) "hours" |> print_endline
let () = Hashtbl.find (List.assoc "uptime" biba) "mins" |> print_endline
let () = Hashtbl.find (List.assoc "uptime" biba) "secs" |> print_endline
