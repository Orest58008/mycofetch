type t = {
    template_path: string;
    template: string;
    help: bool;
    logo: string
  }

let get_config argv : t =
  let template_path = try Sys.getenv "MFTEMPLATE"
                      with _ -> try Sys.getenv "XDG_CONFIG_HOME" ^ "/mycofetch/template"
                                with _ -> Sys.getenv "HOME" ^ "/.config/mycofetch/template" in
  let config = ref { template_path = template_path; template = ""; help = false; logo = "" } in
  Array.iteri (fun i x ->
      match x with
      | "-c" | "--config" -> config := { !config with template_path = argv.(i + 1) }
      | "-h" | "--help" -> config := { !config with help = true }
      | "-l" | "--logo" -> config := { !config with logo = argv.(i + 1)}
      | "-i" | "--inline" -> config := { !config with template = argv.(i + 1) }
      | _ -> ()
    ) argv; !config
