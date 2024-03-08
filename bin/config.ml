type t = {
    marshal_path: string;
    marshal_compile: bool;
    template_path: string;
    template: string;
    help: bool;
    logo: string
  }

let empty : t =
  { marshal_path = ""; marshal_compile = false; template_path = ""; template = ""; help = false;
    logo = "" }

let print_help () : unit =
  let help = [
      "Mycofetch is a fast and flexible system information tool written in Ocaml";
      "";
      "Parameters:";
      "  -t / --template /path/to/template";
      "      specify a custom path to template file";
      "  -m / --marshal /path/to/compiled_template";
      "      use a compiled template file";
      "  -c / --compile";
      "      use with -m / --marshal to compile template into file";
      "  -i / --inline template";
      "      use `template` instead of template file";
      "  -l / --logo distro";
      "      use `distro`'s logo and colours instead of your distro's ones";
      "  -h / --help";
      "      print this message"
    ] in List.iter print_endline help

let rec get_config argl config : t =
  match argl with
  | [] -> { config with template_path = try Sys.getenv "MFTEMPLATE"
                                        with _ ->
                                          try Sys.getenv "XDG_CONFIG_HOME" ^ "/mycofetch/template"
                                          with _ ->
                                            Sys.getenv "HOME" ^ "/.config/mycofetch/template" }
  | ("-t" | "--template")::path::rest -> { (get_config rest config) with template_path = path }
  | ("-m" | "--marshal")::path::rest -> { (get_config rest config) with marshal_path = path }
  | ("-c" | "--compile")::rest -> { (get_config rest config) with marshal_compile = true }
  | ("-i" | "--inline")::inline::rest -> { (get_config rest config) with template = inline }
  | ("-l" | "--logo")::logo::rest -> { (get_config rest config) with logo = logo }
  | ("-h" | "--help")::_ -> print_help (); exit 0
  | other::rest when Str.string_match (Str.regexp ".*mycofetch.*") other 0 ->
     get_config rest config
  | other::_ -> raise (Invalid_argument other)
