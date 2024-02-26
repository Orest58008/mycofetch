let distro_of_id id : (string, string) Hashtbl.t =
  match id with
  | "alpine" -> [
      "pm_name",   "apk";
      "pm_command","apk info";
      "color",     "#c4#";
      "logo_tiny", "";
      "logo_0",    "#c4#      /\\          ";
      "logo_1",    "#c4#     /  \\         ";
      "logo_2",    "#c4#    / /\\ \\  /\\    ";
      "logo_3",    "#c4#   / /  \\ \\/  \\   ";
      "logo_4",    "#c4#  / /    \\ \\/\\ \\  ";
      "logo_5",    "#c4# / / /|   \\ \\ \\ \\ ";
      "logo_6",    "#c4#/_/ /_|    \\_\\ \\_\\";
    ] |> List.to_seq |> Hashtbl.of_seq
  | "arch" -> [
      "pm_name",    "pacman";
      "pm_command", "pacman -Q";
      "color",      "#c6#";
      "logo_tiny",  "";
      "logo_0",     "#c6#      /\\      ";
      "logo_1",     "#c6#     /  \\     ";
      "logo_2",     "#c6#    /\\   \\    ";
      "logo_3",     "#c6#   /      \\   ";
      "logo_4",     "#c6#  /   ,,   \\  ";
      "logo_5",     "#c6# /   |  |  -\\ ";
      "logo_6",     "#c6#/_-''    ''-_\\";
    ] |> List.to_seq |> Hashtbl.of_seq
  | "arco" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "artix" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "centos" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "crux" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "crystal" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "endeavouros" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "fedora" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "freebsd" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "gentoo" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "guix" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "hyperbola" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "linux" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "linuxmint" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "mageia" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "manjaro" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "mx" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "neon" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "nixos" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "openbsd" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "opensuse-tumbleweed" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "opensuse-leap" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "parabola" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "popos" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "pureos" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "qubesos" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "raspbian" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "slackware" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "solus" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "ubuntu" -> [] |> List.to_seq |> Hashtbl.of_seq
  | "void" -> [] |> List.to_seq |> Hashtbl.of_seq
  | other -> raise (Invalid_argument other)
