let rec distro_of_id id : (string, string) Hashtbl.t =
  match id with
  | "alpine" -> [
      "pm_name",   "apk";
      "pm_command","apk info";
      "color",     "c4";
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
      "color",      "c6";
      "logo_tiny",  "";
      "logo_0",     "#c6#      /\\      ";
      "logo_1",     "#c6#     /  \\     ";
      "logo_2",     "#c6#    /\\   \\    ";
      "logo_3",     "#c6#   /      \\   ";
      "logo_4",     "#c6#  /   ,,   \\  ";
      "logo_5",     "#c6# /   |  |  -\\ ";
      "logo_6",     "#c6#/_-''    ''-_\\";
    ] |> List.to_seq |> Hashtbl.of_seq
  | "arco" -> [
      "pm_name",    "pacman";
      "pm_command", "pacman -Q";
      "color",      "c4";
      "logo_tiny",  "";
      "logo_0",     "#c4#      /\\      ";
      "logo_1",     "#c4#     /  \\     ";
      "logo_2",     "#c4#    / /\\ \\    ";
      "logo_3",     "#c4#   / /  \\ \\   ";
      "logo_4",     "#c4#  / /    \\ \\  ";
      "logo_5",     "#c4# / / _____\\ \\ ";
      "logo_6",     "#c4#/_/  `----.__\\";
    ] |> List.to_seq |> Hashtbl.of_seq
  | "artix" -> [
      "pm_name",    "pacman";
      "pm_command", "pacman -Q";
      "color",      "c6";
      "logo_tiny",  "";
      "logo_0",     "#c6#      /\\      ";
      "logo_1",     "#c6#     /  \\     ";
      "logo_2",     "#c6#    /`'.,\\    ";
      "logo_3",     "#c6#   /     ',   ";
      "logo_4",     "#c6#  /      ,`\\  ";
      "logo_5",     "#c6# /   ,.'`.  \\ ";
      "logo_6",     "#c6#/.,'`     `'.\\";
    ] |> List.to_seq |> Hashtbl.of_seq
  | "centos" -> [
      "pm_name",    "rpm";
      "pm_command", "rpm -qa";
      "color",      "c5";
      "logo_tiny",  "";
      "logo_0",     "#c2# ____#c3#^#c5#____ ";
      "logo_1",     "#c2# |\\  #c3#|#c5#  /| ";
      "logo_2",     "#c2# | \\ #c3#|#c5# / | ";
      "logo_3",     "#c5#<---- #c4#---->";
      "logo_4",     "#c4# | / #c2#|#c3# \\ | ";
      "logo_5",     "#c4# |/__#c2#|#c3#__\\| ";
      "logo_6",     "#c2#     v     ";
    ] |> List.to_seq |> Hashtbl.of_seq
  | "crux" -> [
      "pm_name",    "pkginfo";
      "pm_command", "pkginfo -i";
      "color",      "c6";
      "logo_tiny",  "";
      "logo_0",     "#c6#    ___   ";
      "logo_1",     "#c6#   (#c7#.· #c6#|  ";
      "logo_2",     "#c6#   (#c3#<> #c6#|  ";
      "logo_3",     "#c6#  / #c7#__  #c6#\\ ";
      "logo_4",     "#c6# ( #c7#/  \\ #c6#/|";
      "logo_5",     "#c3#_#c6#/\\#c7#\\__)#c6#/#c3#_#c6#)";
      "logo_6",     "#c3#\\/#c6#-____#c3#\\/ ";
    ] |> List.to_seq |> Hashtbl.of_seq
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
  | _ -> distro_of_id "linux"
