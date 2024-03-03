type pm = {
    name : string;
    command : string;
  }

type distro = {
    pm : pm;
    color : string;
    logo_tiny : string;
    logo : string array;
  }

let distro_of_id id : distro =
  match id with
  | "alpine" -> {
      pm = { name = "apk"; command = "apk info" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#      /\\          ";
                "#c4#     /  \\         ";
                "#c4#    / /\\ \\  /\\    ";
                "#c4#   / /  \\ \\/  \\   ";
                "#c4#  / /    \\ \\/\\ \\  ";
                "#c4# / / /|   \\ \\ \\ \\ ";
                "#c4#/_/ /_|    \\_\\ \\_\\"; |]
    }
  | "arch" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c6";
      logo_tiny = "";
      logo = [| "#c6#      /\\      ";
                "#c6#     /  \\     ";
                "#c6#    /\\   \\    ";
                "#c6#   /      \\   ";
                "#c6#  /   ,,   \\  ";
                "#c6# /   |  |  -\\ ";
                "#c6#/_-''    ''-_\\"; |]
    }
  | "arco" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#      /\\      ";
                "#c4#     /  \\     ";
                "#c4#    / /\\ \\    ";
                "#c4#   / /  \\ \\   ";
                "#c4#  / /    \\ \\  ";
                "#c4# / / _____\\ \\ ";
                "#c4#/_/  `----.__\\"; |]
    }
  | "artix" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c6";
      logo_tiny = "";
      logo = [| "#c6#      /\\      ";
                "#c6#     /  \\     ";
                "#c6#    /`'.,\\    ";
                "#c6#   /     ',   ";
                "#c6#  /      ,`\\  ";
                "#c6# /   ,.'`.  \\ ";
                "#c6#/.,'`     `'.\\"; |]
    }
  | "centos" -> {
      pm = { name = "rpm"; command = "rpm -qa" };
      color = "c5";
      logo_tiny = "";
      logo = [| "#c2# ____#c3#^#c5#____ ";
                "#c2# |\\  #c3#|#c5#  /| ";
                "#c2# | \\ #c3#|#c5# / | ";
                "#c5#<---- #c4#---->";
                "#c4# | / #c2#|#c3# \\ | ";
                "#c4# |/__#c2#|#c3#__\\| ";
                "#c2#     v     "; |]
    }
  | "crux" -> {
      pm = { name = "pkginfo"; command = "pkginfo -i" };
      color = "c6";
      logo_tiny ="";
      logo = [| "#c6#    ___   ";
                "#c6#   (#c7#.· #c6#|  ";
                "#c6#   (#c3#<> #c6#|  ";
                "#c6#  / #c7#__  #c6#\\ ";
                "#c6# ( #c7#/  \\ #c6#/|";
                "#c3#_#c6#/\\#c7#\\__)#c6#/#c3#_#c6#)";
                "#c3#\\/#c6#-____#c3#\\/ "; |]
    }
  | "crystal" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c5";
      logo_tiny = "";
      logo = [| "#c5#  //    ";
                "#c5# //     ";
                "#c5#//   \\\\ ";
                "#c5#\\\\    \\\\";
                "#c5# \\\\   //";
                "#c5#     // ";
                "#c5#    //  "; |]
    }
  | "debian" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c1";
      logo_tiny = "";
      logo = [| "#c1#   ,---._ ";
                "#c1# /`  __  \\";
                "#c1#|   /    |";
                "#c1#|   `.__.`";
                "#c1# \\        ";
                "#c1#  `-._    ";
                "#c1#      `   "; |]
    }
  | "devuan" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#-.,          ";
                "#c4#   `'-._     ";
                "#c4#        `::. ";
                "#c4#          \\::";
                "#c4#      __--`:`";
                "#c4# _,--` _.-`  ";
                "#c4#:_,--``      "; |]
    }
  | "endeavouros" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c5";
      logo_tiny = "";
      logo = [| "#c1#      /#c5#\\     ";
                "#c1#    /#c5#/  \\#c4#\\   ";
                "#c1#   /#c5#/    \\ #c4#\\ ";
                "#c1# / #c5#/     _) #c4#)";
                "#c1#/_#c5#/___-- #c4#__- ";
                "#c4# /____--     "; |]
    }
  | "fedora" -> {
      pm = { name = "rpm"; command = "rpm -qa" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c7#      ____   ";
                "#c7#     /  __)#c4#\\ ";
                "#c7#  ___| |_#c4#_) )";
                "#c7# / __|  _)#c4#_/ ";
                "#c7#( (__| |     ";
                "#c7# \\_____/     "; |]
    }
  | "freebsd" -> {
      pm = { name = "pkg"; command = "pkg info" };
      color = "c1";
      logo_tiny = "";
      logo = [| "#c1# _  _____  _ ";
                "#c1#/ \\`     `/ \\";
                "#c1#\\/       (__/";
                "#c1#|           |";
                "#c1#|           |";
                "#c1# \\         / ";
                "#c1#  `-_____-`  "; |]
    }
  | "gentoo" -> {
      pm = { name = "portage"; command = "ls /var/db/pkg/*" };
      color = "c5";
      logo_tiny = "";
      logo = [| "#c5# .-----.   ";
                "#c5#(       \\  ";
                "#c5#\\   ()   \\ ";
                "#c5# \\        )";
                "#c5# /      _/ ";
                "#c5#(     _-   ";
                "#c5#\\____-     "; |]
    }
  | "guix" -> {
      pm = { name = "guix"; command = "guix package --list-installed" };
      color = "c3";
      logo_tiny = "";
      logo = [| "#c3#\\____          ____/";
                "#c3# \\__ \\        / __/ ";
                "#c3#    \\ \\      / /    ";
                "#c3#     \\ \\    / /     ";
                "#c3#      \\ \\  / /      ";
                "#c3#       \\ \\/ /       ";
                "#c3#        \\__/        "; |]
    }
  | "hyperbola" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c7";
      logo_tiny = "";
      logo = [| "#c7#    /`__.`/ ";
                "#c7#    \\____/  ";
                "#c7#    .--.    ";
                "#c7#   /    \\   ";
                "#c7#  /  ___ \\  ";
                "#c7# / .`   `.\\ ";
                "#c7#/.`      `.\\"; |]
    }
  | "linuxmint" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c2";
      logo_tiny = "󰣭";
      logo = [| "#c2# ___________ ";
                "#c2#|_          \\";
                "#c2#  | #c7#| ,.,., #c2#|";
                "#c2#  | #c7#| | | | #c2#|";
                "#c2#  | #c7#| | | | #c2#|";
                "#c2#  | #c7#\\_____/ #c2#|";
                "#c2#  \\_________/"; |]
    }
  | "mageia" -> {
      pm = { name = "rpm"; command = "rpm -qa" };
      color = "c6";
      logo_tiny = "";
      logo = [| "#c6#   *    ";
                "#c6#    *   ";
                "#c6#   **   ";
                "#c7# /\\__/\\ ";
                "#c7#/      \\";
                "#c7#\\      /";
                "#c7# \\____/ "; |]
    }
  | "manjaro" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c2";
      logo_tiny = "";
      logo = [| "#c2#||||||||| ||||";
                "#c2#||||||||| ||||";
                "#c2#|||| .... ||||";
                "#c2#|||| |||| ||||";
                "#c2#|||| |||| ||||";
                "#c2#|||| |||| ||||";
                "#c2#|||| |||| ||||"; |]
    }
  | "mx" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c7";
      logo_tiny = "";
      logo = [| "#c7#    \\\\  / ";
                "#c7#     \\\\/  ";
                "#c7#      \\\\  ";
                "#c7#   /\\/ \\\\ ";
                "#c7#  /  \\  V\\";
                "#c7# /    \\/  \\";
                "#c7#/__________\\"; |]
    }
  | "neon" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c6";
      logo_tiny = "";
      logo = [| "#c7#  .#c6#__#c7#.#c6#__#c7#.  ";
                "#c6# /  _#c7#.#c6#_  \\ ";
                "#c6#/  /   \\  \\";
                "#c7#. #c6#|  #c7#O#c6#  | #c7#.";
                "#c6#\\  \\_#c7#.#c6#_/  /";
                "#c6# \\#c7#.#c6#__#c7#.#c6#__#c7#.#c6#/ "; |]
    }
  | "nixos" -> {
      pm = { name = "nix";
             command = "nix-store -q --requisites /run/current-system/sw
                        nix-store -q --requisites ~/.nix-profile" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#  \\\\  \\\\ //  ";
                "#c4#===\\\\__\\\\/ //";
                "#c4#   //   \\\\// ";
                "#c4#==//     //==";
                "#c4# //\\\\___//   ";
                "#c4#// /\\\\  \\\\===";
                "#c4#  // \\\\  \\\\  "; |]
    }
  | "openbsd" -> {
      pm = { name = "ports"; command = "ls /var/db/pkg" };
      color = "c3";
      logo_tiny = "";
      logo = [| "#c3#     _____    ";
                "#c3#   \\-     -/  ";
                "#c3#\\_/         \\ ";
                "#c3#|        #c7#O O#c3# |";
                "#c3#|_  <   )  3 )";
                "#c3#/ \\         / ";
                "#c3#   /-_____-\\  "; |]
    }
  | "opensuse-tumbleweed" -> {
      pm = { name = "rpm"; command = "rpm -qa" };
      color = "c4";
      logo_tiny = "∞";
      logo = [| "#c4#  _____   ______  ";
                "#c4# / ____\\ / ____ \\ ";
                "#c4#/ /    '/ /    \\ \\";
                "#c4#\\ \\____/ /,____/ /";
                "#c4# \\______/ \\_____/ "; |]
    }
  | "opensuse-leap" -> {
      pm = { name = "rpm"; command = "rpm -qa" };
      color = "c2";
      logo_tiny = "";
      logo = [| "#c2#  _______  ";
                "#c2#__|   __ \\ ";
                "#c2#     / .\\ \\";
                "#c2#     \\__/ |";
                "#c2#   _______|";
                "#c2#   \\_______";
                "#c2#__________/"; |]
    }
  | "parabola" -> {
      pm = { name = "pacman"; command = "pacman -Qq" };
      color = "c5";
      logo_tiny = "";
      logo = [| "#c5#  __ __ __  _  ";
                "#c5#.`_//_//_/ / `.";
                "#c5#          /  .`";
                "#c5#         / .`  ";
                "#c5#        /.`    ";
                "#c5#       /`      "; |]
    }
  | "popos" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c6";
      logo_tiny = "";
      logo = [| "#c7# 76767      ";
                "#c7#76  76   767";
                "#c7# 7676'   76 ";
                "#c7#  76     7  ";
                "#c7#   76   76  ";
                "#c7# __________ ";
                "#c7# 7676767676 "; |]
    }
  | "pureos" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c7";
      logo_tiny = "□";
      logo = [| "#c7# _____________ ";
                "#c7#|  _________  |";
                "#c7#| |         | |";
                "#c7#| |         | |";
                "#c7#| |_________| |";
                "#c7#|_____________|"; |]
    }
  | "qubesos" -> {
      pm = { name = ""; command = "" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#    _--_    ";
                "#c4# _-'    '-_ ";
                "#c4#|   .''.   |";
                "#c4#|  |    |  |";
                "#c4#|  |    |  |";
                "#c4#'-_ '--'  -'";
                "#c4#   '-__-\\_\\ "; |]
    }
  | "raspbian" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c1";
      logo_tiny =  "";
      logo = [| "#c2#  __  __  ";
                "#c2# (_\\)(/_) ";
                "#c1# (_(__)_) ";
                "#c1#(_(_)(_)_)";
                "#c1# (_(__)_) ";
                "#c1#   (__)   "; |]
    }
  | "slackware" -> {
      pm = { name = "pkgtool"; command = "ls /var/log/packages" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#   ________  ";
                "#c4#  /  ______| ";
                "#c4#  | |______  ";
                "#c4#  \\______  \\ ";
                "#c4#   ______| | ";
                "#c4#| |________/ ";
                "#c4#|____________"; |]
    }
  | "solus" -> {
      pm = { name = "eopkg"; command = "eopkg list-installed" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c7#      _     ";
                "#c7#     /|     ";
                "#c7#    / |\\    ";
                "#c7#   /  | \\ _ ";
                "#c7#  /#c4#___#c7#|#c4#__#c7#\\#c4#_#c7#\\";
                "#c4# \\         /";
                "#c4#  `-------' "; |]
    }
  | "ubuntu" -> {
      pm = { name = "dpkg"; command = "dpkg-query -f '.\\n' -W" };
      color = "c1";
      logo_tiny = "";
      logo = [| "#c1#         _ ";
                "#c1#     ---(_)";
                "#c1# _/  ---  \\";
                "#c1#(_) |   |  ";
                "#c1#  \\  --- _/";
                "#c1#     ---(_)"; |]
    }
  | "void" -> {
      pm = { name = "xbps"; command = "xbps-query -l" };
      color = "c2";
      logo_tiny = "";
      logo = [| "#c2#    _______  ";
                "#c2# _ \\______ - ";
                "#c2#| \\  ___  \\ |";
                "#c2#| | /   \\ | |";
                "#c2#| | \\___/ | |";
                "#c2#| \\______ \\_|";
                "#c2# -_______\\   "; |]
    }
  | _ -> {
      pm = { name = ""; command = "" };
      color = "c4";
      logo_tiny = "";
      logo = [| "#c4#    ___   ";
                "#c4#   (#c7#.. #c4#\\  ";
                "#c4#   (#c3#<> #c4#|  ";
                "#c4#  /#c7#/  \\ #c4#\\ ";
                "#c4# ( #c7#|  | #c4#/|";
                "#c3#_#c4#/\\#c7#\\__)#c4#/#c3#_#c4#)";
                "#c3#\\/#c4#-____#c3#\\/ "; |]
    }
