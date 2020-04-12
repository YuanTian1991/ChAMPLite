# ==========================================================================
# ChAMP package initialization
# ==========================================================================

.onAttach <- function(libname, pkgname) {
    mes <- "       ___ _      _   __  __ ___ 
      / __| |_   /_\\ |  \\/  | _ \\
     | (__| ' \\ / _ \\| |\\/| |  _/
      \\___|_||_/_/ \\_\\_|  |_|_|  
      ------------------------------
"
name <- "
   ___ _      _   __  __ ___ _    _ _       
  / __| |_   /_\\ |  \\/  | _ \\ |  (_) |_ ___
 | (__| ' \\ / _ \\| |\\/| |  _/ |__| |  _/ -_)
  \\___|_||_/_/ \\_\\_|  |_|_| |____|_|\\__\\___|
  -------------------------------------------
"
welcomeMessage <- "\n[ Package version 0.0.1 loaded ]\n"
maintainEmail <- "[ Any questions/bug/suggestion, please email champ450K@gmail.com ]\n"

    packageStartupMessage(name, welcomeMessage,maintainEmail)
}

