#' Aider Function
#'
#' This function provides help information for the aider command.
#' 
#' @param args A character vector of arguments.
#' @return Prints help information if '--help' is passed as an argument.
#' @export
aider <- function(args) {
  if ("--help" %in% args) {
    cat("Usage: aider [options]\n")
    cat("Options:\n")
    cat("  --help     Show this help message and exit\n")
  } else {
    cat("Invalid argument. Use --help to see available options.\n")
  }
}
