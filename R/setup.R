#' @title Get path to padlocdev example
#' @description padlocdev comes bundled with a number of sample files in its
#' `inst/extdata` directory. This function make them easy to access.
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @return A [base::character()].
#' @export
postpad_script <- function(file = NULL) {
  package_path <- fs::path_package("postpad")
  exec_path <- fs::path_abs(fs::path_join(c(package_path, "exec")))
  if (is.null(file)) {
    fs::path_file(fs::dir_ls(exec_path))
  } else {
    file_path <- fs::path_join(c(package_path, "exec", file))
    file_exists <- fs::file_exists(file_path)
    if (file_exists) {
      file_path
    } else {
      cli::cli_abort("Can't find package file.", call = NULL)
    }
  }
}

#' @title Print instructions for setting up [postpad].
#' @return NULL
#' @export
instructions <- function(){
  package_path <- fs::path_package("postpad")
  exec_path <- fs::path_abs(fs::path_join(c(package_path, "exec")))
  scripts <- postpad_script()
  cli::cli_par()
  cli::cli_h1(cli::col_cyan("Setup Instructions"))
  cli::cli_end()
  cli::cli_par()
  cli::cli_alert_warning('Run the following line in your terminal, or add it to one of your bash config files to make {.pkg postpad} scripts available from your $PATH.', wrap = TRUE)
  cli::cli_end()
  cli::cli_par()
  cli::cli_text('{.emph export} PATH=$PATH:"{exec_path}"')
  cli::cli_end()
  cli::cli_par()
  cli::cli_alert_info('All scripts have usage information that can be viewed with {.kbd <script-name.R> --help}.', wrap = TRUE)
  cli::cli_alert_info('Available scripts include: {.file {scripts}}', wrap = TRUE)
  cli::cli_end()
}


