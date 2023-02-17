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

#' @export
instructions <- function(){
  options(cli.width = 80)
  package_path <- fs::path_package("postpad")
  exec_path <- fs::path_abs(fs::path_join(c(package_path, "exec")))
  bash_config <- c(".profile", ".bashrc", ".bash_profile")
  scripts <- postpad_script()
  cli::cli_par()
  cli::cli_alert_info('Add the following line to one of your bash config files ({.file {bash_config}}) to make {.pkg {{postpad}}} scripts available from your $PATH:', wrap = TRUE)
  cli::cli_end()
  cli::cli_par()
  cli::cli_alert('export PATH=$PATH:"{exec_path}"')
  cli::cli_end()
  cli::cli_par()
  cli::cli_alert_info('Available scripts include: {.file {scripts}}', wrap = TRUE)
  cli::cli_end()
}


