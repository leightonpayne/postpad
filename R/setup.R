#' @title Get path to padlocdev example
#' @description padlocdev comes bundled with a number of sample files in its
#' `inst/extdata` directory. This function make them easy to access.
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @return A [base::character()].
#' @export
#' @examples
#' padlocdev_example()
#' padlocdev_example("padloc-db/hmm/PDLC00150.hmm")
postpad_script <- function(file = NULL) {
  package_path <- fs::path_package("padlocdev")
  exec_path <- fs::path_join(c(package_path, "exec"))
  if (is.null(file)) {
    fs::dir_ls(extdata_path)
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
