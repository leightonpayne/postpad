.onLoad <- function(libname, pkgname) {

  utils::globalVariables("seqid") # assign_position()
  utils::globalVariables("start") # assign_position()
  utils::globalVariables("type") # assign_position()
  utils::globalVariables("seqid") # calculate_separation()
  utils::globalVariables("system.number") # calculate_separation()
  utils::globalVariables("relative.position") # calculate_separation()
  utils::globalVariables("genes") # calculate_separation()
  utils::globalVariables("seqid") # summarise_system_count()
  utils::globalVariables("system.number") # summarise_system_count()

  invisible()

}
