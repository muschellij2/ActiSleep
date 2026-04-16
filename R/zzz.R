.onAttach <- function(lib, pkg) {
  packageStartupMessage(
    paste0("ActiSleep v", utils::packageVersion("ActiSleep"))
  )
}
