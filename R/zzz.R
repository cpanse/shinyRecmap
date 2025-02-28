#R

.onAttach <- function(lib, pkg){
  if (Sys.info()['machine'] == "arm64" && Sys.info()['sysname'] == "Darwin"){
    GA::gaControl(useRcpp = FALSE)
  }
}


