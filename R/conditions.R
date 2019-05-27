msg_testfile_does_not_exist <- function(x){
  message(
    sprintf("The file '%s' does not exist. ", x),
    "You can create it with testthis::test_skeleton()."
  )
}




require_rstudio <- function(){
  if(!requireNamespace("rstudioapi", quietly = TRUE)){
    stop("This function requires access the RStudio API", call. = FALSE)
  }
}
