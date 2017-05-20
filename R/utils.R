is_test_file <- function(x){
  is.scalar(x) &&  is_test_files(x)
}




is_test_files <- function(x){
  grepl("^test[_\\-]", basename(x))
}



get_current_file <- function(){
  if (!requireNamespace("rstudioapi")){
    stop('This function is designed to be used from within Rstudio')
  }

  res <- rstudioapi::getActiveDocumentContext()$path

  if (identical(res, '')){
    res <-  rstudioapi::getSourceEditorContext()$path
  }

  res
}
