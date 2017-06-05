msg_testfile_does_not_exist <- function(x){
  message(
    sprintf('The file `%s` does not exist. ', x),
    'You can create it with test_skeleton().'
  )
}
