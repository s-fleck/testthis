#' Guess tested functions based on contents of test_that desc argument
#'
#' Display the `desc` arguments of all `test_that(` calls in the
#' `tests/testthat` directory of a package`
#'
#' @return A `test_index` `data.frame`
#' @export
#'
get_test_index <- function(){
  ttfiles <- list_test_files(full_names = TRUE, recursive = TRUE)
  res <- collect_testthat_source_info(ttfiles)

  structure(
    res,
    class = c("test_index", "data.frame")
  )
}




#' Print Test Index objects
#'
#' @param x see [get_test_index()]
#'
#' @return `x` (invisibly)
#' @export
print.test_index <- function(x){
  common_path <- fs::path_common(x$path)
  dd <- x

  dd <- as.data.frame(dd)

  dd$path_diff <- path_diff(x$path, common_path)
  dd$subdir <- dirname(dd$path_diff)
  dd$file <- basename(dd$path)
  dd$line <- style_subtle(pad_left(dd$line1))
  sep     <- style_subtle(":")

  dd <- dd[order(dd$subdir), ]
  dd$subdir <- style_subtle(dd$subdir)

  path_sep = style_subtle("/")


  cat(style_accent(common_path), "\n\n")

  for (path in unique(dd$path)){
    sub <- dd[dd$path == path, ]
    cat(sub$subdir[[1]], path_sep, sub$file[[1]], "\n\n", sep = "")

    cat(paste0(" ", sub$line, sep, " ", sub$desc), sep = "\n")
    cat("\n\n")
  }



  invisible(x)
}




#' Collect source info on test_that calls
#'
#' @param infiles a `character` vector of file paths
#'
#' @return a `data.frame` similar to what [getParseData()] returns
collect_testthat_source_info <- function(
  infiles
){
  tt  <-
    infiles %>%
    lapply(parse) %>%
    extract_testthat_parse_data()

  tt <- lapply(seq_along(tt), function(i) {
    .x <- tt[[i]]
    if (identical(nrow(.x), 0L))
      return(NULL)

    .x$desc <- extract_testthat_desc(.x$text)
    .x$path <- infiles[[i]]
    .x
  })

  do.call(rbind, tt)
}




#' Extract test_that() call related parse data (see [getParseData()])
#'
#' @param exp a `character` scalar (to be parsed) or an `expression` or a
#'   `list` of `expressions` (as returned by [parse()].
#'
#' @return a `data.frame`. See [getParseData()]
extract_testthat_parse_data <- function(
  exp
){
  if (is.list(exp)){
    return(lapply(exp, extract_testthat_parse_data))
  }

  if (is_scalar_character(exp))
    exp <- parse(text = exp)

  assert(is.expression(exp))
  pd <- getParseData(exp, includeText = TRUE)
  tt <- pd[grep("^test_that\\s*\\(", pd$text), ]
}




extract_testthat_desc <- function(
  exp
){
  if (!is_scalar(exp)){
    return(vapply(exp, extract_testthat_desc, character(1)))
  }

  if (is_scalar_character(exp))
    exp <- parse(text = exp)
  else
    assert(is.expression(exp))

  exp <- exp[[1]]
  assert(is.call(exp))
  exp <- as.list(exp)

  if("desc" %in% names(exp)){
    res <- exp$desc
  } else if ("code" %in% names(exp)){
    codepos <- which("code" == names(exp))
    if(identical(codepos, 2L)){
      res <- exp[[3]]
    }
  } else {
    res <- exp[[2]]
  }

  if (is.symbol(res))
    res <- deparse(res)

  format(res)
}




path_diff <- function(x, y){
  assert(is.character(x))
  assert(is_scalar_character(y))
  xs <- fs::path_split(x)
  ys <- unlist(fs::path_split(y))

  vapply(
    xs,
    function(.x) do.call(file.path, as.list(setdiff(unlist(.x), ys))),
    character(1)
  )
}
