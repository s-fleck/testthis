#' Generate an index of all test_that calls
#'
#' Generates an index the `desc` arguments of all `test_that()` calls in the
#' `tests/testthat` directory of a package.
#'
#' @param markers `logical` scalar. If `TRUE`, new markers are created in the
#'   RStudio markers pane. If `FALSE`, the index is printed to the console
#'   instead.
#'
#' @return A `test_index` `data.frame` (invisibly if `markers == TRUE`)
#' @export
#'
test_index <- function(
  markers = interactive() && requireNamespace("rstudioapi", quietly = TRUE)
){
  assert(is_scalar_bool(markers))
  idx <- get_test_index()

  if (markers){
    rstudioapi::sourceMarkers(
      name = "testthat index",
      markers = data.frame(
        type = "info",
        file = idx$path,
        line = idx$line1,
        column = idx$col1,
        message = idx$desc,
        stringsAsFactors = FALSE
      ),
      basePath = fs::path_common(idx$path)
    )
    invisible(idx)
  } else {
    idx
  }
}




get_test_index <- function(){
  ttfiles <- list_test_files(full_names = TRUE, recursive = TRUE)
  res <- collect_testthat_source_info(ttfiles)
  structure(
    res,
    class = c("test_index", "data.frame")
  )
}




#' @param x a `data.frame` of subclass `"test_index"`
#' @param ... currently ignored
#'
#' @rdname test_index
#' @export
print.test_index <- function(x, ...){
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
#' @return a `data.frame` similar to what [getParseData()] returns
#' @noRd
collect_testthat_source_info <- function(
  infiles
){
  assert(all(file.exists(infiles)))
  tt  <-
    infiles %>%
    lapply(parse, keep.source = TRUE) %>%
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
#' @noRd
extract_testthat_parse_data <- function(
  exp
){
  if (is.list(exp)){
    return(lapply(exp, extract_testthat_parse_data))
  }

  if (is_scalar_character(exp))
    exp <- parse(text = exp, keep.source = TRUE)

  assert(is.expression(exp))

  pd <- utils::getParseData(exp, includeText = TRUE)
  pd[grep("^test_that\\s*\\(", pd$text), ]
}




#' Extract desc argument from test_that calls
#'
#' @inheritParams extract_testthat_parse_data
#'
#' @return a `character` vector
#' @noRd
extract_testthat_desc <- function(
  exp
){
  if (!is_scalar(exp)){
    return(vapply(exp, extract_testthat_desc, character(1)))
  }

  if (is_scalar_character(exp))
    exp <- parse(text = exp, keep.source = TRUE)
  else
    assert(is.expression(exp))

  exp <- exp[[1]]
  assert(is.call(exp))
  exp <- as.list(exp)

  assert(identical(as.character(exp[[1]]), "test_that"))

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
