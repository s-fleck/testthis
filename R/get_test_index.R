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
  descs   <- extract_test_that_desc2(ttfiles)

  res <- do.call(rbind, descs)

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

  dd[order(dd$path), ]
  dd$path_diff <- path_diff(x$path, common_path)
  dd$subdir <- dirname(dd$path_diff)
  dd$file <- basename(dd$path)
  dd$line <- style_subtle(pad_left(dd$line))
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


#' Extract "desc" arguments from all test_that functions from .R script files
#'
#' @param infile character. Path to an .R script file, or a list of such paths;
#' usually created with list.files("/path/to/directory")
#' @return content of the "desc" arguments of test_that functions as a named
#'   list (one element per file, names correspond to full file paths.)
#' @noRd
extract_test_that_desc2 <- function(infile){

  tt_files  <- lapply(infile, parse) %>%
    setNames(infile)

  # fun tries to account for all possibilities where desc is not the second
  # argument of testthat
  fun <- function(exps) {
    .x <- as.list(exps)
    if("desc" %in% names(.x)){
      return(.x$desc)
    } else if ("code" %in% names(.x)){
      codepos <- which("code" == names(.x))
      if(identical(codepos, 2L)){
        return(.x[[3]])
      }
    } else {
      return(.x[[2]])
    }
  }


  lapply(seq_along(tt_files), function(i) {
    .x <- tt_files[[i]]
    path <- names(tt_files)[[i]]

    test_that_calls <- .x[grep("test_that", as.list(.x))]

    pd <- getParseData(test_that_calls)
    rids <- pd[grep("^test_that$", pd$text), ][["line1"]]

    descs <- lapply(test_that_calls, fun)

    if (!length(descs)){
      path <- NULL
    }

    data.frame(
      line = as.integer(rids),
      desc = unlist(descs),
      path = path,
      stringsAsFactors = FALSE
    )
  })
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
