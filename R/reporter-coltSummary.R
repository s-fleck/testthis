#' Test reporter: summary of errors (with colt support).
#'
#' This is reporter is a copy of [testthat::SummaryReporter], with the only
#' difference that it uses the [colt] color theme package. That means it will
#' look slightly better in dark terminals (terminal color detection only works
#' in RStudio).
#'
#' @export
#' @family reporters
ColtSummaryReporter <- R6::R6Class(
  "SummaryReporter",
  inherit = testthat::Reporter,
  public = list(
    failures = NULL,
    skips = NULL,
    warnings = NULL,
    max_reports = NULL,
    show_praise = TRUE,
    omit_dots = FALSE,

    initialize = function(
      show_praise = TRUE,
      omit_dots = getOption("testthat.summary.omit_dots"),
      max_reports = getOption("testthat.summary.max_reports", 10L),
      ...
    ){
      super$initialize(...)
      self$failures    <- testthat:::Stack$new()
      self$skips       <- testthat:::Stack$new()
      self$warnings    <- testthat:::Stack$new()
      self$max_reports <- max_reports
      self$show_praise <- show_praise
      self$omit_dots   <- omit_dots
    },

    is_full = function() {
      self$failures$size() >= self$max_reports
    },

    start_context = function(context) {
      self$cat_tight(context, ": ")
    },

    end_context = function(context) {
      self$cat_line()
    },

    add_result = function(context, test, result) {
      if (expectation_broken(result)) {
        self$failures$push(result)
      } else if (expectation_skip(result)) {
        self$skips$push(result)
      } else if (expectation_warning(result)) {
        self$warnings$push(result)
      } else {
        if (isTRUE(self$omit_dots)) {
          return()
        }
      }

      self$cat_tight(private$get_summary(result))
    },

    end_reporter = function() {
      skips <- self$skips$as_list()
      failures <- self$failures$as_list()
      warnings <- self$warnings$as_list()

      self$cat_line()
      private$cat_reports("Skipped", skips, Inf, skip_summary)
      private$cat_reports("Warnings", warnings, Inf, skip_summary)
      private$cat_reports("Failed", failures, self$max_reports, failure_summary)

      if (self$failures$size() >= self$max_reports) {
        self$cat_line(
          "Maximum number of ", self$max_reports, " failures reached, ",
          "some test results may be missing."
        )
        self$cat_line()
      }

      self$rule("DONE", pad = "=")
      if (self$show_praise) {
        if (length(failures) == 0 && runif(1) < 0.1) {
          self$cat_line(colourise(testthat:::praise(), "success"))
        }
        if (length(failures) > 0 && runif(1) < 0.25) {
          self$cat_line(colourise(testthat:::encourage(), "error"))
        }
      }
    }
  ),

  private = list(
    get_summary = function(result) {
      if (expectation_broken(result)) {
        if (self$failures$size() <= length(labels)) {
          return(colourise(labels[self$failures$size()], "error"))
        }
      }

      single_letter_summary(result)
    },

    cat_reports = function(header, expectations, max_n, summary_fun,
                           collapse = "\n\n") {
      n <- length(expectations)
      if (n == 0L) {
        return()
      }

      self$rule(header)

      if (n > max_n) {
        expectations <- expectations[seq_len(max_n)]
      }

      labels <- seq_along(expectations)
      exp_summary <- function(i) {
        summary_fun(expectations[[i]], labels[i])
      }
      report_summary <- vapply(seq_along(expectations), exp_summary, character(1))

      self$cat_tight(paste(report_summary, collapse = collapse))
      if (n > max_n) {
        self$cat_line()
        self$cat_line("  ... and ", n - max_n, " more")
      }

      self$cat_paragraph()
    }
  )
)




labels <- c(1:9, letters, LETTERS)




colourise <- function(
  text,
  as = c("success", "skip", "warning", "failure", "error"))
{
  colour_config <- getOption("testthat.use_colours", TRUE)
  if (!isTRUE(colour_config)) {
    return(text)
  }
  as <- match.arg(as)

  testthat_colours <- list(
    success = colt::clt_true,
    skip    = colt::clt_chr_accent,
    warning = colt::clt_maybe,
    failure = colt::clt_false,
    error   = colt::clt_false
  )

  testthat_colours[[as]](text)
}




skip_summary <- function(x, label) {
  header <- paste0(label, ". ", x$test)

  paste0(
    colourise(header, "skip"), src_loc(x$srcref), " - ", x$message
  )
}




failure_summary <- function(x, label, width = console_width()) {
  header <- paste0(label, ". ", failure_header(x))
  linewidth <- ifelse(nchar(header) > width, 0, width - nchar(header))
  line <- paste(rep("-", linewidth), collapse = "")

  paste0(
    colourise(header, "error"), line, "\n",
    format(x)
  )
}




failure_header <- function(x) {
  type <- switch(expectation_type(x),
                 error = "Error",
                 failure = "Failure"
  )

  paste0(type, ": ", x$test, src_loc(x$srcref), " ")
}




src_loc <- function(ref) {
  if (is.null(ref)) {
    ""
  } else {
    paste0(" (@", basename(attr(ref, "srcfile")$filename), "#", ref[1], ")")
  }
}




expectation_type <- function(exp) {
  stopifnot(is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}




expectation_success <- function(exp) {
  expectation_type(exp) == "success"
}




expectation_failure <- function(exp) {
  expectation_type(exp) == "failure"
}




expectation_error <- function(exp) {
  expectation_type(exp) == "error"
}




expectation_skip <- function(exp) {
  expectation_type(exp) == "skip"
}




expectation_warning <- function(exp) {
  expectation_type(exp) == "warning"
}




expectation_broken <- function(exp) {
  expectation_failure(exp) || expectation_error(exp)
}




expectation_ok <- function(exp) {
  expectation_type(exp) %in% c("success", "warning")
}




single_letter_summary <- function(x) {
  switch(expectation_type(x),
         skip    = colourise("S", "skip"),
         success = colourise(".", "success"),
         error   = colourise("E", "error"),
         failure = colourise("F", "failure"),
         warning = colourise("W", "warning"),
         "?"
  )
}




#testthis::test_all(reporter = ColtSummaryReporter)
