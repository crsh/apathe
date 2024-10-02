# Set hook to print default numbers
#' @keywords internal

inline_numbers <- function (x) {

  if(inherits(x, "difftime")) x <- as.numeric(x)
  if(is.numeric(x)) {
    printed_number <- ifelse(
      x == round(x)
      , as.character(x)
      , papaja::apa_num(x)
    )
    n <- length(printed_number)
    if(n == 1) {
      printed_number
    } else if(n == 2) {
      paste(printed_number, collapse = " and ")
    } else if(n > 2) {
      paste(paste(printed_number[1:(n - 1)], collapse = ", "), printed_number[n], sep = ", and ")
    }
  } else if(is.integer(x)) {
    x <- papaja::apa_num(x, numerals = x > 10)
  } else if(is.character(x)) {
    x
  } else {
    paste(as.character(x), collapse = ', ')
  }
}


# Preprocessor functions are adaptations from the RMarkdown package
# (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
#' @keywords internal

set_default_csl <- function(x, version, metadata) {
  # Use APA6 CSL citations template if no other file is supplied
  has_csl <- function(text) {
    length(grep("^csl\\s*:.*$", text)) > 0
  }

  flavor <- list(NULL, "annotated")[[(!is.null(metadata$annotate_references) && metadata$annotate_references) + 1]]
  flavor <- c(
    flavor
    , list("no-disambiguation", NULL)[[(is.null(metadata$disambiguate_authors) || metadata$disambiguate_authors) + 1]]
  )

  csl_variant <- paste(c(paste0("apa", version), flavor), collapse = "-")

  if (!has_csl(readLines(x, warn = FALSE))) {
    csl_template <- system.file(
      "rmd", paste0(csl_variant, ".csl")
      , package = "papaja"
    )
    if(csl_template == "") stop("No CSL template file found.")
    return(c("--csl", rmarkdown::pandoc_path_arg(csl_template)))
  } else NULL
}


#' @keywords internal

define_latex_variable <- function(x, field, default, includes, redefine = FALSE) {
    if(redefine) {
      command <- "renewcommand"
    } else {
      command <- "providecommand"
    }
    
    if(!is.null(field)) {
      value  <- field
    } else {
      value  <- default
    }

    includes <- c(includes, paste0("\\", command, "{\\", x, "}{", value, "}"))
    includes
  }

#' @keywords internal

readLines_utf8 <- function(con) {
  if(is.character(con)) {
    con <- file(con, encoding = "utf8")
    on.exit(close(con))
  } else if(inherits(con, "connection")) {
    stop("If you want to use an already existing connection, you should use readLines(), directly.")
  }
  y <- try(readLines(con, encoding = "bytes"))
  if(inherits(y, "try-error")) stop("Reading from file ", encodeString(summary(con)$description, quote = "'"), " failed.")
  y
}

#' @keywords internal

writeLines_utf8 <- function(x, con) {
  if(is.character(con)) {
    con <- file(con, encoding = "utf8")
    on.exit(close(con))
  } else if(inherits(con, "connection")) {
    stop("If you want to use an already existing connection, you should use readLines(), directly.")
  }
  y <- try(writeLines(x, con, useBytes = TRUE))
  if(inherits(y, "try-error")) stop("Reading from file ", encodeString(summary(con)$description, quote = "'"), " failed.")
  y
}
