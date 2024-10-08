#' University of Cologne Psychology Homework
#'
#' Template for homework in psychology at the University of Cologne in PDF
#' format.
#'
#' @inheritParams papaja::apa6_pdf
#' @param md_extensions Markdown extensions to be added or removed from the
#'   default definition of R Markdown. See the
#'   \code{\link[rmarkdown]{rmarkdown_format}} for additional details.
#' @param ... Further arguments to pass to
#'   \code{\link[bookdown]{pdf_document2}} or
#'   \code{\link[bookdown]{word_document2}}.
#' @details
#'   When creating PDF documents the YAML option `classoption` is passed
#'   to the class options of the LaTeX apa6 document class. In this case,
#'   additional options are available. Refer to the `apa6` document class
#'   \href{ftp://ftp.fu-berlin.de/tex/CTAN/macros/latex/contrib/apa6/apa6.pdf}{documentation}
#'   to find out about class options such as paper size.
#' 
#' @return R Markdown output format to pass to [rmarkdown::render()].
#' @seealso [uoc_psych_thesis_pdf], [bookdown::pdf_document2()], [bookdown::word_document2()]
#' @export

uoc_psych_homework_pdf <- function(
  fig_caption = TRUE
  , number_sections = FALSE
  , toc = FALSE
  , keep_tex = FALSE
  , md_extensions = NULL
  , includes = NULL
  , ...
) {
  assertthat::is.flag(fig_caption)
  assertthat::is.flag(number_sections)
  assertthat::is.flag(toc)
  assertthat::is.flag(keep_tex)
  if(!is.null(includes)) {
    assertthat::is.list(includes)
  } else {
    includes <- rmarkdown::includes()
  }

  apathe_header_includes <- system.file(
    "rmarkdown", "templates", "uoc-psych-homework", "resources"
    , "uoc_psych_homework_header_includes.tex"
    , package = "apathe"
  )
  if(apathe_header_includes == "") stop("LaTeX header includes file not found.")

  includes$in_header <- c(includes$in_header, apathe_header_includes)

  if(is.null(md_extensions) || !grepl("raw\\_attribute", md_extensions)) {
    md_extensions <- paste0(md_extensions, "+raw_attribute")
  }

  # Call pdf_document() with the appropriate options
  config <- bookdown::pdf_document2(
    fig_caption = fig_caption
    , number_sections = number_sections
    , toc = toc
    , keep_tex = keep_tex
    , md_extensions = md_extensions
    , includes = includes
    , ...
  )

  # Set chunk defaults
  # config$knitr$opts_chunk$echo <- TRUE
  # config$knitr$opts_chunk$message <- TRUE
  config$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  config$knitr$knit_hooks$inline <- inline_numbers

  config$knitr$opts_chunk$dev <- "pdf"
  config$knitr$opts_chunk$dpi <- 300
  # config$clean_supporting <- FALSE # Always keep images files

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL
  
  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- homework_pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(
      (is.null(metadata$replace_ampersands) || metadata$replace_ampersands) &&
      (is.null(metadata$citeproc) || metadata$citeproc)
    ) {
      metadata$citeproc <- FALSE
      assign("front_matter", metadata, pos = parent.frame())
    }

    args
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    output_text <- readLines_utf8(output_file)

    # Prevent (re-)loading of geometry package
    output_text <- gsub("\\\\usepackage\\[?.*\\]?\\{geometry\\}", "", output_text, useBytes = TRUE)

    writeLines_utf8(output_text, output_file)

    rmdfiltr::replace_doi_citations(input_file, metadata$bibliography)

    # Apply bookdown postprocesser and pass format options
    bookdown_post_processor <- bookdown::pdf_document2()$post_processor
    pp_env <- environment(bookdown_post_processor)
    assign("post", NULL, envir = pp_env) # Postprocessor is not self-contained
    assign("config", config, envir = pp_env) # Postprocessor is not self-contained
    assign("number_sections", number_sections, envir = pp_env)
    bookdown_post_processor(metadata = metadata, input = input_file, output = output_file, clean = clean, verbose = verbose)
  }

  config$pre_processor <- pre_processor
  config$post_processor <- post_processor

  config
}

homework_pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {

  # Add pandoc arguments
  args <- NULL

  if((!is.list(metadata$output) ||  !is.list(rmarkdown::metadata$output[[1]]) || is.null(metadata$output[[1]]$citation_package)) &
     (is.null(metadata$citeproc) || metadata$citeproc)) {

    ## Set CSL
    args <- set_default_csl(
      input_file
      , version = 6
      , metadata = metadata
    )
    csl_specified <- is.null(args)

    args <- rmdfiltr::add_doi2cite_filter(args)

    ## Set ampersand filter
    if((is.null(metadata$replace_ampersands) || metadata$replace_ampersands)) {
      if(csl_specified) {
        args <- c(args, "--csl", rmarkdown::pandoc_path_arg(tools::file_path_as_absolute(metadata$csl)))
      }

      args <- rmdfiltr::add_citeproc_filter(args)
      args <- rmdfiltr::add_replace_ampersands_filter(args)
    }
  }

  ## Set additional lua filters
  parse_metadata_filter <- system.file(
    "lua", "parse_metadata.lua"
    , package = "apathe"
  )
  args <- rmdfiltr::add_custom_filter(args, filter_path = parse_metadata_filter, lua = TRUE)

  ## Set template variables and defaults
  if(is.null(metadata$documentclass)) {
    args <- c(args, "--variable", "documentclass:apa6")
  }

  if(is.null(metadata$classoption)) {
    metadata$classoption <- "doc,a4paper"
  }

  if(is.null(metadata$floatsintext) || isTRUE(metadata$floatsintext)) {
    metadata$classoption <- paste0(metadata$classoption, ",floatsintext")
  }

  args <- c(args, "--variable", paste0("classoption:", metadata$classoption))

  if (is.null(metadata$lang)) {
    lang_tag <- "de-DE"
  } else {
    lang_tag <- metadata$lang
  }

  args <- c(args, "--variable", paste0("lang:", lang_tag))

  if(is.null(metadata$title)) {
    args <- c(args, "--variable", "title:TITLE")
  }

  if(is.null(metadata$`block-headings`)) {
    args <- c(args, "--variable", "block-headings:no")
  }


  # Add necessary includes
  header_includes <- NULL
  after_body_includes <- NULL
  before_body_includes <- NULL

  # Define LaTeX variables
  header_includes <- define_latex_variable("studentid", metadata$author[[1]]$`student-id`, default = "Matrikelnummer?", header_includes)
  header_includes <- define_latex_variable("smail", metadata$author[[1]]$email, default = "smail@uni-koeln.de", header_includes)
  
  header_includes <- define_latex_variable("course", metadata$course, default = NULL, header_includes)
  header_includes <- define_latex_variable("advisor", metadata$advisor, default = "Betreuer*in?", header_includes)
  
  ## Additional options
  if(is.null(metadata$geometry)) {
    metadata$geometry <- "a4paper, inner=1in, outer=1in, top=1in, bottom=1in"
  }
  header_includes <- c(header_includes, paste0("\\geometry{", metadata$geometry, "}\n\n"))

  if(is.null(metadata$linestretch)) {
    metadata$linestretch <- 1.5
  }
  header_includes <- c(header_includes, paste0("\\setstretch{", metadata$linestretch, "}\n\n"))

  tmp_includes_file <- function(x) {
    tmp_file <- tempfile(pattern = "includes_", tmpdir = tempdir(), fileext = ".tex")
    writeLines(x, con = tmp_file)
    tmp_file
  }

  header_includes <- c(header_includes, metadata$`header-includes`)
  if(length(header_includes) > 0) {
    args <- c(args, "--include-in-header", tmp_includes_file(header_includes))
  }

  before_body_includes <- c(before_body_includes, metadata$`before-includes`)
  if(length(before_body_includes) > 0) {
    args <- c(args, "--include-before", tmp_includes_file(before_body_includes))
  }

  after_body_includes <- c(after_body_includes, metadata$`after-includes`)
  if(length(after_body_includes) > 0) {
    args <- c(args, "--include-after", tmp_includes_file(after_body_includes))
  }

  args
}
