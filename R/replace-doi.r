#' Replace DOI citations in RMarkdown document
#'
#' This function reads an RMarkdown document and replaces all DOI citations
#' with the corresponding entries from a BibTeX file. Requires the package
#' `RefManageR` to be installed.
#'
#' @param rmd A character vector specifying the path to the RMarkdown file 
#'   (UTF-8 encoding expected).
#' @param bib A character vector specifying the path to the BibTeX file
#'   (UTF-8 encoding expected).
#' @return Returns `TRUE` invisibly.
#' @examples
#' dontrun({
#' replace_doi_citations("myreport.Rmd")
#' })
#' @importFrom RefManageR ReadBib
#' @importFrom stringr str_replace_all

replace_doi_citations <- function(rmd, bib = "__from_DOI.bib") {
  if(!require("RefManageR")) stop("The package `RefManageR` is not avialable but required to run replace DOI citations in the source document. Please install the package and try again.")

  entries <- RefManageR::ReadBib(bib)$doi
  entries <- entries[!is.na(entries) & !duplicated(entries)]

  rmd <- file("test.Rmd", encoding = "UTF-8", )
  on.exit(close(rmd))

  stringr::str_replace_all(
    readLines(con = rmd, encoding = "UTF-8")
    , setNames(
      paste0("@", nameso(entries))
      , paste0("@(doi:|DOI:|(https://)*doi.org/)*", entries)
    )
  ) |>
    writeLines(con = rmd, useBytes = TRUE)

  invisible(TRUE)
}
