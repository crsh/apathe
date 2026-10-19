# Ensures bibtex is installed, because DOI-retrieval via {rmdfiltr} needs it
# but {rmdfiltr} only lists {bibtex} in Suggests

ignore_unused_imports <- function() {
  bibtex::read.bib
}
