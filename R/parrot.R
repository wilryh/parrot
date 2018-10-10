#' Parrot scales short text around common words
#'
#' Let me know if you run into any problems.
#'
#' Here's a link to the paper:
#' https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3044864
#'
#' The method in this code is slightly different from the one in the paper,
#' which needs to be updated. It uses the square root of counts instead of
#' counts in the word cooccurrence matrix. This makes the 'first' dimension of
#' the output pick up word frequency and document length. This frequency
#' dimension is labeled 'X0' in the output.
#'
#' The pivoting is also done in 2 stages for a sharper separation between
#' common words and rare words, along with a standardization in between stages
#' to help with messier data sets (tweets).
#'
#' Finally, there is additional preprocessing step (inside "scale_text()") that
#' makes better use of out-of-sample word embeddings.
#'
#' The scale_text function suggests a truncation for the SVD. It is a function
#' of the vocabulary size and produces approximately the same number of pivot
#' words.
#'
#' @examples \dontrun{
#'  ## install.packages(
#'  ##     c("stm","ggplot2","gridExtra","Matrix",
#'  ##       "reshape2","ForeCA","devtools","magrittr"))
#'  #### word embeddings
#'  ## install.packages(c("dplyr","readr","tidyr","CCA"))
#'  #### recommended
#'  ## install.packages(c("RSpectra","roxygen2"))
#'  #### optional
#'  ## install.packages(c("knitr"))
#'
#' library(devtools)
#' ##
#' install_github("wilryh/parrot", dependencies=TRUE)
#'
#' library(stm)
#' library(parrot)
#'
#' processed <- textProcessor(
#'     input_data$text,
#'     data.frame(input_data),
#'     removestopwords=T, lowercase=T, stem=F
#'     )
#' out <- prepDocuments(
#'     processed$documents, processed$vocab, processed$meta
#'     )
#'
#' tdm <- doc_to_tdm(out)
#'
#' embeddings <- read_word_embeddings(
#'     in_vocab=out$vocab,
#'     ovefile = "O2M_overlap.txt" # must add location on your computer
#'         ## "path/to/O2M_overlap.txt"
#'     ## ovefile2 = "path/to/O2M_oov.txt", # very rare words and misspellings
#'     ## available here:
#'     ## http://www.cis.uni-muenchen.de/~wenpeng/renamed-meta-emb.tar.gz
#'     ## must unpack and replace "path/to/" with location on your computer
#'     )
#'
#' scores <- scale_text(
#'     meta=out$meta,
#'     tdm=tdm,
#'     embeddings=embeddings[["meta"]],
#'     compress_fast=TRUE,
#'     constrain_outliers=TRUE
#'     )
#'
#' document_scores <- score_documents(
#'     scores=scores, n_dimensions=10
#'     )
#'
#' get_keywords(scores, n_dimensions=3, n_words=15)
#'
#' with(document_scores, cor(sqrt(n_words), X0, use="complete"))
#'
#' plot_keywords(
#'     scores, x_dimension=1, y_dimension=2, q_cutoff=0.9
#'     )
#' }
#'
#' @docType package
#' @name parrot
NULL
