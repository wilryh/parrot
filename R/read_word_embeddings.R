#' @rdname read_word_embeddings
#' @export
#'
#' @title
#' Read word embeddings and format for input to \code{scale_text}
#'
#' @description
#' \code{read_word_embeddings} reads specified words from word embedding files
#' quickly and without using much memory. It formats its output for the
#' \code{scale_text} function. The rows of the output are words and the columns
#' are the dimensions from the word embeddings. Correspondingly, the row names
#' are the vocabulary and the column names are the names of the dimensions.
#'
#' @details
#' This function reads one or more of the pre-trained word embeddings listed
#' above. You need to first download these files and unzip them on your computer
#' before you can use them. Remember to add the file path to the file name when
#' you specify it in this function.
#'
#' Meta embeddings:
#' http://www.cis.uni-muenchen.de/~wenpeng/renamed-meta-emb.tar.gz
#'
#' Wikipedia embeddings: http://nlp.stanford.edu/data/glove.6B.zip
#'
#' Twitter embeddings: http://nlp.stanford.edu/data/glove.twitter.27B.zip
#'
#'
#' You can specify one or more pre-trained word embedding files. I recommend the
#' meta embeddings. The full meta embeddings are contained in two files -- one
#' for ordinary words and one for rare words and/or misspellings (that appeared
#' in only a subset of the different text sources).
#'
#' @param in_vocab Character vector. This is the vocabulary to look for in the
#' word embeddings.
#' @param ovefile A character scalar (filename). Use this for O2M_overlap.txt
#' from the meta embeddings. This is a meta-analysis of many pre-trained word
#' embeddings.
#' Recommended.
#' @param ovefile2 A character scalar (filename). Use this for O2M_oov.txt from
#' the meta embeddings. These are the rare words for the meta-analysis of many
#' pre-trained word
#' embeddings.
#' @param wikfile A character scalar (filename). Use this for glove.6B.300d.txt
#' from the Wikipedia embeddings. These word embeddings are trained on Wikipedia
#' entries only.
#' @param twifile A character scalar (filename). Use this for
#' glove.twitter.27B.200d.txt from the Twitter embeddings. These word embeddings
#' are trained on Twitter data only.
#'
#' @seealso \code{\link{scale_text}}, \code{\link{doc_to_tdm}},
#' \code{\link{get_keywords}}, \code{\link{plot_keywords}},
#' \code{\link{score_documents}}
#'
#' @examples
#' \dontrun{
#' # download and extract embeddings data first
#'
#' embeddings <- read_word_embeddings(
#'     in_vocab = out$vocab,
#'     # must add location on your computer "path/to/O2M_overlap.txt"
#'     ovefile = "O2M_overlap.txt",
#'     ovefile2 = "O2M_oov.txt" # very rare words and misspellings
#'     ## available here:
#'     ## http://www.cis.uni-muenchen.de/~wenpeng/renamed-meta-emb.tar.gz
#'     ## must unpack and replace "path/to/" with location on your computer
#'     )
#' }
#'

read_word_embeddings <- function(in_vocab,
                                 ovefile = NA,
                                 ovefile2 = NA,
                                 wikfile = NA,
                                 twifile = NA) {
    ##

    if (!requireNamespace("readr", quietly = TRUE)) {
        stop(
            "Package \"readr\" needed for this function to work. Please install it.",
            call. = FALSE
            )
    }
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop(
            "Package \"dplyr\" needed for this function to work. Please install it.",
            call. = FALSE
            )
    }
    if (!requireNamespace("tidyr", quietly = TRUE)) {
        stop(
            "Package \"tidyr\" needed for this function to work. Please install it.",
            call. = FALSE
            )
    }
    `%>%` <- magrittr::`%>%`

    ##
    embeddings <- list()
    ## find words in meta embeddings -------------------------------------------
    thefilter <- function(x, pos) subset(x, word %in% unique(in_vocab))
    if (!is.na(ovefile)) {
        ## read first meta file ------------------------------------------------
        cat("\nPulling overlapping words from meta word embeddings..\n")
        embeddings[["meta"]] <- readr::read_delim_chunked(
            ovefile,
            callback = readr::DataFrameCallback$new(thefilter),
            delim = "\t",
            progress = T,
            chunk_size = 100000,
            col_names = c("word", "M"),
            col_types = readr::cols(.default = readr::col_character(),
                                   word = readr::col_character()),
            quote = "",
            comment = "",
            quoted_na = F,
            na = character()
            )
        ## separate value columns
        embeddings[["meta"]] <- tidyr::separate(
            data = embeddings[["meta"]],
            col = M,
            paste0("M", 1:200),
            sep = " ",
            convert = T
            )
        if (!is.na(ovefile2)) {
            if (is.na(ovefile)) {
                stop(
                    "This file \"O2M_oov.txt\" is a supplement to \"O2M_overlap.txt\" and will not work well without it. Please specify the location of \"O2M_overlap.txt\".",
                    call. = FALSE
                )
            }
            ## read second meta file -------------------------------------------
            cat("    pulling rest of words from meta word embeddings..\n")
            prepembeddingsmeta <- readr::read_delim_chunked(
                ovefile2,
                callback = readr::DataFrameCallback$new(thefilter),
                delim = "\t",
                progress = T,
                chunk_size = 100000,
                col_names = c("word", "M"),
                col_types = readr::cols(.default = readr::col_character(),
                                        word = readr::col_character()),
                quote = "",
                comment = "",
                quoted_na = F,
                na = character()
                )
            ## separate value columns
            prepembeddingsmeta <- tidyr::separate(
                data = prepembeddingsmeta,
                    col = M,
                    paste0("M", 1:201),
                    sep = " ", convert = T
                    )
            ## combine meta files ---------------------------------------------
            embeddings[["meta"]] <- dplyr::bind_rows(
                embeddings[["meta"]],
                prepembeddingsmeta
                )
            embeddings[["meta"]] <- dplyr::select_if(embeddings[["meta"]],
                                                     ~sum(!is.na(.)) > 0)
        }
        embeddings[["meta"]] <- data.frame(embeddings[["meta"]])
        rownames(embeddings[["meta"]]) <- embeddings[["meta"]]$word
        embeddings[["meta"]] <- embeddings[["meta"]] %>%
            dplyr::select(-word)
        embeddings[["meta"]] <- as.matrix(embeddings[["meta"]])
        cat("\nFound", nrow(embeddings[["meta"]]), "words.\n")
        ##
    }
    ## find words in Wikipedia embeddings --------------------------------------
    if (!is.na(wikfile)) {
        cat("\nPulling words from Wikipedia word embeddings..\n")
        embeddings[["wikipedia"]] <- readr::read_delim_chunked(
            wikfile,
            callback = readr::DataFrameCallback$new(thefilter),
            progress = T,
            chunk_size = 100000,
            delim = " ",
            col_names = c("word", paste0("W", 1:300)),
            col_types = readr::cols(.default = readr::col_double(),
                                    word = readr::col_character()),
            quote = "",
            comment = "",
            quoted_na = F,
            na = character()
            )
        embeddings[["wikipedia"]] <- data.frame(embeddings[["wikipedia"]])
        rownames(embeddings[["wikipedia"]]) <- embeddings[["wikipedia"]]$word
        embeddings[["wikipedia"]] <- embeddings[["wikipedia"]] %>%
            dplyr::select(-word)
        embeddings[["wikipedia"]] <- as.matrix(embeddings[["wikipedia"]])
        cat("\nFound", nrow(embeddings[["wikipedia"]]), "words.\n")
        ##
    }
    ## find words in Twitter embeddings ----------------------------------------
    if (!is.na(twifile)) {
        cat("\nPulling words from Twitter word embeddings..\n")
        embeddings[["twitter"]] <- readr::read_delim_chunked(
            twifile,
            callback = readr::DataFrameCallback$new(thefilter),
            progress = T,
            chunk_size = 1000000,
            delim = " ",
            col_names = c("word", paste0("T", 1:200)),
            col_types = readr::cols(.default = readr::col_double(),
                                    word = readr::col_character()),
            quote = "",
            comment = "",
            quoted_na = F,
            na = character()
            )
        embeddings[["twitter"]] <- data.frame(embeddings[["twitter"]])
        rownames(embeddings[["twitter"]]) <- embeddings[["twitter"]]$word
        embeddings[["twitter"]] <- embeddings[["twitter"]] %>%
            dplyr::select(-word)
        embeddings[["twitter"]] <- as.matrix(embeddings[["twitter"]])
        cat("\nFound", nrow(embeddings[["twitter"]]), "words.\n")
    }
    return(embeddings)
}
