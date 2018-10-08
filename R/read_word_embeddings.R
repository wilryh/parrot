#' @rdname read_word_embeddings
#' @export
#'
#' @title
#' Reads vocab from word embedding files quickly and with little memory use
#'
#' @description
#' \code{read_word_embeddings} reads vocab from word embedding text files
#'
#' @details
#' Wikipedia embeddings: http://nlp.stanford.edu/data/glove.6B.zip
#' Twitter embeddings: http://nlp.stanford.edu/data/glove.twitter.27B.zip
#' Meta embeddings: http://www.cis.uni-muenchen.de/~wenpeng/renamed-meta-emb.tar.gz
#'
#' @param indata character vector. vocab in data set
#' @param ovefile O2M_overlap.txt from meta embeddings
#' @param ovefile2 O2M_oov.txt from meta embeddings
#' @param wikfile glove.6B.300d.txt from Wikipedia embeddings
#' @param twifile glove.twitter.27B.200d.txt from Twitter embeddings
#'

read_word_embeddings <- function(indata, ovefile=NA, ovefile2=NA, wikfile=NA, twifile=NA) {
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
    ##
    embeddings <- list()
    ## find words in Wikipedia embeddings
    thefilter <- function(x, pos) subset(x, word %in% unique(indata))
    if (!is.na(ovefile)) {
        cat("\nPulling overlapping words from meta word embeddings..\n")
        embeddings[["meta"]] <- readr::read_delim_chunked(
            ovefile,
            callback = readr::DataFrameCallback$new(thefilter),
            delim="\t",
            progress = T, chunk_size = 100000,
            col_names=c("word", "M"),
            col_types=readr::cols(
                .default=readr::col_character(), word=readr::col_character()
                ),
            quote="", comment="", quoted_na=F, na=character()
            )
        ## separate value columns
        embeddings[["meta"]] <- tidyr::separate(
            data=embeddings[["meta"]],
            ## tidyr::separate(
            col=M,
            paste0("M", 1:200),
            sep=" ", convert=T
            )
        if (!is.na(ovefile2)) {
            cat("    pulling rest of words from meta word embeddings..\n")
            prepembeddingsmeta <- readr::read_delim_chunked(
                ovefile2,
                callback = readr::DataFrameCallback$new(thefilter),
                delim="\t",
                progress = T, chunk_size = 100000,
                col_names=c("word", "M"),
                col_types=readr::cols(.default=readr::col_character(), word=readr::col_character()),
                quote="", comment="", quoted_na=F, na=character()
                )
            ## separate value columns
            prepembeddingsmeta <- tidyr::separate(
                data=prepembeddingsmeta,
                    col=M,
                    paste0("M", 1:201),
                    sep=" ", convert=T
                    )
            embeddings[["meta"]] <- dplyr::bind_rows(
                embeddings[["meta"]],
                prepembeddingsmeta
                )
            embeddings[["meta"]] <- select_if(embeddings[["meta"]], ~sum(!is.na(.)) > 0)
        }
        embeddings[["meta"]] <- data.frame(embeddings[["meta"]])
        rownames(embeddings[["meta"]]) <- embeddings[["meta"]]$word
        embeddings[["meta"]] <- embeddings[["meta"]] %>% dplyr::select(-word)
        embeddings[["meta"]] <- as.matrix(embeddings[["meta"]])
        cat("\nFound", nrow(embeddings[["meta"]]), "words.\n")
        ##
    }
    if (!is.na(wikfile)) {
        cat("\nPulling words from Wikipedia word embeddings..\n")
        embeddings[["wikipedia"]] <- readr::read_delim_chunked(
            wikfile,
            callback = readr::DataFrameCallback$new(thefilter),
            progress = T, chunk_size = 100000,
            delim=" ", col_names=c("word", paste0("W", 1:300)),
            col_types=readr::cols(.default=readr::col_double(), word=readr::col_character()),
            quote="", comment="", quoted_na=F, na=character()
            )
        embeddings[["wikipedia"]] <- data.frame(embeddings[["wikipedia"]])
        rownames(embeddings[["wikipedia"]]) <- embeddings[["wikipedia"]]$word
        embeddings[["wikipedia"]] <- embeddings[["wikipedia"]] %>% dplyr::select(-word)
        embeddings[["wikipedia"]] <- as.matrix(embeddings[["wikipedia"]])
        cat("\nFound", nrow(embeddings[["wikipedia"]]), "words.\n")
        ##
    }
    ## find words in Twitter embeddings
    ## only used in training
    if (!is.na(twifile)) {
        cat("\nPulling words from Twitter word embeddings..\n")
        embeddings[["twitter"]] <- readr::read_delim_chunked(
            twifile,
            callback = readr::DataFrameCallback$new(thefilter),
            progress = T, chunk_size = 1000000,
            delim=" ", col_names=c("word", paste0("T", 1:200)),
            col_types=readr::cols(.default=readr::col_double(), word=readr::col_character()),
            quote="", comment="", quoted_na=F, na=character()
            )
        embeddings[["twitter"]] <- data.frame(embeddings[["twitter"]])
        rownames(embeddings[["twitter"]]) <- embeddings[["twitter"]]$word
        embeddings[["twitter"]] <- embeddings[["twitter"]] %>% dplyr::select(-word)
        embeddings[["twitter"]] <- as.matrix(embeddings[["twitter"]])
        cat("\nFound", nrow(embeddings[["twitter"]]), "words.\n")
    }
    return(embeddings)
}
