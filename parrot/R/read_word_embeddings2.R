#' @rdname read_word_embeddings2
#' @export
#'
#' @title
#' Reads vocab from word embedding files quickly and with little memory use
#'
#' @description
#' \code{read_word_embeddings2} reads vocab from word embedding text files
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

read_word_embeddings2 <- function(indata, ovefile=NA, ovefile2=NA, wikfile=NA, twifile=NA) {
    library(readr)
    embeddings <- list()
    ## find words in Wikipedia embeddings
    thefilter <- function(x, pos) subset(x, word %in% unique(indata))
    if (!is.na(ovefile)) {
        cat("\nPulling overlapping words from meta word embeddings..\n")
        embeddings[["meta"]] <- readr::read_delim_chunked(
            ovefile,
            callback = DataFrameCallback$new(thefilter),
            delim="\t",
            progress = T, chunk_size = 100000,
            col_names=c("word", "M"),
            col_types=cols(.default=col_character(), word=col_character()),
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
                callback = DataFrameCallback$new(thefilter),
                delim="\t",
                progress = T, chunk_size = 100000,
                col_names=c("word", "M"),
                col_types=cols(.default=col_character(), word=col_character()),
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
                select(data=prepembeddingsmeta, -M201) #empty column
                )
        }
        cat("\nFound", nrow(embeddings[["meta"]]), "words.\n")
        ##
    }
    if (!is.na(wikfile)) {
        cat("\nPulling words from Wikipedia word embeddings..\n")
        embeddings[["wikipedia"]] <- readr::read_delim_chunked(
            wikfile,
            callback = DataFrameCallback$new(thefilter),
            progress = T, chunk_size = 100000,
            delim=" ", col_names=c("word", paste0("W", 1:300)),
            col_types=cols(.default=col_double(), word=col_character()),
            quote="", comment="", quoted_na=F, na=character()
            )
        cat("\nFound", nrow(embeddings[["wikipedia"]]), "words.\n")
        ##
    }
    ## find words in Twitter embeddings
    ## only used in training
    if (!is.na(twifile)) {
        cat("\nPulling words from Twitter word embeddings..\n")
        embeddings[["twitter"]] <- readr::read_delim_chunked(
            twifile,
            callback = DataFrameCallback$new(thefilter),
            progress = T, chunk_size = 1000000,
            delim=" ", col_names=c("word", paste0("T", 1:200)),
            col_types=cols(.default=col_double(), word=col_character()),
            quote="", comment="", quoted_na=F, na=character()
            )
        cat("\nFound", nrow(embeddings[["twitter"]]), "words.\n")
    }
    return(embeddings)
}
