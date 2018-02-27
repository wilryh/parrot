#' @rdname doc_to_tdm_fast
#' @export
#'
#' @title
#' Converts list of documents to sparse term-document matrix
#'
#' @description
#' \code{doc_to_tdm_fast} converts list of documents from stm package \code{prepDocuments} function
#' to a sparse term-document matrix.
#'
#' @param documents a list of documents from stm \code{prepDocuments}
#' @param weights weights to apply to the term-document matrix. must match vocab

doc_to_tdm_fast <- function(documents, weights=NULL) { #, vocab
    ## library(plyr)
    ## library(reshape2)
    ## library(Matrix)
    d2 <- reshape2::melt(lapply(documents, function(x) x[1,]))
    d2 <- data.frame(
    reshape2::melt(lapply(documents, function(x) x[1,])),
        count=reshape2::melt(lapply(documents, function(x) x[2,]))[,1]
        )

    if (is.null(weights)) {
        return(Matrix::sparseMatrix(as.numeric(d2[,2]), d2[,1], x=d2[,3]))
    } else {
        d2 <- dplyr::left_join(                #keeps order
            d2,
            data.frame(weights, L1=as.character(1:length(weights))),
            by="L1"
            )
        d2$weights[is.na(d2$weights)] <- 0
        return(Matrix::sparseMatrix(as.numeric(d2[,2]), d2[,1], x=d2[,3] * d2[,4]))
    }
}
