#' @rdname score_documents
#' @export
#'
#' @title
#' Score documents
#'
#' @description
#' \code{score_documents} uses word scores to assign document scores
#'
#' @param scores A list from output of \code{scale_text}
#' @param n_dimensions An integer scalar. How many dimensions of scaled text to
#' score.
#' @param doc_length_correction A numeric scalar. Power on document length:
#' document_scores / document_length^(doc_length_correction). Set so
#' that extremity on top dimensions is not strongly correlated with document
#' length.
#'
#' @examples
#'
#' \dontrun{
#' scores <- scale_text(
#'     meta = out$meta,
#'     tdm = tdm
#'     )
#'
#' document_scores <- score_documents(
#'     scores = scores, n_dimensions = 10
#'     )
#' }
#'
#' @seealso \code{\link{scale_text}},
#' \code{\link{read_word_embeddings}},
#' \code{\link{get_keywords}}, \code{\link{plot_keywords}},
#' \code{\link{doc_to_tdm}}

score_documents <- function(
                            scores, n_dimensions = 10,
                            doc_length_correction = 0.75
                            )
    {

        scores$tdm <- scores$tdm_orig[,colnames(scores$tdm_orig) %in%
                                       colnames(scores$tdm)
                                      ]

        ## score documents
        scored <- scores$tdm %*%
            as(scores$word_scores[ ,1:n_dimensions], "dgCMatrix")

        ##
        n_words <- Matrix::rowSums(scores$tdm)

        ## standardize by number of words
        scored <- as.matrix(
            scored
        ) / n_words^(doc_length_correction) # abs(X1)/abs(X2) should not be
                                            # strongly correlated w doc length

        ## convert document scores to data frame
        scored <- data.frame(scored)
        names(scored) <- paste0(
            "X",
            ## rename first dimension to X0
            as.numeric(gsub("X", "", names(scored))) - 1
        )

        ## combine meta data and document scores
        scored <- data.frame(
            scores$meta,
            n_words = n_words,
            as.matrix(scored)
            )

        return(scored)
    }
