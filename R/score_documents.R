#' @rdname score_documents
#' @export
#'
#' @title
#' Score documents
#'
#' @description
#' \code{score_documents} uses word scores to assign document scores
#'
#' @param scores list from output of \code{scale_text}
#' @param n_dimensions how many dimensions of scaled text to score
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

score_documents <- function(scores, n_dimensions = 10)
    {

        ## score documents
        scored <- scores$tdm %*%
            as(scores$word_scores[ ,1:n_dimensions], "dgCMatrix")

        ##
        n_words <- Matrix::rowSums(scores$tdm)

        ## standardize by number of words
        scored <- as.matrix(
            scored
            ) / n_words

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
