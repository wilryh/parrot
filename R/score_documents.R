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
#' @param n_dimensions how many dimensions of scaled text to use

score_documents <- function(scores, n_dimensions=10)
    {

        scored <- scores$tdm %*%
            as(scores$word_scores[,1:n_dimensions], "dgCMatrix")

        ##
        n_words <- Matrix::rowSums(scores$tdm)
        ##
        scored <- as.matrix(
            scored
            ) / n_words
        ##
        scored <- data.frame(scored)
        names(scored) <- paste0("X", as.numeric(gsub("X", "", names(scored)))-1)
        ##
        scored <- data.frame(
            scores$meta,
            n_words=n_words,
            as.matrix(scored)
            )
        return(
            scored
            )
    }