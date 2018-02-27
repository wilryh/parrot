#' @rdname score_documents2
#' @export
#'
#' @title
#' Score documents
#'
#' @description
#' \code{score_documents2} uses word scores to assign document scores
#'
#' @param scores list from output of \code{scale_text}
#' @param n_dimensions how many dimensions of scaled text to use

score_documents2 <- function(scores, n_dimensions=10)
    {
        scored <- as.matrix(scores$tdm) %*% scores$word_scores[,1:n_dimensions]
        n_words <- Matrix::rowSums(scores$tdm)
        scored[,1:n_dimensions] <- as.matrix(scored) / n_words^(1)
        ## thescale <- 2 / sum(scores$importance[1:n_dimensions])
        ## e? sqrt(word_counts) not pi
        scored <- sapply(data.frame(pi * (scored)), tanh)
        scored <- data.frame(
            scores$meta,
            n_words=n_words,
            as.matrix(scored)
            )
        return(
            scored
            )
    }
