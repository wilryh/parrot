#' @rdname get_keywords
#' @export
#'
#' @title
#' Lists keywords
#'
#' @description
#' \code{get_keywords} lists keywords for each scaled text dimension
#'
#' @param scores list from output of \code{scale_text}
#' @param n_dimensions number of dimensions used to produce keywords
#' @param n_words how many keywords for each dimension
#'

get_keywords <- function(scores, n_dimensions, n_words=15) {
    ## library(knitr)

    scores$word_scores <- sweep(
        scores$word_scores, 1,
        sqrt(rowMeans(scores$aux_pivots^2))
        / mean(sqrt(scores$word_counts)) +1,
        `/`
        )

    for (i in 1:n_dimensions) {
        ## cat("\nDimension", i, "keywords:")
        print(knitr::kable(
            dplyr::data_frame(
                `<--`=head(
                    scores$vocab[order(
                        -scores$word_scores[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    ),
                `<-`=head(
                    scores$vocab[order(
                        -scores$aux_pivots[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    ),
                `->`=head(
                    scores$vocab[order(
                        scores$aux_pivots[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    ),
                `-->`=head(
                    scores$vocab[order(
                        scores$word_scores[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    )
                ), align="c",format="pandoc",
            caption=paste("Dimension", i, "keywords")
            ))
        cat("\n")
    }


}
