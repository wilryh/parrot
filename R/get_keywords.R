#' @rdname get_keywords
#' @export
#'
#' @title
#' List keywords.
#'
#' @description
#' \code{get_keywords} lists keywords for each scaled text dimension
#'
#' @param scores list from output of \code{scale_text}
#' @param n_dimensions integer scalar/vector. How many/which dimensions to print.
#' @param n_words integer scalar. How many keywords for each dimension.
#' @param unstretch logical scalar. Reduce importance of pivot words in specific (<-- and -->) keywords.
#'
#' @examples
#' \dontrun{
#' scores <- scale_text(
#'     meta=out$meta,
#'     tdm=tdm
#'     )
#'
#' get_keywords(scores, n_dimensions=3, n_words=15)
#' }
#'

get_keywords <- function(scores, n_dimensions, n_words=15, unstretch=TRUE) {

    if (unstretch) {
        scores$word_scores <- sweep(
            scores$word_scores, 1,
            sqrt(
                rowSums((scores$unadjusted_importance[-1] * scores$pivot_scores[,-1])^2)
            ) + 1,
            `/`
        )
    }

    for (i in if (length(n_dimensions)==1) {1:n_dimensions } else {n_dimensions}) {
        general_keywords <- scores$vocab[order(
                                   scores$pivot_scores[,i+1] *
                                   sqrt(rowSums(scores$pivot_scores[,-1]^2)),
                                   decreasing=T
                               )]
        ##
        specific_keywords <- scores$vocab[order(
                                    scores$word_scores[,i+1]^3 *
                                    sqrt(rowSums(scores$pivot_scores[,-1]^2)), #scores$word_counts^(1/2)
                                    decreasing=T
                                )]
        ##
        keywords <- data.frame(
            head(
                rev(specific_keywords),
                n=n_words
            ),
            head(
                rev(general_keywords),
                n=n_words
            ),
            head(
                general_keywords,
                n=n_words
            ),
            head(
                specific_keywords,
                n=n_words
            )
        )
        names(keywords) <- c("<--","<-","->","-->")

        if (!requireNamespace("knitr", quietly = TRUE)) {
            ##
            cat("\nDimension", i, "keywords\n\n")
            print(keywords, row.names=F)
            cat("\n")
        } else {
            ##
            print(
                knitr::kable(
                           keywords, align="c",format="pandoc",
                           caption=paste("Dimension", i, "keywords")
                       )
            )
            cat("\n")
        }

    }

}
