#' @rdname get_keywords
#' @export
#'
#' @title
#' List keywords from \code{scale_text}
#'
#' @description
#' \code{get_keywords} lists keywords for each scaled text dimension.
#'
#' @param scores List from output of \code{scale_text}.
#' @param n_dimensions Integer scalar or vector. How many/which dimensions to
#' print.
#' @param n_words An integer scalar. How many keywords for each dimension.
#' @param pivots_only A logical scalar. Whether to show only pivot in keywords.
#' @param stretch An integer scalar. Must be positive, odd integer. Reduce
#' importance of pivot words in 'specific' keywords.
#' @param capture_output A logical scalar. Whether to return the output as list
#' of data frames rather than print to console.
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
#' @seealso \code{\link{read_word_embeddings}},
#' \code{\link{plot_keywords}},
#' \code{\link{score_documents}}, \code{\link{doc_to_tdm}}
#'

get_keywords <- function(scores,
                         n_dimensions,
                         n_words = 15,
                         stretch = 3,
                         capture_output = FALSE,
                         pivots_only = TRUE
                         ) {

    all_keywords <- list()

    if (stretch %% 2 != 1) stop("Please enter odd integer for \"stretch\"")

    for (i in if (length(n_dimensions) == 1) {1:n_dimensions} else {n_dimensions}) {
        general_keywords <- scores$vocab[order(
                                   scores$pivot_scores[ ,i+1] *
                                   sqrt(rowSums(scores$pivot_scores[ ,-1]^2)),
                                   decreasing = TRUE
                               )]
        ##
        specific_keywords <- scores$vocab[order(
                                    scores$word_scores[ ,i+1]^(stretch) *
                                    sqrt(rowSums(scores$pivot_scores[ ,-1]^2)),
                                    decreasing = TRUE
                                )]
        ##

        if (pivots_only) {
            keywords <- data.frame(
                head(
                    rev(general_keywords),
                    n = n_words
                ),
                head(
                    general_keywords,
                    n = n_words
                )
            )
            names(keywords) <- c("pivots (-)","(+) pivots")
        } else {
            keywords <- data.frame(
                head(
                    rev(specific_keywords),
                    n = n_words
                ),
                head(
                    rev(general_keywords),
                    n = n_words
                ),
                head(
                    general_keywords,
                    n = n_words
                ),
                head(
                    specific_keywords,
                    n = n_words
                )
            )
            names(keywords) <- c("scores (-)","pivots (-)","(+) pivots","(+) scores")
        }

        if (capture_output) {

            all_keywords[[paste0("D", i)]] <- keywords

        } else {

            if (!requireNamespace("knitr", quietly = TRUE)) {
                ##
                cat("\nDimension", i, "keywords\n\n")
                print(keywords, row.names = F)
                cat("\n")
            } else {
                ##
                print(
                    knitr::kable(
                               keywords,
                               align = "c",
                               format = "pandoc",
                               caption = paste("Dimension", i, "keywords")
                           )
                )
                cat("\n")
            }
        }

    }

    if (capture_output) {

        return(all_keywords)

    }

}
