#' @rdname plot_keywords
#' @export
#'
#' @title
#' Plots keywords from \code{scale_text}
#'
#' @description
#' \code{plot_keywords} plots keywords for each scaled text dimension.
#'
#' @param scores List from output of \code{scale_text}.
#' @param x_dimension An integer scalar. Dimension for x axis.
#' @param y_dimension An integer scalar. Dimension for y axis.
#' @param q_cutoff A numeric (0-1) scalar. Proportion of words to plot. For
#' example, setting q_cutoff=0.9 will plot the most common 10\% of words.
#' @param plot_density A logical scalar. Plot both biplot and density.
#' @param unstretch A logical scalar. Move pivot words toward origin.
#' @param color A logical scalar. Color words in plot. If vector \code{color} is
#' not provided in \code{scores}, will assign 5 colors using k-means on
#' dimensions 1 through 10.
#'
#' @examples
#' \dontrun{
#' scores <- scale_text(
#'     meta=out$meta,
#'     tdm=tdm
#'     )
#'
#' plot_keywords(
#'     scores, x_dimension=1, y_dimension=2, q_cutoff=0.9
#'     )
#' }
#'
#' @seealso \code{\link{scale_text}},
#' \code{\link{read_word_embeddings}},
#' \code{\link{get_keywords}},
#' \code{\link{score_documents}}, \code{\link{doc_to_tdm}}
#'

plot_keywords <- function(scores,
                          x_dimension = 1,
                          y_dimension = 2,
                          q_cutoff = 0.9,
                          plot_density = FALSE,
                          unstretch = FALSE,
                          color = FALSE) {

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "Package \"ggplot2\" needed for this function to work. Please install it.",
            call. = FALSE
            )
    }

    if (!requireNamespace("gridExtra", quietly = TRUE) & plot_density) {
        stop(
            "Package \"gridExtra\" needed for option \"plot_density=TRUE\" to work. Please install it.",
            call. = FALSE
            )
    }

    if (unstretch) {

        scores$word_scores <- sweep(
            scores$word_scores, 1,
            sqrt(rowSums((scores$importance[-1] * scores$pivot_scores[ ,-1])^2))
            + 1,
            `/`
        )

    }

    ## format data -------------------------------------------------------------
    word_scores <- data.frame(scores$word_scores)
    word_counts <- scores$word_counts

    above_cutoff <- word_counts>quantile(word_counts, q_cutoff)

    x_dimension <- x_dimension + 1
    y_dimension <- y_dimension + 1

    if (color & !("color" %in% names(scores))) {
        scores$color <- factor(
            kmeans(
                scores$word_scores[ ,2:11],
                centers=5
            )$cluster
        )
    }

    ## color plot --------------------------------------------------------------
    if (!color) {
        g <- ggplot2::ggplot() +
            ggplot2::geom_text(
                         data=word_scores[above_cutoff, ],
                         ggplot2::aes(
                                      x=word_scores[above_cutoff,x_dimension],
                                      y=word_scores[above_cutoff,y_dimension],
                                      size=word_counts[above_cutoff] *
                                          sqrt(word_scores[above_cutoff, x_dimension]^2 +
                                               word_scores[above_cutoff, y_dimension]^2),
                                      label=scores$vocab[above_cutoff]
                                  )
                     ) +
            ggplot2::xlab(paste("Dimension:",x_dimension - 1)) +
            ggplot2::ylab(paste("Dimension:", y_dimension - 1)) +
            ggplot2::guides(size=F) + ggplot2::theme_classic() +
            ggplot2::xlim(
                         -max(abs(word_scores[above_cutoff,x_dimension])),
                         max(abs(word_scores[above_cutoff,x_dimension]))
                     ) +
            ggplot2::ylim(
                         -max(abs(word_scores[above_cutoff,y_dimension])),
                         max(abs(word_scores[above_cutoff,y_dimension]))
                     )
    } else {
        ## bw plot  ------------------------------------------------------------
        g <- ggplot2::ggplot() +
            ggplot2::geom_text(
                         data=word_scores[above_cutoff, ],
                         ggplot2::aes(
                                      x=word_scores[above_cutoff,x_dimension],
                                      y=word_scores[above_cutoff,y_dimension],
                                      size=word_counts[above_cutoff] *
                                          sqrt(word_scores[above_cutoff, x_dimension]^2 +
                                               word_scores[above_cutoff, y_dimension]^2),
                                      label=scores$vocab[above_cutoff],
                                      color=scores$color[above_cutoff]
                                  )
                     ) +
            ggplot2::xlab(paste("Dimension:",x_dimension - 1)) +
            ggplot2::ylab(paste("Dimension:", y_dimension - 1)) +
            ggplot2::guides(size=F, color=F) + ggplot2::theme_classic() +
            ggplot2::xlim(
                         -max(abs(word_scores[above_cutoff,x_dimension])),
                         max(abs(word_scores[above_cutoff,x_dimension]))
                     ) +
            ggplot2::ylim(
                         -max(abs(word_scores[above_cutoff,y_dimension])),
                         max(abs(word_scores[above_cutoff,y_dimension]))
                     )
    }

    ## print -------------------------------------------------------------------
    if (!plot_density) {
        print(g)
    } else {
        ## add density ---------------------------------------------------------
        gridExtra::grid.arrange(
                       g,
                       ##
                       ggplot2::ggplot() +
                       ggplot2::geom_density(
                                    ggplot2::aes(x=word_scores[ ,x_dimension])
                                ) +
                       ggplot2::xlab(
                                    paste("Dimension:", x_dimension - 1)
                                ) + ggplot2::theme_classic(),
                       ##
                       ggplot2::ggplot() +
                       ggplot2::geom_density(
                                    ggplot2::aes(x=word_scores[ ,y_dimension])
                                ) +
                       ggplot2::xlab(
                                    paste("Dimension", y_dimension - 1)
                                ) + ggplot2::theme_classic(),
                       ##
                       layout_matrix=rbind(c(1,1,2),c(1,1,3))
                   )
    }

}
