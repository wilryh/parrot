#' @rdname plot_keywords
#' @export
#'
#' @title
#' Plots keywords
#'
#' @description
#' \code{plot_keywords} plots keywords for each scaled text dimension
#'
#' @param scores list from output of \code{scale_text}
#' @param x_dimension integer scalar. dimension for x axis
#' @param y_dimension integer scalar. dimension for y axis
#' @param q_cutoff numeric (0-1) scalar. proportion of words to plot
#' @param plot_density logical scalar. plot both biplot and density
#' @param print_keywords logical scalar. plot biplot, density, and keywords
#' @param unstretch unstretch space before plotting
#' @param n_words number of keywords to print
#'

plot_keywords <- function(
    scores, x_dimension=1, y_dimension=2, q_cutoff=0.9, plot_density=FALSE,
    unstretch=FALSE, print_keywords=T, n_words=12
    )
{
    ## library(ggplot2)
    ## library(grid)
    ## library(gridExtra)
    ## library(gtable)
    ## library(knitr)

    if (unstretch) {

    scores$word_scores <- sweep(
        scores$word_scores, 1,
        sqrt(rowMeans(scores$aux_pivots^2))
        / mean(sqrt(scores$word_counts))
        + 1,
        `/`
        )

        ## scores$word_scores <- sweep(
        ##     scores$word_scores, 1,
        ##     sqrt(rowMeans(scores$aux_pivots^2))
        ##     / mean(sqrt(scores$word_counts))
        ##     + mean(sqrt(rowMeans(scores$aux_pivots^2)))
        ##     ,
        ##     `/`
        ##     )
    }

    word_scores <- data.frame(scores$word_scores)
    word_counts <- scores$word_counts

    g <- ggplot2::ggplot() +
        ggplot2::geom_text(
            data=word_scores[word_counts>quantile(word_counts, q_cutoff),],
            ggplot2::aes(
                x=word_scores[word_counts>quantile(word_counts, q_cutoff),x_dimension],
                y=word_scores[word_counts>quantile(word_counts, q_cutoff),y_dimension],
                size=word_counts[word_counts>quantile(word_counts, q_cutoff)],
                label=scores$vocab[word_counts>quantile(word_counts, q_cutoff)]
                )
            ) +
                ggplot2::xlab(paste("Dimension:",x_dimension)) +
                    ggplot2::ylab(paste("Dimension:", y_dimension)) +
                    ggplot2::guides(size=F) + ggplot2::theme_classic()

    if (print_keywords) {
        i <- x_dimension
        table1 <- ## kable(
            dplyr::data_frame(
                `<--`=head(
                    scores$vocab[order(
                        -scores$word_scores[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    ),
                ## `<-`=head(
                ##     scores$vocab[order(
                ##         -scores$aux_pivots[,i],
                ##         decreasing=T
                ##         )],
                ##     n=n_words
                ##     ),
                ## `->`=head(
                ##     scores$vocab[order(
                ##         scores$aux_pivots[,i],
                ##         decreasing=T
                ##         )],
                ##     n=n_words
                ##     )## ,
                `-->`=head(
                    scores$vocab[order(
                        scores$word_scores[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    )
                )## , align="c",format="pandoc",
        ## caption=paste("Dimension", i, "keywords")
        ## )[-2]

        i <- y_dimension
        table2 <- ## kable(
            dplyr::data_frame(
                `<--`=head(
                    scores$vocab[order(
                        -scores$word_scores[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    ),
                ## `<-`=head(
                ##     scores$vocab[order(
                ##         -scores$aux_pivots[,i],
                ##         decreasing=T
                ##         )],
                ##     n=n_words
                ##     ),
                ## `->`=head(
                ##     scores$vocab[order(
                ##         scores$aux_pivots[,i],
                ##         decreasing=T
                ##         )],
                ##     n=n_words
                ##     )## ,
                `-->`=head(
                    scores$vocab[order(
                        scores$word_scores[,i] * scores$pivot_scores,
                        decreasing=T
                        )],
                    n=n_words
                    )
                )## , align="c",format="pandoc",
        ## caption=paste("Dimension", i, "keywords")
        ## )[-2]

        mytheme <- gridExtra::ttheme_default(
            core = list(fg_params=list(cex = 0.75)),
            colhead = list(fg_params=list(cex = 0.75)),
            rowhead = list(fg_params=list(cex = 0.5))
            )


        table1 <- gridExtra::tableGrob(table1, rows=NULL, theme = mytheme)
        table2 <- gridExtra::tableGrob(table2, rows=NULL, theme = mytheme)

        title1 <- grid::textGrob(paste("Dimension", x_dimension, "keywords"),gp=grid::gpar(fontsize=9))
        title2 <- grid::textGrob(paste("Dimension", y_dimension, "keywords"),gp=grid::gpar(fontsize=9))
        padding <- grid::unit(5,"mm")

        table1 <- gtable::gtable_add_rows(
            table1,
            heights = grid::grobHeight(title1) + padding,
            pos = 0)
        table1 <- gtable::gtable_add_grob(
            table1,
            title1,
            1, 1, 1, ncol(table1))

        table2 <- gtable::gtable_add_rows(
            table2,
            heights = grid::grobHeight(title2) + padding,
            pos = 0)
        table2 <- gtable::gtable_add_grob(
            table2,
            title2,
            1, 1, 1, ncol(table2))


    }

    if (!plot_density & !print_keywords) {
        print(g)
    } else if (plot_density) {
        gridExtra::grid.arrange(
            g,
            ggplot2::ggplot() + ggplot2::geom_density(ggplot2::aes(x=word_scores[,x_dimension])) +
            ggplot2::xlab(paste("Dimension:", x_dimension)) + ggplot2::theme_classic(),
            ggplot2::ggplot() + ggplot2::geom_density(ggplot2::aes(x=word_scores[,y_dimension])) +
            ggplot2::xlab(paste("Dimension", y_dimension)) + ggplot2::theme_classic(),
            layout_matrix=rbind(c(1,1,2),c(1,1,3))
            )
    } else if (print_keywords) {
        gridExtra::grid.arrange(
            g,
            table1,
            table2,
            ggplot2::ggplot() + ggplot2::geom_density(ggplot2::aes(x=word_scores[,x_dimension])) +
            ggplot2::xlab(paste("Dimension:", x_dimension)) + ggplot2::theme_classic(),
            ggplot2::ggplot() + ggplot2::geom_density(ggplot2::aes(x=word_scores[,y_dimension])) +
            ggplot2::xlab(paste("Dimension", y_dimension)) + ggplot2::theme_classic(),
            layout_matrix=rbind(c(1,1,1,2,3),c(1,1,1,2,3),c(1,1,1,4,5))
            )
    }

}
