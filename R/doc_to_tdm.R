#' @rdname doc_to_tdm
#' @export
#'
#' @title
#' Convert list of documents to sparse term-document matrix
#'
#' @description
#' \code{doc_to_tdm} converts list of documents from stm package
#' \code{prepDocuments} function to a sparse term-document matrix.
#'
#' @param out A list of the output from stm \code{prepDocuments}.
#' @param binary A logical scalar. If TRUE (default) then only count one occurrence of a word in a
#' document.
#'
#' @examples
#'
#' \dontrun{
#' library(stm)
#'
#' processed <- textProcessor(
#'     input_data$text,
#'     data.frame(input_data),
#'     removestopwords=T, lowercase=T, stem=F
#'     )
#' out <- prepDocuments(
#'     processed$documents, processed$vocab, processed$meta
#'     )
#'
#' tdm <- doc_to_tdm(out)
#' }
#'
#' @seealso \code{\link[stm]{prepDocuments}},
#' \code{\link[stm]{textProcessor}},
#' \code{\link{scale_text}},
#' \code{\link{read_word_embeddings}},
#' \code{\link{get_keywords}}, \code{\link{plot_keywords}},
#' \code{\link{score_documents}}
#'

doc_to_tdm <- function(out,
                       binary = TRUE) {
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop(
            "Package \"reshape2\" needed for this function to work. Please install it.",
            call. = FALSE
            )
    }

    d2 <- reshape2::melt(lapply(out$documents, function(x) x[1, ]))
    d2 <- data.frame(
        reshape2::melt(
                      lapply(out$documents, function(x) x[1, ])
                  ),
        count = reshape2::melt(
                              lapply(out$documents, function(x) x[2, ])
                          )[ ,1]
        )

    if (binary) {
        tdm <- Matrix::sparseMatrix(
                           as.numeric(d2[ ,2]),
                           d2[ ,1],
                           x = rep(1, length(d2[ ,3]))
                       )
        } else {
            tdm <- Matrix::sparseMatrix(
                               as.numeric(d2[ ,2]),
                               d2[ ,1],
                               x = d2[ ,3])
        }

    tdm <- tdm[Matrix::rowSums(tdm) > 0, ]

    colnames(tdm) <- out$vocab
    rownames(tdm) <- names(out$documents)

    return(tdm)
}
