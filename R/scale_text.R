#' @rdname scale_text
#' @export
#'
#' @title
#' Scale text using pivoted text scaling
#'
#' @description
#' \code{scale_text} runs pivoted text scaling
#'
#' @param tdm A sparseMatrix. Rows are documents and columns are vocabulary.
#' @param compress_fast A logical scalar. use R base (F) or RSpectra (T)
#' @param embeddings A numeric matrix. A matrix of embedding values.
#' @param n_dimension_compression An integer scalar. How many dimensions of PCA
#' to use. The algorithm will not work if this is set too high. If left NULL, a
#' recommended number of dimensions will be calculated automatically.
#' @param meta data.frame. Must line up with tdm etc. This is included only to
#' keep track of any accompanying variables. It is unaltered by the function.
#' @param pivot An integer scalar. This is the power of the pivot. It should be
#' set as high as possible as long as algorithm still works. 2 or 4 is a good
#' bet. If using out-of-sample embeddings, this can be set lower (e.g. 1/2).
#' @param constrain_outliers A logical scalar. This requires in-sample words and
#' embedding scores for documents to have approximately unit norms. Recommended
#' for online surveys (reduce influence of bad data), focused survey questions,
#' and online social media data.
#' @param tdm_vocab A character vector. Provide vocabulary for columns of tdm if
#' missing in column names.
#' @param embeddings_vocab A character vector. Provide vocabulary for rows of
#' chosen embeddings if missing in row names.
#' @param verbose A logical scalar. Print progress of the function.
#' @param simple A logical scalar. Pivot once.
#' @param holdout A logical or numeric vector. A logical or numeric vector
#' indicating which rows to exclude from training.
#'
#' @examples
#' \dontrun{
#' library(stm)
#' library(parrot)
#'
#' processed <- textProcessor(
#'     input_data$text,
#'     data.frame(input_data),
#'     removestopwords = T, lowercase = T, stem = F
#'     )
#' out <- prepDocuments(
#'     processed$documents, processed$vocab, processed$meta
#'     )
#'
#' tdm <- doc_to_tdm(out)
#'
#' # download and extract embeddings data first
#'
#' embeddings <- read_word_embeddings(
#'     in_vocab = out$vocab,
#'     ovefile = "O2M_overlap.txt" # must add location on your computer "path/to/O2M_overlap.txt"
#'     ## ovefile2 = "O2M_oov.txt", # very rare words and misspellings
#'     ## available here http://www.cis.uni-muenchen.de/~wenpeng/renamed-meta-emb.tar.gz
#'     ## must unpack and replace "path/to/" with location on your computer
#'     )
#'
#' scores <- scale_text(
#'     meta = out$meta,
#'     tdm = tdm,
#' ##    embeddings = embeddings[["meta"]], ## limited effects on output
#'     compress_fast = TRUE,
#'     constrain_outliers = TRUE
#'     )
#'
#' document_scores <- score_documents(
#'     scores = scores, n_dimensions = 10
#'     )
#'
#' get_keywords(scores, n_dimensions = 3, n_words = 15)
#'
#' with(document_scores, cor(sqrt(n_words), X0, use = "complete"))
#'
#' plot_keywords(
#'     scores, x_dimension = 1, y_dimension = 2, q_cutoff = 0.9
#'     )
#' }
#'
#' @seealso \code{\link{read_word_embeddings}},
#' \code{\link{get_keywords}}, \code{\link{plot_keywords}},
#' \code{\link{score_documents}}, \code{\link{doc_to_tdm}}
#'

scale_text <- function(tdm,
                       meta = NULL,
                       tdm_vocab = NULL,
                       embeddings = NULL,
                       embeddings_vocab = NULL,
                       compress_fast = TRUE,
                       n_dimension_compression = NULL,
                       pivot = 2,
                       verbose = TRUE,
                       constrain_outliers = FALSE,
                       simple = TRUE,
                       holdout = NULL,
                       approx = FALSE
    )
{

    ## check vocab -------------------------------------------------------------
    if (class(tdm)!="dgCMatrix") {
        tdm <- as(tdm, "dgCMatrix")
        }

    if (!is.null(embeddings_vocab)) {
        rownames(embeddings) <- embeddings_vocab
    }
    if (!is.null(tdm_vocab)) {
        colnames(tdm) <- tdm_vocab
    }

    if (is.null(rownames(embeddings)) & is.null(colnames(tdm)) & !is.null(embeddings)) {
        stop("\nPlease supply vocabulary of term-document matrix and word embeddings\n")
    }
    if (is.null(colnames(tdm))) {
        stop("\nPlease supply vocabulary of term-document matrix\n")
    }
    if (is.null(rownames(embeddings)) & !is.null(embeddings)) {
        stop("\nPlease supply vocabulary of word embeddings\n")
    }

    tdm_orig <- tdm
    if (!is.null(holdout)) {
        if (class(holdout)!="logical") {
            holdout <- as.numeric(as.character(holdout))
            holdout <- 1:nrow(tdm) %in% holdout
        }
        tdm <- tdm[!holdout,Matrix::colSums(tdm[!holdout,])>0]
    }

    ## prep embeddings ---------------------------------------------------------
    if (is.null(embeddings)) {
        vocab_intersect <- colnames(tdm)
        vocab_out <- vocab_intersect
    } else {
        vocab_intersect <- intersect(colnames(tdm), rownames(embeddings))
        vocab_out <- vocab_intersect
        ##
        used_embeddings <- TRUE
        tdm <- tdm[ ,vocab_intersect]
        embeddings <- embeddings[match(colnames(tdm), rownames(embeddings)), ]

    }

    ## scale text --------------------------------------------------------------
    if (verbose) cat("Computing word co-occurrences..\n")

    cooccur <- Matrix::crossprod(tdm)
    used_vocab <- vocab_intersect

    ##
    word_counts <- Matrix::diag(cooccur)
    if (!is.null(embeddings)) {
        word_counts <- word_counts[vocab_out %in% used_vocab]
    }
    ##
    standardized_cooccur <- as(cooccur, "dgCMatrix")
    ## standardize rows (sparse matrix is by column, already symmetric matrix)
    standardized_cooccur@x <- standardized_cooccur@x /
        rep.int(Matrix::colSums(standardized_cooccur), diff(standardized_cooccur@p))
    standardized_cooccur@x <- standardized_cooccur@x^(1/2)
    ## columns --> rows
    standardized_cooccur <- Matrix::t(standardized_cooccur)
    ## center matrix (unnecessary -- slowdown, not sparse)
    ## standardized_cooccur <- as(scale(standardized_cooccur, scale = FALSE), "dgCMatrix")

    ## check compression -------------------------------------------------------
    if (is.null(n_dimension_compression)) {
        was_null <- TRUE
        ## n_dimension_compression <- round(exp(1)^(log(ncol(tdm))/2 + 1))
        n_dimension_compression <- sum(
            Matrix::colSums(standardized_cooccur^2) >
            1
            ## Matrix::rowSums(standardized_cooccur^2)
        ) / 2 # approx
        if (n_dimension_compression > nrow(tdm)) {
            n_dimension_compression <- nrow(tdm)
        }
    } else {
        was_null <- FALSE
    }

    if (!is.null(embeddings)) {
        if (n_dimension_compression <= ncol(embeddings)) {
            embeddings_svd <- RSpectra::svds(
                                            embeddings,
                                            k = n_dimension_compression
                                        )
            embeddings <- embeddings_svd$u %*% diag(embeddings_svd$d)
        } else {
            warning("Embeddings smaller than proposed truncation.")
        }

    }

    ## compress text -----------------------------------------------------------
    if (verbose) cat("Compressing text..\n")

    if (approx) {
    if (!compress_fast) {
            cooccur_svd_coefs <- svd(
                standardized_cooccur[vocab_out %in% used_vocab, ][word_counts > sort(
                                                       word_counts, decreasing=T
                                                   )[n_dimension_compression],],
                ## not necessary to estimate all dimensions with hard truncation
                nu = round(sqrt(n_dimension_compression)),
                nv = round(sqrt(n_dimension_compression))
            )
        word_scores <- standardized_cooccur %*% cooccur_svd_coefs$v
        pivot_scores <- word_scores * as.numeric(word_counts > sort(
                                                                 word_counts, decreasing=T
                                                             )[n_dimension_compression])
    } else {
        if (!requireNamespace("RSpectra", quietly = TRUE)) {
            stop(
                paste(
                    "Package \"RSpectra\" needed for option",
                    "\"compress_fast = TRUE\" to work. Please install it."
                ),
                call. = FALSE
            )
        }
        cooccur_svd_coefs <- RSpectra::svds(
                                           as(standardized_cooccur[vocab_out %in% used_vocab, ][word_counts >
                                                                   sort(
                                                                       word_counts, decreasing=T
                                                                   )[n_dimension_compression],],
                                              "dgCMatrix"),
                                           k = round((n_dimension_compression)-5)
                                       )
        word_scores <- standardized_cooccur %*% cooccur_svd_coefs$v
        pivot_scores <- word_scores * as.numeric(word_counts >
            sort(
                word_counts, decreasing=T
            )[n_dimension_compression])
    }} else {
    if (!compress_fast) {
            cooccur_svd_coefs <- svd(
                standardized_cooccur,
                nu = n_dimension_compression,
                nv = n_dimension_compression
            )
        rotated_data <- cooccur_svd_coefs$u %*% diag(cooccur_svd_coefs$d)
    } else {
        if (!requireNamespace("RSpectra", quietly = TRUE)) {
            stop(
                paste(
                    "Package \"RSpectra\" needed for option",
                    "\"compress_fast = TRUE\" to work. Please install it."
                ),
                call. = FALSE
            )
        }
        cooccur_svd_coefs <- RSpectra::svds(
                                           as(standardized_cooccur, "dgCMatrix"),
                                           k = n_dimension_compression
                                       )
        rotated_data <- cooccur_svd_coefs$u %*% diag(cooccur_svd_coefs$d)
    }
    }

    if (!approx) {
    ## pivot 1 -----------------------------------------------------------------
    if (verbose) cat("Regularizing words and n grams..\n")
    if (verbose) cat("    power:", pivot,"\n")

    ## if (!is.null(embeddings) & pivot > 0.5) {
    ##     warning("Recommended pivot with embeddings is 1/2.")
    ##     }

    if (is.null(embeddings)) {
        X1 <- rotated_data
        } else {
            X1 <- rotated_data[vocab_out %in% used_vocab, ]
            tdm <- tdm[ ,vocab_out %in% used_vocab]
            vocab_out <- vocab_out[vocab_out %in% used_vocab]
            }

    reg <- cov(X1[ ,1:2])[1,1]

    if (constrain_outliers) {
        X1 <- sweep(
                X1, 1,
                sqrt(rowSums(X1^2))
               ,
                `/`
            )
    }

    if (is.null(embeddings)) {
    Y1 <- sweep(
        ForeCA::whiten(X1[ ,1:n_dimension_compression])$U, 1,
        word_counts^pivot
       ,
        `*`
    )
    } else {
    Y1 <- sweep(
        embeddings, 1,
        word_counts^pivot,
        `*`
    )
    }

    ## regularized canonical correlation analysis (mostly, rcc from CCA)
    ## regularization step
    Cxx1 <- diag(1, ncol(X1))
    Cyy1 <- var(Y1, na.rm = TRUE, use = "pairwise")
    Cxy1 <- cov(X1, Y1, use = "pairwise")
    res1 <- fda::geigen(Cxy1, Cxx1, Cyy1)
    names(res1) <- c("cor", "xcoef", "ycoef")
    ## shortened "comput" from package "CCA"
    X1.aux = scale(X1, center = TRUE, scale = FALSE)
    Y1.aux = scale(Y1, center = TRUE, scale = FALSE)
    ##
    word_scores1 = X1.aux %*% res1$xcoef
    pivot_scores1 = Y1.aux %*% res1$ycoef
    aux_pivots = sqrt(
        rowSums(((Y1.aux %*% res1$ycoef)^2))
    )
    ##


    if (simple) {

        word_scores <- word_scores1
        pivot_scores <- pivot_scores1
        res2 <- res1

    } else {

        ## pivot 2 -----------------------------------------------------------------
        if (constrain_outliers) {
            regularized_word_scores1 <- sweep(
                word_scores1, 1,
                sqrt(rowSums(word_scores1^2))
               ,
                `/`
            )
        } else {
            regularized_word_scores1 <- word_scores1
        }

        if (verbose) cat("Pivoting..\n")
        if (verbose) cat("    power:", pivot,"\n")

        X2 <- regularized_word_scores1

        if (is.null(embeddings)) {
            if (verbose) cat("    (no word embeddings provided)\n")
            Y2 <- sweep(
                ForeCA::whiten(X2)$U, 1,
                aux_pivots *
                word_counts^pivot
               ,
                `*`
            )
        } else {
            Y2 <- sweep(
            (embeddings), 1,
            aux_pivots *
            word_counts^pivot
           ,
            `*`
            )
        }

        ## regularized canonical correlation analysis (rcc)
        ## pivot step
        Cxx2 <- diag(1, ncol(X2))
        Cyy2 <- var(Y2, na.rm = TRUE, use = "pairwise")
        ##
        Cxy2 <- cov(X2, Y2, use = "pairwise")
        res2 <- fda::geigen(Cxy2, Cxx2, Cyy2)
        names(res2) <- c("cor", "xcoef", "ycoef")
        ##
        X2.aux = scale(X2, center = TRUE, scale = FALSE)
        Y2.aux = scale(Y2, center = TRUE, scale = FALSE)
        ##

        word_scores = X2.aux %*% res2$xcoef
        pivot_scores = Y2.aux %*% res2$ycoef

    }

    if (simple) {
        X2.aux <- X1.aux
        Y2.aux <- Y1.aux
    }
        }

    if (approx) {
    return(
        list(
            simple = simple,
            unadjusted_importance = cooccur_svd_coefs$d,
            constrain_outliers = constrain_outliers,
            holdout = holdout,
            vocab = vocab_out,
            tdm = tdm,
            tdm_orig = tdm_orig,
            meta = meta,
            word_scores = word_scores,
            word_counts = word_counts,
            pivot_scores = pivot_scores,
            X_orig = standardized_cooccur
        )
        )
    } else {
    return(
        list(
            simple = simple,
            unadjusted_importance = res2$cor,
            constrain_outliers = constrain_outliers,
            holdout = holdout,
            vocab = vocab_out,
            tdm = tdm,
            tdm_orig = tdm_orig,
            meta = meta,
            word_scores = word_scores,
            word_counts = word_counts,
            pivot_scores = pivot_scores,
            res1 = res1,
            res2 = res2,
            X_orig = standardized_cooccur,
            X1.aux = X1.aux,
            X2.aux = X2.aux,
            Y1.aux = Y1.aux,
            Y2.aux = Y2.aux
            ## simple = simple,
            ## unadjusted_importance = res2$cor,
            ## constrain_outliers = constrain_outliers,
            ## holdout = holdout,
            ## vocab = vocab_out,
            ## tdm = tdm,
            ## tdm_orig = tdm_orig,
            ## meta = meta,
            ## word_scores = word_scores,
            ## word_counts = word_counts,
            ## pivot_scores = pivot_scores
        )
    )
    }

}
