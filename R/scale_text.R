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
#' bet. If the method does not converge at 2, try lowering
#' \code{n_dimension_compression} to the sqrt of the vocabulary size. If that
#' does not work, you might need to run without out-of-sample embeddings.
#' @param embeddings_ratio A numeric scalar. Ratio of out-of-sample word
#' embeddings to in-sample text for later scaling
#' @param embeddings_count_contribution A numeric scalar. Fraction of added
#' out-of-sample words to include as pivot words.
#' @param constrain_outliers A logical scalar. This requires in-sample words and
#' embedding scores for documents to have approximately unit norms. Recommended
#' for online surveys (reduce influence of bad data), focused survey questions,
#' and online social media data.
#' @param tdm_vocab A character vector. Provide vocabulary for columns of tdm if
#' missing in column names.
#' @param embeddings_vocab A character vector. Provide vocabulary for rows of
#' chosen embeddings if missing in row names.
#' @param verbose A logical scalar. Print progress of the function.
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
#'     embeddings = embeddings[["meta"]],
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
                       embeddings_ratio = 1/4,
                       embeddings_count_contribution = 1,
                       compress_fast = FALSE,
                       n_dimension_compression = NULL,
                       pivot = 2,
                       verbose = TRUE,
                       constrain_outliers = TRUE## ,
                       ## unfocused = TRUE
    )
{

    ## check vocab -------------------------------------------------------------
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

    ## check compression -------------------------------------------------------
    if (is.null(n_dimension_compression)) {
        was_null <- TRUE
        n_dimension_compression <- round(exp(1)^(log(ncol(tdm))/2 + 1))
    } else {
        was_null <- FALSE
    }

    if (is.null(embeddings_ratio)) {
        embeddings_ratio <- 1
    }

    if (is.null(embeddings_count_contribution)) {
        embeddings_count_contribution <- 1
    }

    ## prep embeddings ---------------------------------------------------------
    if (is.null(embeddings)) {
        vocab_intersect <- colnames(tdm)
        vocab_out <- vocab_intersect
    } else {
        vocab_intersect <- intersect(colnames(tdm), rownames(embeddings))
        ##
        tdm_orig <- tdm
        tdm <- tdm[ ,vocab_intersect]
        embeddings <- embeddings[match(vocab_intersect, rownames(embeddings)), ]
        ## if (!unfocused) {
        ## emb <- sweep(
        ##         as.matrix(tdm),
        ##     2, sqrt(Matrix::colSums(tdm)), `/`
        ## ) %*% embeddings
        ## } else {
        emb <- as.matrix(tdm) %*% embeddings
        ## }
        ## add noise for responses with no matches in embeddings
        emb[is.na(emb)] <- sample(emb, sum(is.na(emb)))
        emb_rowsums <- sqrt(rowSums(emb^2))
        emb_rowsums[emb_rowsums == 0] <- sample(
            emb_rowsums[emb_rowsums != 0],
            length(emb_rowsums[emb_rowsums == 0])
        )
        if (constrain_outliers) {
            emb <- emb / (emb_rowsums)
        }
        ##
        ## compress docs, map in-sample + out-of-sample to same space ----------
        if (verbose) cat("\nPreparing word embeddings..\n")
        ##
        if (!compress_fast) {
            if (verbose) cat("    in-sample data..\n")
            tdm_svd <- svd(
                as.matrix(tdm),
                nu = n_dimension_compression,
                nv = n_dimension_compression
            )
            tdm_pcs <- unname(
                as.matrix(tdm) %*% as.matrix(tdm_svd$v)
            )
            ##
            if (verbose) cat("    embeddings..\n")
            emb_svd_coefs <- svd(emb, nu = ncol(emb), nv = ncol(emb))$v
            emb_pcs <- unname(
                emb %*% emb_svd_coefs
            )
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
            if (verbose) cat("    in-sample data..\n")
            tdm_svd <- RSpectra::svds(
                                     tdm, k = n_dimension_compression
                                 )
            tdm_pcs <- unname(
                as.matrix(tdm) %*% as.matrix(tdm_svd$v)
            )
            ##
            if (verbose) cat("    embeddings..\n")
            emb_svd_coefs <- svd(
                emb, nu = ncol(emb)
            )$v
            emb_pcs <- unname(
                emb %*% emb_svd_coefs
            )
        }
        ##
        ## map in-sample and out-of-sample to same space -----------------------
        if (verbose) cat("    aligning..\n")
        emb_cca <- CCA::rcc(
                            X = as.matrix(tdm_pcs),
                            Y = emb_pcs[ ,-(1:1)],
                            lambda1 = cov(tdm_pcs)[1,1],
                            lambda2 = cov(emb_pcs)[2,2]
                        )

        ## reconstruct tdm -----------------------------------------------------
        tdm_supp <- (emb_cca$cor^(0.5) * emb_cca$scores$yscores) %*%
            t(emb_cca$xcoef) %*% t(tdm_svd$v)
        ## add words where above threshold
        tdm_supp <- tdm_supp > quantile(
                                   tdm_supp,
                                   1 - ((sum(tdm_orig) * embeddings_ratio) /
                                        length(tdm_supp))
                               )

        vocab_out <- c(
            colnames(tdm_orig),
            paste0(vocab_intersect[colSums(tdm_supp)>0], "_EMB")
        )

        tdm <- as(cbind(tdm_orig, tdm_supp[ ,colSums(tdm_supp)>0]), "dgCMatrix")
        colnames(tdm) <- vocab_out

        rownames(embeddings) <- NULL
        embeddings <- rbind(embeddings, embeddings[colSums(tdm_supp)>0, ])

        ## pare embeddings -----------------------------------------------------
        embeddings <- emb_cca$cor^(0.5) *
            ((embeddings %*% emb_svd_coefs)[ ,-(1:1)] %*% emb_cca$ycoef)

        rownames(embeddings) <-c(
            vocab_intersect,
            paste0(vocab_intersect[colSums(tdm_supp)>0], "_EMB")
        )

        if (was_null) {
            n_dimension_compression <- round(exp(1)^(log(ncol(tdm))/2 + 1))
        }

    }

    ## scale text --------------------------------------------------------------
    if (verbose) cat("Computing word co-occurrences..\n")

    cooccur <- as.matrix(Matrix::crossprod(tdm))
    if (!is.null(embeddings)) {
    used_vocab <- c(
        vocab_intersect,
        paste0(vocab_intersect[colSums(tdm_supp)>0], "_EMB")
    )
    } else {
        used_vocab <- vocab_intersect
    }
    ##
    word_counts <- diag(cooccur)
    if (!is.null(embeddings)) {
        word_counts[grepl("_EMB", vocab_out)] <- word_counts[grepl("_EMB", vocab_out)] *
            embeddings_count_contribution
        word_counts <- word_counts[vocab_out %in% used_vocab]
    }
    ##
    standardized_cooccur <- scale(
        sweep(cooccur, 1, diag(cooccur), `/`)^(1/2),
        scale = F
    )

    ## compress text -----------------------------------------------------------
    if (verbose) cat("Compressing text..\n")

    if (!compress_fast) {
        cooccur_svd_coefs <- svd(
            as.matrix(standardized_cooccur),
            nu = n_dimension_compression, nv = n_dimension_compression
        )$v
        rotated_data <- unname(
            as.matrix(standardized_cooccur) %*% as.matrix(cooccur_svd_coefs)
        )
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
        thesvd_coefs <- RSpectra::svds(
            as(crossprod(standardized_cooccur), "dgCMatrix"),
            k = n_dimension_compression
            )$v
        rotated_data <- unname(
            as.matrix(standardized_cooccur) %*% as.matrix(thesvd_coefs)
            )
    }

    ## pivot 1 -----------------------------------------------------------------
    if (verbose) cat("Regularizing words and n grams..\n")
    if (verbose) cat("    power:", pivot,"\n")

    if (is.null(embeddings)) {
        X1 <- rotated_data
        } else {
            X1 <- rotated_data[vocab_out %in% used_vocab, ]
            tdm <- tdm[ ,vocab_out %in% used_vocab]
            vocab_out <- vocab_out[vocab_out %in% used_vocab]
            }

    reg <- cov(X1[ ,1:2])[1,1]

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
    Cxx1 <- var(X1, na.rm = TRUE, use = "pairwise") +
        diag(reg, ncol(X1))
    Cyy1 <- var(Y1, na.rm = TRUE, use = "pairwise")
    Cxy1 <- cov(X1, Y1, use = "pairwise")
    res1 <- fda::geigen(Cxy1, Cxx1, Cyy1)
    names(res1) <- c("cor", "xcoef", "ycoef")
    ## shortened "comput" from package "CCA"
    X1.aux = scale(X1, center = TRUE, scale = FALSE)
    Y1.aux = scale(Y1, center = TRUE, scale = FALSE)
    ##
    word_scores1 = X1.aux %*% res1$xcoef
    aux_pivots = sqrt(
        rowSums(((Y1.aux %*% res1$ycoef)^2))
    )
    ##


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

    return(
        list(
            unadjusted_importance = res2$cor,
            vocab = vocab_out,
            tdm = tdm,
            meta = meta,
            word_scores = word_scores,
            word_counts = word_counts,
            pivot_scores = pivot_scores
        )
    )

}
