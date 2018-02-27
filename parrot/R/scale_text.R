#' @rdname scale_text
#' @export
#'
#' @title
#' Scale text using canonical pivot analysis
#'
#' @description
#' \code{scale_text} runs pivoted text scaling
#'
#' @param tdm sparseMatrix. rows are documents, columns are vocab
#' @param vocab character vector. must align with columns of tdm
#' @param embedding_values matrix of embedding values
#' @param embedding_vocab character vector. vocab for rows of embedding values
#' @param compress_full logical scalar. TRUE for small data sets
#' @param n_dimension_compression integer scalar. how many dimensions of PCA to use
#' @param n_grams logical scalar. set to false unless data is extremely clean
#' @param meta list of data. output of stm's \code{prepDocuments}. must line up with tdm etc
#'

scale_text <- function(
    tdm, vocab, embedding_values=NULL, embedding_vocab=NULL, compress_full=FALSE,
    n_dimension_compression=200, n_grams=TRUE, meta
    )
{
    ## library(ForeCA)                     #ForeCA::whiten
    ## library(CCA)
    ## library(Matrix)
    ## library(boot)                       #inv.logit
    ## library(fda)

    if (!is.null(embedding_vocab)) {
        cat("\nRemoving vocabulary not in word embeddings..\n")
        vocab_sub <- vocab[vocab%in%embedding_vocab]
        tdm_sub <- tdm[,vocab%in%embedding_vocab]
        ## word_counts_sub <- Matrix::colSums(tdm_sub)
    } else {
        vocab_sub <- vocab
        tdm_sub <- tdm
    }

    cat("Computing word co-occurrences..\n")
    cooccur <- as.matrix(Matrix::crossprod(tdm_sub))
    word_counts_sub <- diag(cooccur)
    standardized_cooccur <- scale(sweep(cooccur, 1, word_counts_sub, `/`), scale=F)

    cat("Compressing text..\n")
    if (compress_full) {
        rotated_data <- unname(
            as.matrix(
                prcomp(standardized_cooccur, rank.=n_dimension_compression)$x
                ## [,1:n_dimension_compression]
                )
            )
    } else {
        loadings <- irlba::irlba(
            standardized_cooccur, nv=n_dimension_compression, fastpath=F, maxit=500
            )$v
        rotated_data <- unname(
            as.matrix(standardized_cooccur) %*% as.matrix(loadings)
            )
    }

    if (!is.null(embedding_values) & !is.null(embedding_vocab)) {
        cat("Aligning word embeddings..\n")
        vocab_loc <- match(vocab_sub, embedding_vocab)
        ##
        embedding_vocab_sub <- embedding_vocab[vocab_loc]
        embedding_values_sub <- embedding_values[vocab_loc,]
    }

    cat("Regularizing words and n grams..\n")
    X1 <- rotated_data
    Y1 <- sweep(ForeCA::whiten(rotated_data)$U, 1, word_counts_sub^4, `*`)

    ## thercc1 <- rcc(X1, Y1, lambda1=100, lambda2=0)
    ## word_scores1 = thercc1$scores$xscores
    ## pivot_scores = sqrt(rowSums(thercc1$scores$yscores^2))
    ## ##
    reg <- cov(X1[,1:2])[1,1]

    ## regularized canonical correlation analysis (mostly, rcc from CCA)
    ## regularization step
    Cxx1 <- ((var(X1, na.rm = TRUE, use = "pairwise"))) +
        diag(reg, ncol(X1))
    Cyy1 <- var(Y1, na.rm = TRUE, use = "pairwise")
    Cxy1 <- cov(X1, Y1, use = "pairwise")
    res1 <- fda::geigen(Cxy1, Cxx1, Cyy1)
    names(res1) <- c("cor", "xcoef", "ycoef")
    ## shortened "comput" from package "CCA"
    X1.aux = scale(X1, center = TRUE, scale = FALSE)
    Y1.aux = scale(Y1, center = TRUE, scale = FALSE)
    ##
    X1.aux[is.na(X1.aux)] = 0
    Y1.aux[is.na(Y1.aux)] = 0
    ##
    word_scores1 = X1.aux %*% res1$xcoef
    pivot_scores = sqrt(rowSums((res1$cor * (Y1.aux %*% res1$ycoef))^2))
    ##

    regularized_word_scores1 <- sweep(
        word_scores1, 1,
        (pivot_scores/max(pivot_scores)) + sqrt(rowSums(word_scores1^2)) +
        ## 1,
        ifelse(n_grams, mean(sqrt(rowSums(word_scores1^2))), 0),
        ## sqrt(pivot_scores),
        `/`
        )

    cat("Pivoting..\n")
    X2 <- scale(regularized_word_scores1, scale=F)
    if (is.null(embedding_values)) {
        cat("    (no word embeddings provided)\n")
        Y2 <- sweep(
            ForeCA::whiten(rotated_data)$U, 1,
            pivot_scores *
            sqrt(word_counts_sub),
            `*`
            )
    } else {
        Y2 <- sweep(
            embedding_values_sub, 1,
            pivot_scores *
            sqrt(word_counts_sub),
            `*`
            )
    }

    ## regularized canonical correlation analysis (rcc)
    ## pivot step
    Cxx2 <- diag(1, ncol(X2))
    Cyy2 <- var(Y2, na.rm = TRUE, use = "pairwise")##  +
    ## diag(100, ncol(Y2))
    ## Cyy2 <- Cyy2 + diag(max(Cyy2), ncol(Y2))
    Cxy2 <- cov(X2, Y2, use = "pairwise")
    res2 <- fda::geigen(Cxy2, Cxx2, Cyy2)
    names(res2) <- c("cor", "xcoef", "ycoef")
    ##
    X2.aux = scale(X2, center = TRUE, scale = FALSE)
    Y2.aux = scale(Y2, center = TRUE, scale = FALSE)
    ##
    X2.aux[is.na(X2.aux)] = 0
    Y2.aux[is.na(Y2.aux)] = 0
    ##

    word_scores = X2.aux %*% res2$xcoef
    aux_pivots = Y2.aux %*% res2$ycoef
    shrink_pivots = sqrt(rowSums(aux_pivots^2))
    ##

    return(
        list(
            importance = res2$cor,
            vocab = vocab_sub,
            tdm = tdm_sub,
            meta = meta,
            word_scores = word_scores,
            word_counts = word_counts_sub,
            pivot_scores = pivot_scores,
            shrink_pivots = shrink_pivots,
            aux_pivots = aux_pivots,
            reg=reg
            )
        )

}
