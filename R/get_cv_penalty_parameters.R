#' Title
#'
#' @param explanatory x
#' @param response x
#' @param penalization x
#' @param lambdas x
#' @param nonzeros x
#' @param cv_n_folds x
#' @param parallel x
#' @param max_iteration x
#' @param tolerance x
#' @param seed x
#'
#' @return x
#'
#' @examples
#' x
.get_cv_penalty_parameters <- function(
    explanatory, response, penalization, lambdas, nonzeros, cv_n_folds,
    parallel, max_iteration, tolerance, seed
) {

    shuffled <- .get_resampled_splits(
        X = explanatory,
        Y = response,
        n_folds = cv_n_folds,
        seed = seed
    )

    X.sampled <- shuffled$X.sampled
    Y.sampled <- shuffled$Y.sampled
    label <- shuffled$labels

    cat("running cross-validation ... \n")

    if (parallel) {
        cv_results <- .run_parallel_cv(
            X = X.sampled,
            Y = Y.sampled,
            lambdas = lambdas,
            nonzeros = nonzeros,
            label = label,
            cv_n_folds = cv_n_folds,
            penalization = penalization,
            max_iteration = max_iteration,
            tolerance = tolerance
        )
    } else {
        stop("function '.get_non_parallel_cv' is not ready", call. = FALSE)
        cv_results <- .run_non_parallel_cv(
            X = X.sampled,
            Y = Y.sampled,
            lambdas = lambdas,
            nonzeros = nonzeros,
            label = label,
            penalization = penalization,
            max_iteration = max_iteration,
            tolerance = tolerance
        )
    }

    best_lambda <- cv_results[which.max(cv_results$mean_absolute_rhos), "lambda"]
    best_nonzero <- cv_results[which.max(cv_results$mean_absolute_rhos), "nonzero"]

    result <- list(
        cv_results = cv_results,
        best_lambda = best_lambda,
        best_nonzero = best_nonzero
    )

    return(result)
}


#' Title
#'
#' This is an internal function
#'
#' @param X x
#' @param Y x
#' @param n_folds x
#' @param seed x
#'
#' @return x
#'
#' @examples x
.get_resampled_splits <- function(X, Y, n_folds, seed) {

    if (missing(seed)) {
        seed <- 1234
    }

    n <- dim(X)[1]
    #re-sample data rows
    set.seed(seed)
    splitting_dimensions <- sample(1:n, n)

    X.sampled <- X[splitting_dimensions, ]
    Y.sampled <- Y[splitting_dimensions, ]

    # calculate how many rows are left over
    leftover = n %% n_folds

    labels = rep(1:n_folds, each = (n - leftover) / n_folds)

    if(leftover != 0) {
        labels = c(labels, (1:n_folds)[1:leftover])
    }

    result <- list(
        X.sampled = X.sampled,
        Y.sampled = Y.sampled,
        labels = labels
    )

    return(result)
}
