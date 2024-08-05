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

    best_combinations <- cv_results[which.max(cv_results$mean_abs_cor), ]
    best_lambda <- best_combinations$lambda
    best_nonzero <- best_combinations$nonzero

    result <- list(
        cv_results = cv_results,
        best_lambda = best_lambda,
        best_nonzero = best_nonzero
    )

    return(result)
}
