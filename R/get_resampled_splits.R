#' Title
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
