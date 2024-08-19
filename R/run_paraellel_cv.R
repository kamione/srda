#' Title
#'
#' @param X x
#' @param Y x
#' @param lambdas x
#' @param nonzeros x
#' @param label x
#' @param cv_n_folds x
#' @param penalization x
#' @param max_iteration x
#' @param tolerance x
#'
#' @return x
#'
#' @import parallel
#' @import doSNOW
#' @import foreach
#'
#' @examples
#' x
.run_parallel_cv <- function(
    X, Y, lambdas, nonzeros, label, cv_n_folds, penalization, max_iteration,
    tolerance
) {

    n_cores <- parallel::detectCores() - 1 # avoid exhaustion of CPU cores
    cl <- parallel::makeCluster(n_cores, type = "SOCK")
    doSNOW::registerDoSNOW(cl)
    foreach::getDoParWorkers()

    pb <- txtProgressBar(min = 0, max = length(lambdas), style = 3)
    progress <- function(n) {
        setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)

    # Get the alphas with parallel computing
    result <- foreach::foreach(
        lambda = rep(lambdas, each = length(nonzeros)),
        nonzero = rep(nonzeros, times = length(lambdas)),
        .combine = rbind,
        .packages = c("srdax"),
        .options.snow = opts
    ) %dopar% {
        cv_absolute_rhos <- numeric(cv_n_folds)
        cv_max_iteration <- numeric(cv_n_folds)
        for (ith_fold in 1:cv_n_folds) {
            X_train <- X[label != ith_fold, ]
            X_test  <- X[label == ith_fold, ]
            Y_train <- Y[label != ith_fold, ]
            Y_test  <- Y[label == ith_fold, ]
            result <- .srda_core(
                X = X_train,
                Y = Y_train,
                lambda = lambda,
                nonzero = nonzero,
                max_iteration = max_iteration,
                penalization = penalization,
                tolerance = tolerance
            )
            XI_test = scale(X_test) %*% result$ALPHA
            # store correlations between X_test and each columns of Y_test
            cv_absolute_rhos[ith_fold] <- sum(
                abs(cor(XI_test, Y_test)), na.rm = TRUE
            )
            cv_max_iteration[ith_fold] <- result$n_iteration
        }

        iter_result <- data.frame(
            lambda = lambda,
            nonzero = nonzero,
            mean_absolute_rhos = mean(cv_absolute_rhos),
            absolute_rhos = I(list(cv_absolute_rhos)),
            max_iteration = I(list(cv_max_iteration))
        )

        return(iter_result)

    } # end of lambda for loop
    close(pb)
    parallel::stopCluster(cl)

    return(result)
}
