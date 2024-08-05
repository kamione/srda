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
#' @examples
#' x
.run_parallel_cv <- function(
    X, Y, lambdas, nonzeros, label, cv_n_folds, penalization, max_iteration,
    tolerance
) {

    n_cores <- detectCores() - 1 # avoid exhaustion of CPU cores
    cl <- makeCluster(n_cores, type = "SOCK")
    registerDoSNOW(cl)
    getDoParWorkers()

    pb <- txtProgressBar(min = 0, max = length(lambdas), style = 3)
    progress <- function(n) {
        setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)

    # Get the alphas with parallel computing
    result <- foreach(
        # lambda = lambdas,
        lambda = rep(lambdas, times = length(nonzeros)),
        nonzero = rep(nonzeros, each = length(lambdas)),
        .combine = rbind,
        #.export = c("enet", ".srda_core", ".get_enet"),
        .packages = c("srda"),
        .options.snow = opts
    ) %dopar% {
        cv_abs_cors <- rep(NA, cv_n_folds)
        cv_max_iteration <- rep(NA, cv_n_folds)
        for (ith_fold in 1:cv_n_folds) {
            X.train <- X[label != ith_fold, ]
            X.test  <- X[label == ith_fold, ]
            Y.train <- Y[label != ith_fold, ]
            Y.test  <- Y[label == ith_fold, ]
            res <- .srda_core(
                X = X.train,
                Y = Y.train,
                lambda = lambda,
                nonzero = nonzero,
                max_iteration = max_iteration,
                penalization = penalization,
                tolerance = tolerance
            )
            XI.test = scale(X.test) %*% res$ALPHA
            cv_abs_cors[ith_fold] <- sum(
                abs(cor(XI.test, Y.test)), na.rm = TRUE
            )
            cv_max_iteration[ith_fold] <- res$n_iterations
        }

        iter_result <- data.frame(
            lambda = lambda,
            nonzero = nonzero,
            mean_abs_cor = mean(cv_abs_cors),
            abs_cors = I(list(cv_abs_cors)),
            max_iteration = I(list(cv_max_iteration))
        )

        return(iter_result)

    } # end of lambda for loop
    close(pb)
    stopCluster(cl)

    return(result)
}
