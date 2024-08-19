#' Title
#'
#' @param X x
#' @param Y x
#' @param penalization x
#' @param lambdas x
#' @param nonzeros x
#' @param n_lvs x
#' @param tolerance x
#' @param max_iteration x
#' @param cv_n_folds x
#' @param parallel x
#' @param seed x
#'
#' @return x
#'
#' @examples
#' x
.run_multiple_lvs <- function(
    X, Y, penalization, lambdas, nonzeros, n_lvs, tolerance, max_iteration,
    cv_n_folds, parallel, seed
) {
    # residuals of X
    Res_X <- X
    ith_component <- 1
    stop_flag <- FALSE

    cat("A total of", n_lvs, "latent variables are being calculated.", "\n\n")

    results <- list()

    while (ith_component <= n_lvs && !stop_flag) {
        cat("Running component", ith_component, "\n")

        result <- srda(
            explanatory = Res_X,
            response = Y,
            penalization = penalization,
            lambdas = lambdas,
            nonzeros = nonzeros,
            tolerance = tolerance,
            cv_n_folds = cv_n_folds,
            parallel = parallel,
            max_iteration = max_iteration
        )
        # update component index
        result$component <- ith_component

        # calculate the residuals of X
        Res_X = apply(Res_X, 2, function(Xcol) {
                Xcol - result$inverse_of_XIXI %*% Xcol %*% t(result$XI)
            }
        )

        results[[ith_component]] <- result

        if (ith_component > 1) {
            stop_condition <- abs(
                result$sum_squared_betas - results[[ith_component - 1]]$sum_squared_betas
            )
        } else {
            stop_condition <- Inf
        }

        if (stop_condition < tolerance) {
            stop_flag <- TRUE
        }

        cat("Calculation of component", ith_component, "is completed! \n\n")
        ith_component <- ith_component + 1
    }

    return(results)
}
