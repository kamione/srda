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
    comp_counter <- 1
    stop_condition <- Inf
    stop_flag <- FALSE

    cat("A total of", n_lvs, "latent variables are being calculated.", "\n\n")

    results <- list()

    while (comp_counter <= n_lvs && stop_flag == FALSE) {

        cat("Running component", comp_counter, "\n")
        results[[comp_counter]] <- srda(
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

        results[[comp_counter]]$component <- comp_counter

        reg_coeff <- results[[comp_counter]]$inverse_of_XIXI %*% as.matrix(Res_X)

        # calculate the residuals
        .calc_residuals <- function(Xcol) {
            Xcol - results[[comp_counter]]$inverse_of_XIXI %*% Xcol %*% t(results[[comp_counter]]$XI)
        }

        # update
        Res_X = apply(Res_X, 2, .calc_residuals)

        results[[comp_counter]]$sum_abs_betas <- sum(results[[comp_counter]]$BETA^2)

        if (comp_counter > 1) {
            stop_condition <- abs(
                results[[comp_counter]]$sum_abs_betas - results[[comp_counter - 1]]$sum_abs_betas
            )
        }

        if (stop_condition < tolerance) {
            stop_flag <- TRUE
        }

        cat("Calculation of component", comp_counter, "is completed! \n\n")
        comp_counter <- comp_counter + 1
    }

    return(results)

}
