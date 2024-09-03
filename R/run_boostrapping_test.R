#' Bootstrapping test for the significance of features in sRDA
#'
#' @param explanatory x
#' @param response x
#' @param lambda x
#' @param nonzero x
#' @param n_bootstrap Number of bootstrapping
#' @param ci Confidence interval
#'
#' @return a result data frame
#' @export
#'
#' @examples
#' x
#'

run_bootstrapping_test <- function(
    explanatory,
    response,
    lambda,
    nonzero,
    method,
    n_bootstrap = 1000,
    ci = 0.95
) {
    cat("Now performing bootstrapping to assess the significance of features: \n")
    pb = txtProgressBar(min = 0, max = n_bootstrap, width = 40, style = 3)
    alphas <- list()
    betas  <- list()

    n_subjects <- dim(explanatory)[1]

    for (iteration in 1:n_bootstrap) {

        if (method == "stability") {
            set.seed(iteration)
            resampled_index <- sample(1:n_subjects, n_subjects, replace = TRUE)
            resampled_x <- explanatory[resampled_index, ]
            resampled_y <- response[resampled_index, ]
        }

        result <- sRDA(
            explanatory = resampled_x,
            response = resampled_y,
            lambdas = lambda,
            nonzeros = nonzero,
            penalization = "enet",
            parallel = TRUE,
            max_iteration = 100
        )

        alphas[[iteration]] <- as.data.frame(result$ALPHA) %>%
            magrittr::set_colnames(paste0("iteration_", iteration))
        betas[[iteration]]  <- as.data.frame(result$BETA) %>%
            magrittr::set_colnames(paste0("iteration_", iteration))

        setTxtProgressBar(pb, iteration)

    }
    close(pb)

    results <- list()
    results$alphas <- do.call(bind_cols, alphas)
    results$betas <- do.call(bind_cols, betas)
    results$alpha_boots <- .calculate_bootstats(results$alphas, ci)
    results$beta_boots  <- .calculate_bootstats(results$betas, ci)

    return(results)
}
