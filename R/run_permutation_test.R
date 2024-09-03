#' Permutation test for the significance of features in sRDA
#'
#' @param rda x
#' @param n_permutation x
#' @param plot x
#' @return a result data frame
#' @export
#'
#' @import parallel
#' @import doSNOW
#' @import foreach
#'
#' @examples
#' x
#'

run_permutation_test <- function(rda, n_permutation, plot = FALSE) {

    if (!.check_rda_class(rda)) {
        stop("The input is not of class 'sRDA'.")
    }

    n_cores <- parallel::detectCores() - 1 # avoid exhaustion of CPU cores
    cl <- parallel::makeCluster(n_cores, type = "SOCK")
    doSNOW::registerDoSNOW(cl)
    foreach::getDoParWorkers()

    cat("Now performing a permutation test to assess the significance of RDA componenet: \n")
    # prgoress bar
    pb <- txtProgressBar(min = 0, max = n_permutation,width = 80, style = 3)
    progress <- function(n) {
        setTxtProgressBar(pb, n)
    }
    opts <- list(progress = progress)

    n_subjects <- dim(rda$explanatory)[1]
    permuted_ssr <- numeric(n_permutation)

    permuted_ssr <- foreach::foreach(
        ith_perm = 1:n_permutation,
        .combine = c,
        .packages = c("srdax"),
        .options.snow = opts
    ) %dopar% {
        set.seed(ith_perm)
        resampled_index <- sample(1:n_subjects, n_subjects, replace = FALSE)
        resampled_x <- rda$explanatory[resampled_index, ]
        result <- srda(
            explanatory = resampled_x,
            response = rda$response,
            lambdas = rda$selected_lambda,
            nonzeros = rda$selected_nonzero,
            penalization = "enet",
            max_iteration = 100
        )
        return(result$sum_squared_betas)
    }
    parallel::stopCluster(cl)
    close(pb)

    results <- list()
    results$empirical_rho <- rda$sum_squared_betas
    results$permuted_rhos <- permuted_ssr
    results$p_value <- sum(permuted_ssr > empirical_ssr) / (1 + n_permutation)
    return(results)
}

.check_rda_class <- function(object) {
    if (class(object) == "sRDA") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

.plot_null_distribution <- function(dat) {

    if (dat$p_value == 0) {
        dat$p_value = "< 0.001"
    } else {
        dat$p_value = paste0("= ", round(dat$p_value, 3))
    }

    annotation_text <- paste0(
        "*β* = ",
        round(dat$empirical_rho, 3),
        ", *p* ",
        dat$p_value
    )

    fig <- data.frame(x = dat$permuted_rhos) %>%
        ggplot(aes(x = x)) +
        geom_density(color = "gray70", fill = "gray70") +
        geom_vline(
            xintercept = dat$empirical_rho,
            color = "tomato3",
            linetype = "dashed",
            linewidth = 1
        ) +
        labs(
            x = "Standardized Coefficients (*β*)",
            y = "",
            title = annotation_text
        ) +
        ggthemes::theme_pander() +
        theme(
            plot.margin = margin(5, 5, 5, 5, "mm"),
            plot.title = ggtext::element_markdown(),
            axis.title.x = ggtext::element_markdown(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
    return(fig)
}
