sRDA_permutation_test <- function(rda, n_permutation, plot = FALSE) {

    if (!.check_rda_class(rda)) {
        stop("The input is not of class 'sRDA'.")
    }

    cat("Now performing a permutation test to assess the significance of RDA componenet: \n")
    pb = txtProgressBar(min = 0, max = n_permutation, width = 40, style = 3)

    n_subjects <- dim(rda$explanatory)[1]

    ssr <- sum((cor(rda$XI, rda$response))^2)

    permuted_rhos <- numeric(n_permutation)
    for (iteration in 1:n_permutation) {
        set.seed(iteration)
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

        permuted_rhos = c(permuted_rhos, sum((cor(result$XI, result$response))^2))

        setTxtProgressBar(pb, iteration)
    }
    close(pb)

    results <- list()
    results$empirical_rho <- empirical_rho
    results$p_value <- sum(empirical_rho > permuted_rhos)
    results$permuted_rhos <- permuted_rhos

    if (plot) {
        results$fig <- .plot_null_distribution(results)
    }

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
