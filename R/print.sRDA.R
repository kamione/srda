#' Title
#'
#' @param x x
#' @param ... x
#'
#' @return x
#' @export
#'
print.sRDA <- function(x, ...) {
    cat("Sparse Redundancy Analysis (sRDA)", "\n")
    cat(rep("-", 80), sep="")
    cat("\n   NAME               ", "DESCRIPTION")
    cat("\n1  $component         ", "sRDA component number")
    cat("\n2  $explanatory       ", "exploratory dataset(s)")
    cat("\n3  $response          ", "response dataset(s)")
    cat("\n4  $XI                ", "latent variable(s) of exploratory dataset")
    cat("\n5  $ETA               ", "latent variable(s) of response dataset")
    cat("\n6  $ALPHA             ", "weights of exploratory dataset's latent variable")
    cat("\n7  $BETA              ", "weights of response dataset's latent variable")
    cat("\n8  $reg_coefficient   ", "regression coefficient of the latent component")
    cat("\n9  $sum_of_square_rhos", "sum of square correlations (XI ~ Y)")
    cat("\n10 $n_iterations      ", "number of iterations ran before convergence (or max number of iterations)")
    cat("\n11 $inverse_of_XIXI   ", "inverse of XI * XI (used for multiple latent variable calculation)")
    cat("\n12 $iterations_crts   ", "convergence criterion value")
    cat("\n13 $sum_absolute_betas", "sum of the absolute values of beta weights")
    cat("\n14 $sum_square_betas  ", "sum of the square values of beta weights")
    cat("\n15 $selected_lambda   ", "ridge penalty parameter")
    cat("\n16 $selected_nonzero  ", "number of nonzero alpha weights in the model")
    cat("\n17 $cv_results        ", "detailed results of cross validations")
    cat("\n")
    cat(rep("-", 80), sep="")

    return(invisible(x))
}
