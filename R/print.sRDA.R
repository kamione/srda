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
    cat("\n   NAME              ", "DESCRIPTION")
    cat("\n1  $component        ", "sRDA component number")
    cat("\n2  $explanatory      ", "exploratory dataset(s)")
    cat("\n3  $response         ", "response dataset(s)")
    cat("\n4  $XI               ", "latent variable(s) of exploratory dataset")
    cat("\n5  $ETA              ", "latent variable(s) of response dataset")
    cat("\n6  $ALPHA            ", "weights of exploratory dataset's latent variable")
    cat("\n7  $BETA             ", "weights of response dataset's latent variable")
    cat("\n8  $n_iterations     ", "number of iterations ran before convergence (or max number of iterations)")
    cat("\n9  $inverse_of_XIXI  ", "inverse of XI * XI (used for multiple latent variable calculation)")
    cat("\n10 $iterations_crts  ", "convergence criterion value")
    cat("\n11 $sum_abs_betas    ", "sum of the absolute values of beta weights")
    cat("\n12 $selected_lambda  ", "ridge penalty parameter")
    cat("\n13 $selected_nonzero ", "number of nonzero alpha weights in the model")
    cat("\n14 $cv_results       ", "detailed results of cross validations")
    cat("\n")
    cat(rep("-", 80), sep="")

    return(invisible(x))
}
