#' Get penalized weights with univariate soft-thresholding
#'
#' This is an internal function. (UST; ridge = Inf)
#'
#' @inheritParams elasticnet::enet
#' @param nonzero The number of non-zero alpha weights of the explanatory latent variable (Xi)
#'
#' @return A matrix of penalized weights
#'
#' @examples
#' testing
.get_ust = function(x, y, nonzero) {

    beta = as.numeric(t(x) %*% y)
    # select the highest values to be non-zero
    # setting RIDGE to infinity indicates that highest values will be selected
    penalized_weights = rep(0, length(beta))
    select = order(abs(beta), decreasing = TRUE)[1:nonzero]
    penalized_weights[select] = beta[select]
    names(penalized_weights) <- colnames(x)

    return(as.matrix(penalized_weights))
}
