#' Get penalized weights with the elastic net
#'
#' This is an internal function.
#'
#' @inheritParams elasticnet::enet
#' @param nonzero x
#'
#' @return A matrix of penalized weights
#'
#' @examples
#' testing
.get_enet = function(x, y, lambda, nonzero) {

    max.steps = nonzero + 1

    colnames(x) = paste("x", 1:ncol(x), sep = ".")
    epsilon.enet = elasticnet::enet(
        x, y, lambda = lambda, max.steps = max.steps
    )[[4]]
    # reorder
    penalized_weights = rep(0, ncol(x))
    # empty vector of regression coefficients
    penalized_weights[colnames(x) %in% colnames(epsilon.enet)] <- epsilon.enet[max.steps, ]
    names(penalized_weights) <- colnames(x)

    return(as.matrix(penalized_weights))
}
