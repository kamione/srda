#' The core algorithm of the sparse redundancy analysis (sRDA)
#'
#' This is an internal function.
#'
#' @param X A explanatory matrix or data frame;
#' @param Y A response matrix or data frame;
#' @param lambda The ridge penalty parameter of the explanatory latent variable (Xi) used for enet
#' @param nonzero The number of non-zero alpha weights of the explanatory latent variable (Xi) used for enet or ust
#' @param penalization The penalization method: "enet", "ust", "none"
#' @param max_iteration Maximum number of iterations
#' @param tolerance Convergence criteria
#'
#' @return A list
#'
#' @examples
#' testing

.srda_core <- function(
    X,
    Y,
    penalization,
    lambda,
    nonzero,
    max_iteration,
    tolerance
) {

    #1. Preparation of the data ------------------------------------------------
    # get columns only with finite values
    X_centered <- scale(as.matrix(X))
    X_centered <- X_centered[, !colSums(!is.finite(X_centered))]

    Y_centered <- scale(as.matrix(Y))
    Y_centered <- Y_centered[, !colSums(!is.finite(Y_centered))]

    # initiate weights of 1 for X's ALPHA and Y's BETA
    ALPHA = matrix(rep(1, ncol(X_centered)), ncol = 1)
    BETA = matrix(rep(1, ncol(Y_centered)), ncol = 1)

    #2. Iterative loop until convergence ---------------------------------------
    CRT <- Inf # initiate convergence criteria between alpha and beta
    CRTs <- numeric()
    abs_CRT_delta <- Inf
    ith_iteration <- 0
    stop_flag <- FALSE
    sum_abs_BETAs <- numeric()
    sum_squared_BETAs <- numeric()

    while (CRT > tolerance && !stop_flag && ith_iteration < max_iteration) {

        XI <- scale(X_centered %*% ALPHA)
        ETA <- scale(Y_centered %*% BETA)

        # Chose between generic RDA, sRDA with ENET or sRDA with UST
        ALPH_0 <- switch(
            penalization,
            # calculate with lm without penalization
            "none" = {solve(t(X_centered) %*% X_centered) %*% t(X_centered) %*% ETA},
            # calculate with elastic net
            "enet" = {.get_enet(X_centered, ETA, lambda, nonzero)},
            # calculate with UST
            "ust" = {.get_ust(X_centered, ETA, nonzero)}
        )

        # compute the value of XI^(1):
        #   XIhat^(1) = SUM_running to p where t=1 ( ahat_t^(0) *x_t )
        # normalize XIhat^(1) such that
        #   t(XIhat^(1))*XIhat^(1) = t(ahat^(0)) t(X)*X*ahat^(0) = 1
        #   t(XI)%*%XI = t(ALPH_0) %*% t(X_centered) %*% X_centered %*% ALPH_0 = 1
        #   that is its variance is 1
        XI <- scale(X_centered %*% ALPH_0)

        # For the value BETAhat^(1) and hence ETAhat^(1),
        # regress y1, y2, ..., yq separately on XIhat^(1),
        #   y_1 = BETA_1 * XIhat^(1) + Epsilon_1
        #   ...
        #   y_q = BETA_q * XIhat^(1) + Epsilon_q
        BETA_0 <- t(as.matrix(solve(t(XI) %*% XI) %*% t(XI) %*% Y_centered))

        # compute the vaule of ETAhat^(1),
        #   ETAhat^(1) = SUM_running to q where k=1 (BETAhat_k^(1) * y_k)
        ETA <- scale(Y_centered %*% BETA_0)

        # calculate convergence criteria
        CRT <- sum((ALPHA - ALPH_0)^2, (BETA - BETA_0)^2)

        # update ALPHA and BETA
        ALPHA <- ALPH_0
        BETA  <- BETA_0

        # check if last two iterations CR converges
        ith_iteration <- ith_iteration + 1
        CRTs[ith_iteration] <- CRT
        sum_abs_BETAs[ith_iteration] <- sum(abs(BETA))
        sum_squared_BETAs[ith_iteration] <- sum(BETA^2)

        if (ith_iteration > 1) {
            abs_CRT_delta <- abs(CRTs[ith_iteration] - CRTs[ith_iteration - 1])
        }

        if (abs_CRT_delta < tolerance) {
            stop_flag <- TRUE
        }
    }

    # residual: use this later for second latent variable
    inverse_of_XIXI = solve(t(XI) %*% XI) %*% t(XI)

    # Given that the data are standardized, the regression coefficients are the
    # same as the correlation coefficients
    reg_coefficient <- cor(XI, ETA)[1, 1]

    result <- list(
        component = 1, # this is a default value
        explanatory = X,
        response = Y,
        XI = XI,
        ETA = ETA,
        ALPHA = ALPHA,
        BETA = BETA,
        reg_coefficient = reg_coefficient,
        n_iteration = ith_iteration,
        inverse_of_XIXI = inverse_of_XIXI,
        iterations_crts = CRTs,
        sum_absolute_betas = sum_abs_BETAs[ith_iteration],
        sum_squared_betas = sum_squared_BETAs[ith_iteration]
    )

    class(result) <- "sRDA"
    return(result)
}
