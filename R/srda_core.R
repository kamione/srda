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
    lambda,
    nonzero,
    penalization,
    max_iteration,
    tolerance
) {

    #1. Preparation of the data ------------------------------------------------
    Y.mat <- as.matrix(Y)
    Yc <- scale(Y.mat)
    Yc <- Yc[, !colSums(!is.finite(Yc))]

    X.mat <- as.matrix(X)
    Xcr <- scale(X.mat)
    Xcr <- Xcr[, !colSums(!is.finite(Xcr))]

    # for initialization, let:
    #   ETA^(0) = y_1 + y_2 + ... + y_q = Y * BETA_hat^(0) ( = Y_i) where
    #   BETA^(0) = [1, 1, ..., 1]
    # an y-weight column vector of q elements
    BETA = matrix(rep(1, ncol(Yc)), ncol = 1, byrow = TRUE)
    # an x-weight column vector of p elements
    ALPHA = matrix(rep(1, ncol(Xcr)), ncol = 1, byrow = TRUE)

    #2. Iterative loop until convergence ---------------------------------------
    CRTs <- c()
    sum_abs_Betas <- c()
    n_iterations = 0 # iteration counter

    stop_flag <- FALSE
    CRT = 1 # initate convergence measure between alpha and beta

    while (CRT > tolerance && !stop_flag && n_iterations < max_iteration) {

        ETA = Yc %*% BETA
        ETA = scale(ETA)

        XI = Xcr %*% ALPHA
        XI = scale(XI)

        # Chose between generic RDA, sRDA with ENET or sRDA with UST
        ALPH_0 <- switch(
            penalization,
            # calculate with lm without penalization
            "none" = {solve(t(Xcr) %*% Xcr) %*% t(Xcr) %*% ETA},
            # calculate with elastic net
            "enet" = {.get_enet(Xcr, ETA, lambda, nonzero)},
            # calculate with UST
            "ust" = {.get_ust(Xcr, ETA, nonzero)}
        )

        # compute the value of XI^(1):
        #   XIhat^(1) = SUM_running to p where t=1 ( ahat_t^(0) *x_t )
        XI = Xcr %*% ALPH_0

        # normalize XIhat^(1) such that
        #   t(XIhat^(1))*XIhat^(1) = t(ahat^(0)) t(X)*X*ahat^(0) = 1
        #   t(XI)%*%XI = t(ALPH_0) %*% t(Xcr) %*% Xcr %*% ALPH_0 = 1
        #   that is its variance is 1
        XI = scale(XI)

        # For the value BETAhat^(1) and hence ETAhat^(1),
        # regress y1, y2, ..., yq separately on XIhat^(1),
        #   y_1 = BETA_1 * XIhat^(1) + Epsilon_1
        #   ...
        #   y_q = BETA_q * XIhat^(1) + Epsilon_q
        BETA_0 = solve(t(XI) %*% XI) %*% t(XI) %*% Yc
        BETA_0 = t(as.matrix(BETA_0))

        # compute the vaule of ETAhat^(1),
        #   ETAhat^(1) = SUM_running to q where k=1 (BETAhat_k^(1) * y_k)
        ETA = Yc %*% BETA_0
        ETA = scale(ETA)

        # calculate convergence of Alphas and Betas
        CRT = sum((ALPHA - ALPH_0)^2, (BETA - BETA_0)^2)

        # update alpha and beta
        ALPHA = ALPH_0
        BETA = BETA_0

        # check if last two iterations CR converges
        n_iterations = n_iterations + 1
        CRTs[[n_iterations]] = CRT
        sum_abs_Betas[[n_iterations]] = sum(abs(BETA))

        if (n_iterations > 1) {
            abs_crt_delta <- abs(CRTs[[n_iterations]] - CRTs[[n_iterations - 1]])
            if (abs_crt_delta < tolerance) {
                stop_flag <- TRUE
            }
        }
    }

    # use this later for second latent variable
    inverse_of_XIXI = solve(t(XI) %*% XI) %*% t(XI)

    result <- list(
        explanatory = X,
        response = Y,
        XI = XI,
        ETA = ETA,
        ALPHA = ALPHA,
        BETA= BETA,
        n_iterations = n_iterations,
        inverse_of_XIXI = inverse_of_XIXI,
        iterations_crts = CRTs,
        sum_absolute_betas = sum_abs_Betas
    )

    class(result) <- "sRDA"
    return(result)
}
