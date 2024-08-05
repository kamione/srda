#' Check if a variable is a sRDA object
#' @param object any
#' @return logical; TRUE or FALSE
.check_rda_class <- function(object) {
    if (class(object) == "sRDA") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Check the penalization method inputs
#' @param method A string
#' @return logical; TRUE or FALSE
.check_penalization_method <- function(method) {
    if (!method %in% c("enet", "ust", "none")) {
        message <- paste(
            "Please select one of the penalization methods:",
            "'enet', 'ust', or 'none'"
        )
        stop(message, call. = FALSE)
    }
}

#' Check the hyperparameters
#' @param lambdas x
#' @param nonzeros x
#' @return logical; TRUE or FALSE
.check_parameters <- function(lambdas, nonzeros) {
    # if more then 1 lambda or 1 non-zero, run cross-validations
    if ((length(lambdas) > 1 || length(nonzeros) > 1)) {
        result <- TRUE
    }
    # if only 1 lambda and 1 nonzero, do not run cross-validations
    if (length(lambdas) == 1 && length(nonzeros) == 1) {
        result <- FALSE
    }
    return(result)
}

.check_dimension <- function(x) {
    cat("this is incomplete")
}
