#' Print Method for an Object of Class
#' `bootstatespace`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Prints a matrix of
#'   estimates,
#'   standard errors,
#'   number of bootstrap replications,
#'   and
#'   confidence intervals.
#'
#' @param x Object of Class `bootstatespace`.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `x`.
#' @inheritParams summary.bootstatespace
#'
#' @keywords methods
#' @export
print.bootstatespace <- function(x,
                                 alpha = NULL,
                                 type = "pc",
                                 digits = 4,
                                 ...) {
  cat("Call:\n")
  base::print(x$call)
  if (x$method == "parametric") {
    cat(
      paste0(
        "\n",
        "Parametric bootstrap confidence intervals.",
        "\n",
        "type = ",
        "\"",
        type,
        "\"",
        "\n"
      )
    )
  }
  base::print(
    round(
      .PBCI(
        object = x,
        alpha = alpha,
        type = type
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class
#' `bootstatespace`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   estimates,
#'   standard errors,
#'   number of bootstrap replications,
#'   and
#'   confidence intervals.
#'
#' @param object Object of Class `bootstatespace`.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `object`.
#' @param type Charater string.
#'   Confidence interval type, that is,
#'   `type = "pc"` for percentile;
#'   `type = "bc"` for bias corrected.
#' @param digits Digits to print.
#'
#' @keywords methods
#' @export
summary.bootstatespace <- function(object,
                                   alpha = NULL,
                                   type = "pc",
                                   digits = 4,
                                   ...) {
  cat("Call:\n")
  base::print(object$call)
  if (object$method == "parametric") {
    if (interactive()) {
      # nocov start
      cat(
        paste0(
          "\n",
          "Parametric bootstrap confidence intervals.",
          "\n",
          "type = ",
          "\"",
          type,
          "\"",
          "\n"
        )
      )
      # nocov end
    }
  }
  round(
    .PBCI(
      object = object,
      alpha = alpha,
      type = type
    ),
    digits = digits
  )
}

#' Sampling Variance-Covariance Matrix Method for an Object of Class
#' `bootstatespace`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns the variance-covariance matrix of estimates.
#'
#' @inheritParams summary.bootstatespace
#'
#' @keywords methods
#' @export
vcov.bootstatespace <- function(object,
                                ...) {
  object$vcov
}

#' Estimated Parameter Method for an Object of Class
#' `bootstatespace`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of estimated parameters.
#'
#' @inheritParams summary.bootstatespace
#'
#' @keywords methods
#' @export
coef.bootstatespace <- function(object,
                                ...) {
  object$est
}

#' Confidence Intervals Method for an Object of Class
#' `bootstatespace`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @inheritParams summary.bootstatespace
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @keywords methods
#' @export
confint.bootstatespace <- function(object,
                                   parm = NULL,
                                   level = 0.95,
                                   type = "pc",
                                   ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(
        object$est
      )
    )
  }
  ci <- .PBCI(
    object = object,
    alpha = 1 - level[1],
    type = type
  )[parm, 4:5, drop = FALSE]
  varnames <- colnames(ci)
  varnames <- gsub(
    pattern = "%",
    replacement = " %",
    x = varnames
  )
  colnames(ci) <- varnames
  ci
}

#' Extract Generic Function
#'
#' A generic function for extracting elements from objects.
#'
#' @param object An object.
#' @param what Character string.
#' @return A value determined by the specific method for the object's class.
#' @keywords methods
#' @export
extract <- function(object,
                    what) {
  UseMethod("extract")
}

#' Extract Method for an Object of Class
#' `bootstatespace`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a list.
#'   Each element of the list
#'   is a list of bootstrap estimates
#'   in matrix format.
#'
#' @param object Object of Class `bootstatespace`.
#' @param what Character string.
#'   What specific matrix to extract.
#'   If `what = NULL`,
#'   extract all available matrices.
#'
#' @keywords methods
#' @export
#' @method extract bootstatespace
extract.bootstatespace <- function(object,
                                   what = NULL) {
  output <- lapply(
    X = object$thetahatstar,
    FUN = function(i) {
      .Vec2Mat(x = i)
    }
  )
  if (is.null(what)) {
    return(
      output
    )
  } else {
    return(
      lapply(
        X = output,
        FUN = function(i) {
          i[[what]]
        }
      )
    )
  }
}
