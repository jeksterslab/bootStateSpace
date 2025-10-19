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
#' @examples
#' \dontrun{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' print(pb)
#' print(pb, type = "bc")
#' }
#'
#' @keywords methods
#' @export
print.bootstatespace <- function(x,
                                 alpha = NULL,
                                 type = "pc",
                                 digits = 4,
                                 ...) {
  print.summary.bootstatespace(
    summary.bootstatespace(
      object = x,
      alpha = alpha,
      type = type,
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
#' @examples
#' \dontrun{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' summary(pb)
#' summary(pb, type = "bc")
#' }
#'
#' @keywords methods
#' @export
summary.bootstatespace <- function(object,
                                   alpha = NULL,
                                   type = "pc",
                                   digits = 4,
                                   ...) {
  ci <- .PBCI(
    object = object,
    alpha = alpha,
    type = type
  )
  print_summary <- round(
    x = ci,
    digits = digits
  )
  attr(
    x = ci,
    which = "fit"
  ) <- object
  attr(
    x = ci,
    which = "print_summary"
  ) <- print_summary
  attr(
    x = ci,
    which = "alpha"
  ) <- alpha
  attr(
    x = ci,
    which = "type"
  ) <- type
  attr(
    x = ci,
    which = "digits"
  ) <- digits
  class(ci) <- "summary.bootstatespace"
  ci
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.bootstatespace
print.summary.bootstatespace <- function(x,
                                         ...) {
  print_summary <- attr(
    x = x,
    which = "print_summary"
  )
  object <- attr(
    x = x,
    which = "fit"
  )
  type <- attr(
    x = x,
    which = "type"
  )
  cat("Call:\n")
  base::print(object$call)
  if (object$method == "parametric") {
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
  print(print_summary)
  invisible(x)
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
#' @examples
#' \dontrun{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' vcov(pb)
#' }
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
#' @examples
#' \dontrun{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' coef(pb)
#' }
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
#' @examples
#' \dontrun{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' confint(pb)
#' confint(pb, type = "bc")
#' }
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
#' @examples
#' \dontrun{
#' # prepare parameters
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 50
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' path <- tempdir()
#'
#' pb <- PBSSMVARFixed(
#'   R = 10L, # use at least 1000 in actual research
#'   path = path,
#'   prefix = "var",
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0,
#'   ncores = 1, # consider using multiple cores
#'   seed = 42
#' )
#' extract(pb, what = "beta")
#' }
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
