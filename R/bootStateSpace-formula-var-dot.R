.FormulaVAR <- function(p,
                        intercept) {
  formula <- lapply(
    X = seq_len(p),
    FUN = function(i) {
      terms <- paste0(
        "(",
        "beta_",
        i,
        "_",
        seq_len(p),
        " * eta_",
        seq_len(p),
        ")",
        collapse = " + "
      )
      paste0(
        "eta_",
        i,
        " ~ ",
        terms
      )
    }
  )
  if (intercept) {
    formula <- lapply(
      X = seq_len(length(formula)),
      FUN = function(i) {
        paste0(
          formula[[i]],
          " + ",
          "alpha_",
          i,
          "_1"
        )
      }
    )
  }
  formula
}
