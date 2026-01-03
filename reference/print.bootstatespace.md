# Print Method for an Object of Class `bootstatespace`

Print Method for an Object of Class `bootstatespace`

## Usage

``` r
# S3 method for class 'bootstatespace'
print(x, alpha = NULL, type = "pc", digits = 4, ...)
```

## Arguments

- x:

  Object of Class `bootstatespace`.

- alpha:

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `x`.

- type:

  Charater string. Confidence interval type, that is, `type = "pc"` for
  percentile; `type = "bc"` for bias corrected.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Prints a matrix of estimates, standard errors, number of bootstrap
replications, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
if (FALSE) { # \dontrun{
# prepare parameters
## number of individuals
n <- 5
## time points
time <- 50
## dynamic structure
p <- 3
mu0 <- rep(x = 0, times = p)
sigma0 <- 0.001 * diag(p)
sigma0_l <- t(chol(sigma0))
alpha <- rep(x = 0, times = p)
beta <- 0.50 * diag(p)
psi <- 0.001 * diag(p)
psi_l <- t(chol(psi))

path <- tempdir()

pb <- PBSSMVARFixed(
  R = 10L, # use at least 1000 in actual research
  path = path,
  prefix = "var",
  n = n,
  time = time,
  mu0 = mu0,
  sigma0_l = sigma0_l,
  alpha = alpha,
  beta = beta,
  psi_l = psi_l,
  type = 0,
  ncores = 1, # consider using multiple cores
  seed = 42
)
print(pb)
print(pb, type = "bc")
} # }
```
