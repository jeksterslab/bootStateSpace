# Confidence Intervals Method for an Object of Class `bootstatespace`

Confidence Intervals Method for an Object of Class `bootstatespace`

## Usage

``` r
# S3 method for class 'bootstatespace'
confint(object, parm = NULL, level = 0.95, type = "pc", ...)
```

## Arguments

- object:

  Object of Class `bootstatespace`.

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- type:

  Charater string. Confidence interval type, that is, `type = "pc"` for
  percentile; `type = "bc"` for bias corrected.

- ...:

  additional arguments.

## Value

Returns a matrix of confidence intervals.

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
confint(pb)
confint(pb, type = "bc")
} # }
```
