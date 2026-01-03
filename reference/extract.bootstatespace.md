# Extract Method for an Object of Class `bootstatespace`

Extract Method for an Object of Class `bootstatespace`

## Usage

``` r
# S3 method for class 'bootstatespace'
extract(object, what = NULL)
```

## Arguments

- object:

  Object of Class `bootstatespace`.

- what:

  Character string. What specific matrix to extract. If `what = NULL`,
  extract all available matrices.

## Value

Returns a list. Each element of the list is a list of bootstrap
estimates in matrix format.

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
extract(pb, what = "beta")
} # }
```
