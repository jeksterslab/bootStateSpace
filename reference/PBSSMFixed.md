# Parametric Bootstrap for the State Space Model (Fixed Parameters)

This function simulates data from a state-space model and fits the model
using the `dynr` package. The process is repeated `R` times. It assumes
that the parameters remain constant across individuals and over time. At
the moment, the function only supports `type = 0`.

## Usage

``` r
PBSSMFixed(
  R,
  path,
  prefix,
  n,
  time,
  delta_t = 1,
  mu0,
  sigma0_l,
  alpha,
  beta,
  psi_l,
  nu,
  lambda,
  theta_l,
  type = 0,
  x = NULL,
  gamma = NULL,
  kappa = NULL,
  mu0_fixed = FALSE,
  sigma0_fixed = FALSE,
  alpha_level = 0.05,
  optimization_flag = TRUE,
  hessian_flag = FALSE,
  verbose = FALSE,
  weight_flag = FALSE,
  debug_flag = FALSE,
  perturb_flag = FALSE,
  xtol_rel = 1e-07,
  stopval = -9999,
  ftol_rel = -1,
  ftol_abs = -1,
  maxeval = as.integer(-1),
  maxtime = -1,
  ncores = NULL,
  seed = NULL,
  clean = TRUE
)
```

## Arguments

- R:

  Positive integer. Number of bootstrap samples.

- path:

  Path to a directory to store bootstrap samples and estimates.

- prefix:

  Character string. Prefix used for the file names for the bootstrap
  samples and estimates.

- n:

  Positive integer. Number of individuals.

- time:

  Positive integer. Number of time points.

- delta_t:

  Numeric. Time interval. The default value is `1.0` with an option to
  use a numeric value for the discretized state space model
  parameterization of the linear stochastic differential equation model.

- mu0:

  Numeric vector. Mean of initial latent variable values
  (\\\boldsymbol{\mu}\_{\boldsymbol{\eta} \mid 0}\\).

- sigma0_l:

  Numeric matrix. Cholesky factorization (`t(chol(sigma0))`) of the
  covariance matrix of initial latent variable values
  (\\\boldsymbol{\Sigma}\_{\boldsymbol{\eta} \mid 0}\\).

- alpha:

  Numeric vector. Vector of constant values for the dynamic model
  (\\\boldsymbol{\alpha}\\).

- beta:

  Numeric matrix. Transition matrix relating the values of the latent
  variables at the previous to the current time point
  (\\\boldsymbol{\beta}\\).

- psi_l:

  Numeric matrix. Cholesky factorization (`t(chol(psi))`) of the
  covariance matrix of the process noise (\\\boldsymbol{\Psi}\\).

- nu:

  Numeric vector. Vector of intercept values for the measurement model
  (\\\boldsymbol{\nu}\\).

- lambda:

  Numeric matrix. Factor loading matrix linking the latent variables to
  the observed variables (\\\boldsymbol{\Lambda}\\).

- theta_l:

  Numeric matrix. Cholesky factorization (`t(chol(theta))`) of the
  covariance matrix of the measurement error (\\\boldsymbol{\Theta}\\).

- type:

  Integer. State space model type. See Details for more information.

- x:

  List. Each element of the list is a matrix of covariates for each
  individual `i` in `n`. The number of columns in each matrix should be
  equal to `time`.

- gamma:

  Numeric matrix. Matrix linking the covariates to the latent variables
  at current time point (\\\boldsymbol{\Gamma}\\).

- kappa:

  Numeric matrix. Matrix linking the covariates to the observed
  variables at current time point (\\\boldsymbol{\kappa}\\).

- mu0_fixed:

  Logical. If `mu0_fixed = TRUE`, fix the initial mean vector to `mu0`.
  If `mu0_fixed = FALSE`, `mu0` is estimated.

- sigma0_fixed:

  Logical. If `sigma0_fixed = TRUE`, fix the initial covariance matrix
  to `tcrossprod(sigma0_l)`. If `sigma0_fixed = FALSE`, `sigma0` is
  estimated.

- alpha_level:

  Numeric vector. Significance level \\\alpha\\.

- optimization_flag:

  a flag (TRUE/FALSE) indicating whether optimization is to be done.

- hessian_flag:

  a flag (TRUE/FALSE) indicating whether the Hessian matrix is to be
  calculated.

- verbose:

  a flag (TRUE/FALSE) indicating whether more detailed intermediate
  output during the estimation process should be printed

- weight_flag:

  a flag (TRUE/FALSE) indicating whether the negative log likelihood
  function should be weighted by the length of the time series for each
  individual

- debug_flag:

  a flag (TRUE/FALSE) indicating whether users want additional dynr
  output that can be used for diagnostic purposes

- perturb_flag:

  a flag (TRUE/FLASE) indicating whether to perturb the latent states
  during estimation. Only useful for ensemble forecasting.

- xtol_rel:

  Stopping criteria option for parameter optimization. See
  [`dynr::dynr.model()`](https://rdrr.io/pkg/dynr/man/dynr.model.html)
  for more details.

- stopval:

  Stopping criteria option for parameter optimization. See
  [`dynr::dynr.model()`](https://rdrr.io/pkg/dynr/man/dynr.model.html)
  for more details.

- ftol_rel:

  Stopping criteria option for parameter optimization. See
  [`dynr::dynr.model()`](https://rdrr.io/pkg/dynr/man/dynr.model.html)
  for more details.

- ftol_abs:

  Stopping criteria option for parameter optimization. See
  [`dynr::dynr.model()`](https://rdrr.io/pkg/dynr/man/dynr.model.html)
  for more details.

- maxeval:

  Stopping criteria option for parameter optimization. See
  [`dynr::dynr.model()`](https://rdrr.io/pkg/dynr/man/dynr.model.html)
  for more details.

- maxtime:

  Stopping criteria option for parameter optimization. See
  [`dynr::dynr.model()`](https://rdrr.io/pkg/dynr/man/dynr.model.html)
  for more details.

- ncores:

  Positive integer. Number of cores to use. If `ncores = NULL`, use a
  single core. Consider using multiple cores when number of bootstrap
  samples `R` is a large value.

- seed:

  Random seed.

- clean:

  Logical. If `clean = TRUE`, delete intermediate files generated by the
  function.

## Value

Returns an object of class `bootstatespace` which is a list with the
following elements:

- call:

  Function call.

- args:

  Function arguments.

- thetahatstar:

  Sampling distribution of \\\boldsymbol{\hat{\theta}}\\.

- vcov:

  Sampling variance-covariance matrix of \\\boldsymbol{\hat{\theta}}\\.

- est:

  Vector of estimated \\\boldsymbol{\hat{\theta}}\\.

- fun:

  Function used ("PBSSMFixed").

- method:

  Bootstrap method used ("parametric").

## Details

### Type 0

The measurement model is given by \$\$ \mathbf{y}\_{i, t} =
\boldsymbol{\nu} + \boldsymbol{\Lambda} \boldsymbol{\eta}\_{i, t} +
\boldsymbol{\varepsilon}\_{i, t}, \quad \mathrm{with} \quad
\boldsymbol{\varepsilon}\_{i, t} \sim \mathcal{N} \left( \mathbf{0},
\boldsymbol{\Theta} \right) \$\$ where \\\mathbf{y}\_{i, t}\\,
\\\boldsymbol{\eta}\_{i, t}\\, and \\\boldsymbol{\varepsilon}\_{i, t}\\
are random variables and \\\boldsymbol{\nu}\\, \\\boldsymbol{\Lambda}\\,
and \\\boldsymbol{\Theta}\\ are model parameters. \\\mathbf{y}\_{i, t}\\
represents a vector of observed random variables,
\\\boldsymbol{\eta}\_{i, t}\\ a vector of latent random variables, and
\\\boldsymbol{\varepsilon}\_{i, t}\\ a vector of random measurement
errors, at time \\t\\ and individual \\i\\. \\\boldsymbol{\nu}\\ denotes
a vector of intercepts, \\\boldsymbol{\Lambda}\\ a matrix of factor
loadings, and \\\boldsymbol{\Theta}\\ the covariance matrix of
\\\boldsymbol{\varepsilon}\\.

An alternative representation of the measurement error is given by \$\$
\boldsymbol{\varepsilon}\_{i, t} = \boldsymbol{\Theta}^{\frac{1}{2}}
\mathbf{z}\_{i, t}, \quad \mathrm{with} \quad \mathbf{z}\_{i, t} \sim
\mathcal{N} \left( \mathbf{0}, \mathbf{I} \right) \$\$ where
\\\mathbf{z}\_{i, t}\\ is a vector of independent standard normal random
variables and \\ \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right) \left(
\boldsymbol{\Theta}^{\frac{1}{2}} \right)^{\prime} = \boldsymbol{\Theta}
. \\

The dynamic structure is given by \$\$ \boldsymbol{\eta}\_{i, t} =
\boldsymbol{\alpha} + \boldsymbol{\beta} \boldsymbol{\eta}\_{i, t - 1} +
\boldsymbol{\zeta}\_{i, t}, \quad \mathrm{with} \quad
\boldsymbol{\zeta}\_{i, t} \sim \mathcal{N} \left( \mathbf{0},
\boldsymbol{\Psi} \right) \$\$ where \\\boldsymbol{\eta}\_{i, t}\\,
\\\boldsymbol{\eta}\_{i, t - 1}\\, and \\\boldsymbol{\zeta}\_{i, t}\\
are random variables, and \\\boldsymbol{\alpha}\\,
\\\boldsymbol{\beta}\\, and \\\boldsymbol{\Psi}\\ are model parameters.
Here, \\\boldsymbol{\eta}\_{i, t}\\ is a vector of latent variables at
time \\t\\ and individual \\i\\, \\\boldsymbol{\eta}\_{i, t - 1}\\
represents a vector of latent variables at time \\t - 1\\ and individual
\\i\\, and \\\boldsymbol{\zeta}\_{i, t}\\ represents a vector of dynamic
noise at time \\t\\ and individual \\i\\. \\\boldsymbol{\alpha}\\
denotes a vector of intercepts, \\\boldsymbol{\beta}\\ a matrix of
autoregression and cross regression coefficients, and
\\\boldsymbol{\Psi}\\ the covariance matrix of \\\boldsymbol{\zeta}\_{i,
t}\\.

An alternative representation of the dynamic noise is given by \$\$
\boldsymbol{\zeta}\_{i, t} = \boldsymbol{\Psi}^{\frac{1}{2}}
\mathbf{z}\_{i, t}, \quad \mathrm{with} \quad \mathbf{z}\_{i, t} \sim
\mathcal{N} \left( \mathbf{0}, \mathbf{I} \right) \$\$ where \\ \left(
\boldsymbol{\Psi}^{\frac{1}{2}} \right) \left(
\boldsymbol{\Psi}^{\frac{1}{2}} \right)^{\prime} = \boldsymbol{\Psi} .
\\

### Type 1

The measurement model is given by \$\$ \mathbf{y}\_{i, t} =
\boldsymbol{\nu} + \boldsymbol{\Lambda} \boldsymbol{\eta}\_{i, t} +
\boldsymbol{\varepsilon}\_{i, t}, \quad \mathrm{with} \quad
\boldsymbol{\varepsilon}\_{i, t} \sim \mathcal{N} \left( \mathbf{0},
\boldsymbol{\Theta} \right) . \$\$

The dynamic structure is given by \$\$ \boldsymbol{\eta}\_{i, t} =
\boldsymbol{\alpha} + \boldsymbol{\beta} \boldsymbol{\eta}\_{i, t - 1} +
\boldsymbol{\Gamma} \mathbf{x}\_{i, t} + \boldsymbol{\zeta}\_{i, t},
\quad \mathrm{with} \quad \boldsymbol{\zeta}\_{i, t} \sim \mathcal{N}
\left( \mathbf{0}, \boldsymbol{\Psi} \right) \$\$ where
\\\mathbf{x}\_{i, t}\\ represents a vector of covariates at time \\t\\
and individual \\i\\, and \\\boldsymbol{\Gamma}\\ the coefficient matrix
linking the covariates to the latent variables.

### Type 2

The measurement model is given by \$\$ \mathbf{y}\_{i, t} =
\boldsymbol{\nu} + \boldsymbol{\Lambda} \boldsymbol{\eta}\_{i, t} +
\boldsymbol{\kappa} \mathbf{x}\_{i, t} + \boldsymbol{\varepsilon}\_{i,
t}, \quad \mathrm{with} \quad \boldsymbol{\varepsilon}\_{i, t} \sim
\mathcal{N} \left( \mathbf{0}, \boldsymbol{\Theta} \right) \$\$ where
\\\boldsymbol{\kappa}\\ represents the coefficient matrix linking the
covariates to the observed variables.

The dynamic structure is given by \$\$ \boldsymbol{\eta}\_{i, t} =
\boldsymbol{\alpha} + \boldsymbol{\beta} \boldsymbol{\eta}\_{i, t - 1} +
\boldsymbol{\Gamma} \mathbf{x}\_{i, t} + \boldsymbol{\zeta}\_{i, t},
\quad \mathrm{with} \quad \boldsymbol{\zeta}\_{i, t} \sim \mathcal{N}
\left( \mathbf{0}, \boldsymbol{\Psi} \right) . \$\$

## References

Chow, S.-M., Ho, M. R., Hamaker, E. L., & Dolan, C. V. (2010).
Equivalence and differences between structural equation modeling and
state-space modeling techniques. *Structural Equation Modeling: A
Multidisciplinary Journal*, 17(2), 303–332.
[doi:10.1080/10705511003661553](https://doi.org/10.1080/10705511003661553)

## See also

Other Bootstrap for State Space Models Functions:
[`PBSSMLinSDEFixed()`](https://github.com/jeksterslab/bootStateSpace/reference/PBSSMLinSDEFixed.md),
[`PBSSMOUFixed()`](https://github.com/jeksterslab/bootStateSpace/reference/PBSSMOUFixed.md),
[`PBSSMVARFixed()`](https://github.com/jeksterslab/bootStateSpace/reference/PBSSMVARFixed.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
if (FALSE) { # \dontrun{
# prepare parameters
set.seed(42)
## number of individuals
n <- 5
## time points
time <- 50
delta_t <- 1
## dynamic structure
p <- 3
mu0 <- rep(x = 0, times = p)
sigma0 <- 0.001 * diag(p)
sigma0_l <- t(chol(sigma0))
alpha <- rep(x = 0, times = p)
beta <- 0.50 * diag(p)
psi <- 0.001 * diag(p)
psi_l <- t(chol(psi))
## measurement model
k <- 3
nu <- rep(x = 0, times = k)
lambda <- diag(k)
theta <- 0.001 * diag(k)
theta_l <- t(chol(theta))

path <- tempdir()

pb <- PBSSMFixed(
  R = 10L, # use at least 1000 in actual research
  path = path,
  prefix = "ssm",
  n = n,
  time = time,
  delta_t = delta_t,
  mu0 = mu0,
  sigma0_l = sigma0_l,
  alpha = alpha,
  beta = beta,
  psi_l = psi_l,
  nu = nu,
  lambda = lambda,
  theta_l = theta_l,
  type = 0,
  ncores = 1, # consider using multiple cores
  seed = 42
)
print(pb)
summary(pb)
confint(pb)
vcov(pb)
coef(pb)
print(pb, type = "bc") # bias-corrected
summary(pb, type = "bc")
confint(pb, type = "bc")
} # }
```
