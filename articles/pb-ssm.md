# Parametric Bootstrap (The State Space Model)

## Model

The measurement model is given by
``` math
\begin{equation}
  \mathbf{y}_{i, t}
  =
  \boldsymbol{\nu}
  +
  \boldsymbol{\Lambda}
  \boldsymbol{\eta}_{i, t}
  +
  \boldsymbol{\varepsilon}_{i, t},
  \quad
  \mathrm{with}
  \quad
  \boldsymbol{\varepsilon}_{i, t}
  \sim
  \mathcal{N}
  \left(
  \mathbf{0},
  \boldsymbol{\Theta}
  \right)
\end{equation}
```
where $`\mathbf{y}_{i, t}`$, $`\boldsymbol{\eta}_{i, t}`$, and
$`\boldsymbol{\varepsilon}_{i, t}`$ are random variables and
$`\boldsymbol{\nu}`$, $`\boldsymbol{\Lambda}`$, and
$`\boldsymbol{\Theta}`$ are model parameters. $`\mathbf{y}_{i, t}`$
represents a vector of observed random variables,
$`\boldsymbol{\eta}_{i, t}`$ a vector of latent random variables, and
$`\boldsymbol{\varepsilon}_{i, t}`$ a vector of random measurement
errors, at time $`t`$ and individual $`i`$. $`\boldsymbol{\nu}`$ denotes
a vector of intercepts, $`\boldsymbol{\Lambda}`$ a matrix of factor
loadings, and $`\boldsymbol{\Theta}`$ the covariance matrix of
$`\boldsymbol{\varepsilon}`$.

An alternative representation of the measurement error is given by
``` math
\begin{equation}
  \boldsymbol{\varepsilon}_{i, t}
  =
  \boldsymbol{\Theta}^{\frac{1}{2}}
  \mathbf{z}_{i, t},
  \quad
  \mathrm{with}
  \quad
  \mathbf{z}_{i, t}
  \sim
  \mathcal{N}
  \left(
  \mathbf{0},
  \mathbf{I}
  \right)
\end{equation}
```
where $`\mathbf{z}_{i, t}`$ is a vector of independent standard normal
random variables and
$`\left( \boldsymbol{\Theta}^{\frac{1}{2}} \right) \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)^{\prime} = \boldsymbol{\Theta}`$
.

The dynamic structure is given by
``` math
\begin{equation}
  \boldsymbol{\eta}_{i, t}
  =
  \boldsymbol{\alpha}
  +
  \boldsymbol{\beta}
  \boldsymbol{\eta}_{i, t - 1}
  +
  \boldsymbol{\zeta}_{i, t},
  \quad
  \mathrm{with}
  \quad
  \boldsymbol{\zeta}_{i, t}
  \sim
  \mathcal{N}
  \left(
  \mathbf{0},
  \boldsymbol{\Psi}
  \right)
\end{equation}
```
where $`\boldsymbol{\eta}_{i, t}`$, $`\boldsymbol{\eta}_{i, t - 1}`$,
and $`\boldsymbol{\zeta}_{i, t}`$ are random variables, and
$`\boldsymbol{\alpha}`$, $`\boldsymbol{\beta}`$, and
$`\boldsymbol{\Psi}`$ are model parameters. Here,
$`\boldsymbol{\eta}_{i, t}`$ is a vector of latent variables at time
$`t`$ and individual $`i`$, $`\boldsymbol{\eta}_{i, t - 1}`$ represents
a vector of latent variables at time $`t - 1`$ and individual $`i`$, and
$`\boldsymbol{\zeta}_{i, t}`$ represents a vector of dynamic noise at
time $`t`$ and individual $`i`$. $`\boldsymbol{\alpha}`$ denotes a
vector of intercepts, $`\boldsymbol{\beta}`$ a matrix of autoregression
and cross regression coefficients, and $`\boldsymbol{\Psi}`$ the
covariance matrix of $`\boldsymbol{\zeta}_{i, t}`$.

An alternative representation of the dynamic noise is given by
``` math
\begin{equation}
  \boldsymbol{\zeta}_{i, t}
  =
  \boldsymbol{\Psi}^{\frac{1}{2}}
  \mathbf{z}_{i, t},
  \quad
  \mathrm{with}
  \quad
  \mathbf{z}_{i, t}
  \sim
  \mathcal{N}
  \left(
  \mathbf{0},
  \mathbf{I}
  \right)
\end{equation}
```
where
$`\left( \boldsymbol{\Psi}^{\frac{1}{2}} \right) \left( \boldsymbol{\Psi}^{\frac{1}{2}} \right)^{\prime} = \boldsymbol{\Psi}`$
.

## Parameters

### Notation

Let $`t = 100`$ be the number of time points and $`n = 5`$ be the number
of individuals.

Let the measurement model intecept vector $`\boldsymbol{\nu}`$ be given
by

``` math
\begin{equation}
\boldsymbol{\nu}
=
\left(
\begin{array}{c}
  0 \\
  0 \\
  0 \\
\end{array}
\right) .
\end{equation}
```

Let the factor loadings matrix $`\boldsymbol{\Lambda}`$ be given by

``` math
\begin{equation}
\boldsymbol{\Lambda}
=
\left(
\begin{array}{ccc}
  1 & 0 & 0 \\
  0 & 1 & 0 \\
  0 & 0 & 1 \\
\end{array}
\right) .
\end{equation}
```

Let the measurement error covariance matrix $`\boldsymbol{\Theta}`$ be
given by

``` math
\begin{equation}
\boldsymbol{\Theta}
=
\left(
\begin{array}{ccc}
  0.2 & 0 & 0 \\
  0 & 0.2 & 0 \\
  0 & 0 & 0.2 \\
\end{array}
\right) .
\end{equation}
```

Let the initial condition $`\boldsymbol{\eta}_{0}`$ be given by

``` math
\begin{equation}
\boldsymbol{\eta}_{0} \sim \mathcal{N} \left( \boldsymbol{\mu}_{\boldsymbol{\eta} \mid 0}, \boldsymbol{\Sigma}_{\boldsymbol{\eta} \mid 0} \right)
\end{equation}
```

``` math
\begin{equation}
\boldsymbol{\mu}_{\boldsymbol{\eta} \mid 0}
=
\left(
\begin{array}{c}
  0 \\
  0 \\
  0 \\
\end{array}
\right)
\end{equation}
```

``` math
\begin{equation}
\boldsymbol{\Sigma}_{\boldsymbol{\eta} \mid 0}
=
\left(
\begin{array}{ccc}
  1 & 0.2 & 0.2 \\
  0.2 & 1 & 0.2 \\
  0.2 & 0.2 & 1 \\
\end{array}
\right) .
\end{equation}
```

Let the constant vector $`\boldsymbol{\alpha}`$ be given by

``` math
\begin{equation}
\boldsymbol{\alpha}
=
\left(
\begin{array}{c}
  0 \\
  0 \\
  0 \\
\end{array}
\right) .
\end{equation}
```

Let the transition matrix $`\boldsymbol{\beta}`$ be given by

``` math
\begin{equation}
\boldsymbol{\beta}
=
\left(
\begin{array}{ccc}
  0.7 & 0 & 0 \\
  0.5 & 0.6 & 0 \\
  -0.1 & 0.4 & 0.5 \\
\end{array}
\right) .
\end{equation}
```

Let the dynamic process noise $`\boldsymbol{\Psi}`$ be given by

``` math
\begin{equation}
\boldsymbol{\Psi}
=
\left(
\begin{array}{ccc}
  0.1 & 0 & 0 \\
  0 & 0.1 & 0 \\
  0 & 0 & 0.1 \\
\end{array}
\right) .
\end{equation}
```

### R Function Arguments

``` r

n
#> [1] 5
time
#> [1] 100
mu0
#> [1] 0 0 0
sigma0
#>      [,1] [,2] [,3]
#> [1,]  1.0  0.2  0.2
#> [2,]  0.2  1.0  0.2
#> [3,]  0.2  0.2  1.0
sigma0_l # sigma0_l <- t(chol(sigma0))
#>      [,1]      [,2]      [,3]
#> [1,]  1.0 0.0000000 0.0000000
#> [2,]  0.2 0.9797959 0.0000000
#> [3,]  0.2 0.1632993 0.9660918
alpha
#> [1] 0 0 0
beta
#>      [,1] [,2] [,3]
#> [1,]  0.7  0.0  0.0
#> [2,]  0.5  0.6  0.0
#> [3,] -0.1  0.4  0.5
psi
#>      [,1] [,2] [,3]
#> [1,]  0.1  0.0  0.0
#> [2,]  0.0  0.1  0.0
#> [3,]  0.0  0.0  0.1
psi_l # psi_l <- t(chol(psi))
#>           [,1]      [,2]      [,3]
#> [1,] 0.3162278 0.0000000 0.0000000
#> [2,] 0.0000000 0.3162278 0.0000000
#> [3,] 0.0000000 0.0000000 0.3162278
nu
#> [1] 0 0 0
lambda
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    1
theta
#>      [,1] [,2] [,3]
#> [1,]  0.2  0.0  0.0
#> [2,]  0.0  0.2  0.0
#> [3,]  0.0  0.0  0.2
theta_l # theta_l <- t(chol(theta))
#>           [,1]      [,2]      [,3]
#> [1,] 0.4472136 0.0000000 0.0000000
#> [2,] 0.0000000 0.4472136 0.0000000
#> [3,] 0.0000000 0.0000000 0.4472136
```

### Parametric Bootstrap

``` r

R <- 5L # use at least 1000 in actual research
path <- getwd()
prefix <- "ssm"
```

We use the `PBSSMFixed` function from the `bootStateSpace` package to
perform parametric bootstraping using the parameters described above.
The argument `R` specifies the number of bootstrap replications. The
generated data and model estimates are stored in `path` using the
specified `prefix` for the file names. The
`ncores = parallel::detectCores()` argument instructs the function to
use all available CPU cores in the system.

> ***NOTE:*** Fitting the state space model multiple times is
> computationally intensive.

``` r

library(bootStateSpace)
pb <- PBSSMFixed(
  R = R,
  path = path,
  prefix = prefix,
  n = n,
  time = time,
  mu0 = mu0,
  sigma0_l = sigma0_l,
  alpha = alpha,
  beta = beta,
  psi_l = psi_l,
  nu = nu,
  lambda = lambda,
  theta_l = theta_l,
  ncores = parallel::detectCores(),
  seed = 42
)
summary(pb)
#> Call:
#> PBSSMFixed(R = R, path = path, prefix = prefix, n = n, time = time, 
#>     mu0 = mu0, sigma0_l = sigma0_l, alpha = alpha, beta = beta, 
#>     psi_l = psi_l, nu = nu, lambda = lambda, theta_l = theta_l, 
#>     ncores = parallel::detectCores(), seed = 42)
#> 
#> Parametric bootstrap confidence intervals.
#> type = "pc"
#>             est     se R    2.5%   97.5%
#> beta_1_1    0.7 0.1214 5  0.6071  0.8779
#> beta_2_1    0.5 0.1142 5  0.3707  0.6521
#> beta_3_1   -0.1 0.0496 5 -0.1359 -0.0182
#> beta_1_2    0.0 0.0581 5 -0.0720  0.0561
#> beta_2_2    0.6 0.1332 5  0.4154  0.7346
#> beta_3_2    0.4 0.0786 5  0.2833  0.4701
#> beta_1_3    0.0 0.0553 5 -0.0442  0.0782
#> beta_2_3    0.0 0.0647 5 -0.0938  0.0409
#> beta_3_3    0.5 0.1045 5  0.3640  0.6138
#> psi_1_1     0.1 0.0412 5  0.0514  0.1495
#> psi_2_2     0.1 0.0506 5  0.0563  0.1652
#> psi_3_3     0.1 0.0604 5  0.0709  0.2102
#> theta_1_1   0.2 0.0350 5  0.1370  0.2242
#> theta_2_2   0.2 0.0506 5  0.1307  0.2311
#> theta_3_3   0.2 0.0509 5  0.1178  0.2443
#> mu0_1_1     0.0 0.3588 5 -0.5086  0.3155
#> mu0_2_1     0.0 0.2375 5 -0.3283  0.2207
#> mu0_3_1     0.0 0.2996 5 -0.4104  0.2763
#> sigma0_1_1  1.0 0.5645 5  0.2801  1.5268
#> sigma0_2_1  0.2 0.4023 5 -0.5564  0.4601
#> sigma0_3_1  0.2 0.4738 5 -0.4340  0.7084
#> sigma0_2_2  1.0 0.9294 5  0.0606  2.2416
#> sigma0_3_2  0.2 0.6169 5 -1.0613  0.3582
#> sigma0_3_3  1.0 0.3446 5  0.2331  1.1122
summary(pb, type = "bc")
#> Call:
#> PBSSMFixed(R = R, path = path, prefix = prefix, n = n, time = time, 
#>     mu0 = mu0, sigma0_l = sigma0_l, alpha = alpha, beta = beta, 
#>     psi_l = psi_l, nu = nu, lambda = lambda, theta_l = theta_l, 
#>     ncores = parallel::detectCores(), seed = 42)
#> 
#> Parametric bootstrap confidence intervals.
#> type = "bc"
#>             est     se R    2.5%   97.5%
#> beta_1_1    0.7 0.1214 5  0.6363  0.9010
#> beta_2_1    0.5 0.1142 5  0.4585  0.6694
#> beta_3_1   -0.1 0.0496 5 -0.1429 -0.0526
#> beta_1_2    0.0 0.0581 5 -0.0721  0.0482
#> beta_2_2    0.6 0.1332 5  0.4033  0.7247
#> beta_3_2    0.4 0.0786 5  0.2751  0.4660
#> beta_1_3    0.0 0.0553 5 -0.0456  0.0751
#> beta_2_3    0.0 0.0647 5 -0.0934  0.0430
#> beta_3_3    0.5 0.1045 5  0.3604  0.5965
#> psi_1_1     0.1 0.0412 5  0.0478  0.1456
#> psi_2_2     0.1 0.0506 5  0.0568  0.1681
#> psi_3_3     0.1 0.0604 5  0.0707  0.1964
#> theta_1_1   0.2 0.0350 5  0.1655  0.2279
#> theta_2_2   0.2 0.0506 5  0.1293  0.2308
#> theta_3_3   0.2 0.0509 5  0.1288  0.2467
#> mu0_1_1     0.0 0.3588 5 -0.3981  0.3667
#> mu0_2_1     0.0 0.2375 5 -0.2678  0.2216
#> mu0_3_1     0.0 0.2996 5 -0.4628  0.1744
#> sigma0_1_1  1.0 0.5645 5  0.2865  1.5500
#> sigma0_2_1  0.2 0.4023 5 -0.1177  0.5107
#> sigma0_3_1  0.2 0.4738 5 -0.1438  0.7953
#> sigma0_2_2  1.0 0.9294 5  0.4849  2.4096
#> sigma0_3_2  0.2 0.6169 5 -0.8642  0.3681
#> sigma0_3_3  1.0 0.3446 5  0.5994  1.1597
```

## References

Ou, L., Hunter, M. D., & Chow, S.-M. (2019). What’s for dynr: A package
for linear and nonlinear dynamic modeling in R. *The R Journal*,
*11*(1), 91. <https://doi.org/10.32614/rj-2019-012>

Pesigan, I. J. A., Russell, M. A., & Chow, S.-M. (2025). Inferences and
effect sizes for direct, indirect, and total effects in continuous-time
mediation models. *Psychological Methods*.
<https://doi.org/10.1037/met0000779>

R Core Team. (2024). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>
