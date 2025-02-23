% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootStateSpace-pb-ssm-fixed.R
\name{PBSSMFixed}
\alias{PBSSMFixed}
\title{Parametric Bootstrap for the
State Space Model
(Fixed Parameters)}
\usage{
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
}
\arguments{
\item{R}{Positive integer.
Number of bootstrap samples.}

\item{path}{Path to a directory
to store bootstrap samples and estimates.}

\item{prefix}{Character string.
Prefix used for the file names
for the bootstrap samples and estimates.}

\item{n}{Positive integer.
Number of individuals.}

\item{time}{Positive integer.
Number of time points.}

\item{delta_t}{Numeric.
Time interval.
The default value is \code{1.0}
with an option to use a numeric value
for the discretized state space model
parameterization of the
linear stochastic differential equation model.}

\item{mu0}{Numeric vector.
Mean of initial latent variable values
(\eqn{\boldsymbol{\mu}_{\boldsymbol{\eta} \mid 0}}).}

\item{sigma0_l}{Numeric matrix.
Cholesky factorization (\code{t(chol(sigma0))})
of the covariance matrix
of initial latent variable values
(\eqn{\boldsymbol{\Sigma}_{\boldsymbol{\eta} \mid 0}}).}

\item{alpha}{Numeric vector.
Vector of constant values for the dynamic model
(\eqn{\boldsymbol{\alpha}}).}

\item{beta}{Numeric matrix.
Transition matrix relating the values of the latent variables
at the previous to the current time point
(\eqn{\boldsymbol{\beta}}).}

\item{psi_l}{Numeric matrix.
Cholesky factorization (\code{t(chol(psi))})
of the covariance matrix
of the process noise
(\eqn{\boldsymbol{\Psi}}).}

\item{nu}{Numeric vector.
Vector of intercept values for the measurement model
(\eqn{\boldsymbol{\nu}}).}

\item{lambda}{Numeric matrix.
Factor loading matrix linking the latent variables
to the observed variables
(\eqn{\boldsymbol{\Lambda}}).}

\item{theta_l}{Numeric matrix.
Cholesky factorization (\code{t(chol(theta))})
of the covariance matrix
of the measurement error
(\eqn{\boldsymbol{\Theta}}).}

\item{type}{Integer.
State space model type.
See Details for more information.}

\item{x}{List.
Each element of the list is a matrix of covariates
for each individual \code{i} in \code{n}.
The number of columns in each matrix
should be equal to \code{time}.}

\item{gamma}{Numeric matrix.
Matrix linking the covariates to the latent variables
at current time point
(\eqn{\boldsymbol{\Gamma}}).}

\item{kappa}{Numeric matrix.
Matrix linking the covariates to the observed variables
at current time point
(\eqn{\boldsymbol{\kappa}}).}

\item{mu0_fixed}{Logical.
If \code{mu0_fixed = TRUE},
fix the initial mean vector
to \code{mu0}.
If \code{mu0_fixed = FALSE},
\code{mu0} is estimated.}

\item{sigma0_fixed}{Logical.
If \code{sigma0_fixed = TRUE},
fix the initial covariance matrix
to \code{tcrossprod(sigma0_l)}.
If \code{sigma0_fixed = FALSE},
\code{sigma0} is estimated.}

\item{alpha_level}{Numeric vector.
Significance level \eqn{\alpha}.}

\item{optimization_flag}{a flag (TRUE/FALSE) indicating whether optimization is to be done.}

\item{hessian_flag}{a flag (TRUE/FALSE) indicating whether the Hessian matrix is to be calculated.}

\item{verbose}{a flag (TRUE/FALSE) indicating whether more detailed intermediate output during the 
estimation process should be printed}

\item{weight_flag}{a flag (TRUE/FALSE) indicating whether the negative log likelihood function should 
be weighted by the length of the time series for each individual}

\item{debug_flag}{a flag (TRUE/FALSE) indicating whether users want additional dynr output that can 
be used for diagnostic purposes}

\item{perturb_flag}{a flag (TRUE/FLASE) indicating whether to perturb the latent states during estimation. Only useful for ensemble forecasting.}

\item{xtol_rel}{Stopping criteria option
for parameter optimization.
See \code{\link[dynr:dynr.model]{dynr::dynr.model()}} for more details.}

\item{stopval}{Stopping criteria option
for parameter optimization.
See \code{\link[dynr:dynr.model]{dynr::dynr.model()}} for more details.}

\item{ftol_rel}{Stopping criteria option
for parameter optimization.
See \code{\link[dynr:dynr.model]{dynr::dynr.model()}} for more details.}

\item{ftol_abs}{Stopping criteria option
for parameter optimization.
See \code{\link[dynr:dynr.model]{dynr::dynr.model()}} for more details.}

\item{maxeval}{Stopping criteria option
for parameter optimization.
See \code{\link[dynr:dynr.model]{dynr::dynr.model()}} for more details.}

\item{maxtime}{Stopping criteria option
for parameter optimization.
See \code{\link[dynr:dynr.model]{dynr::dynr.model()}} for more details.}

\item{ncores}{Positive integer.
Number of cores to use.
If \code{ncores = NULL},
use a single core.
Consider using multiple cores
when number of bootstrap samples \code{R}
is a large value.}

\item{seed}{Random seed.}

\item{clean}{Logical.
If \code{clean = TRUE},
delete intermediate files generated by the function.}
}
\value{
Returns an object
of class \code{bootstatespace} which is a list with the following elements:
\describe{
\item{call}{Function call.}
\item{args}{Function arguments.}
\item{thetahatstar}{Sampling distribution of
\eqn{\boldsymbol{\hat{\theta}}}.}
\item{vcov}{Sampling variance-covariance matrix of
\eqn{\boldsymbol{\hat{\theta}}}.}
\item{est}{Vector of estimated
\eqn{\boldsymbol{\hat{\theta}}}.}
\item{fun}{Function used ("PBSSMFixed").}
\item{method}{Bootstrap method used ("parametric").}
}
}
\description{
This function simulates data from
a state-space model
and fits the model using the \code{dynr} package.
The process is repeated \code{R} times.
It assumes that the parameters remain constant
across individuals and over time.
At the moment, the function only supports
\code{type = 0}.
}
\details{
\subsection{Type 0}{

The measurement model is given by
\deqn{
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
  }
where
\eqn{\mathbf{y}_{i, t}},
\eqn{\boldsymbol{\eta}_{i, t}},
and
\eqn{\boldsymbol{\varepsilon}_{i, t}}
are random variables
and
\eqn{\boldsymbol{\nu}},
\eqn{\boldsymbol{\Lambda}},
and
\eqn{\boldsymbol{\Theta}}
are model parameters.
\eqn{\mathbf{y}_{i, t}}
represents a vector of observed random variables,
\eqn{\boldsymbol{\eta}_{i, t}}
a vector of latent random variables,
and
\eqn{\boldsymbol{\varepsilon}_{i, t}}
a vector of random measurement errors,
at time \eqn{t} and individual \eqn{i}.
\eqn{\boldsymbol{\nu}}
denotes a vector of intercepts,
\eqn{\boldsymbol{\Lambda}}
a matrix of factor loadings,
and
\eqn{\boldsymbol{\Theta}}
the covariance matrix of
\eqn{\boldsymbol{\varepsilon}}.

An alternative representation of the measurement error
is given by
\deqn{
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
  }
where
\eqn{\mathbf{z}_{i, t}} is a vector of
independent standard normal random variables and
\eqn{
    \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)
    \left( \boldsymbol{\Theta}^{\frac{1}{2}} \right)^{\prime}
    =
    \boldsymbol{\Theta} .
  }

The dynamic structure is given by
\deqn{
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
  }
where
\eqn{\boldsymbol{\eta}_{i, t}},
\eqn{\boldsymbol{\eta}_{i, t - 1}},
and
\eqn{\boldsymbol{\zeta}_{i, t}}
are random variables,
and
\eqn{\boldsymbol{\alpha}},
\eqn{\boldsymbol{\beta}},
and
\eqn{\boldsymbol{\Psi}}
are model parameters.
Here,
\eqn{\boldsymbol{\eta}_{i, t}}
is a vector of latent variables
at time \eqn{t} and individual \eqn{i},
\eqn{\boldsymbol{\eta}_{i, t - 1}}
represents a vector of latent variables
at time \eqn{t - 1} and individual \eqn{i},
and
\eqn{\boldsymbol{\zeta}_{i, t}}
represents a vector of dynamic noise
at time \eqn{t} and individual \eqn{i}.
\eqn{\boldsymbol{\alpha}}
denotes a vector of intercepts,
\eqn{\boldsymbol{\beta}}
a matrix of autoregression
and cross regression coefficients,
and
\eqn{\boldsymbol{\Psi}}
the covariance matrix of
\eqn{\boldsymbol{\zeta}_{i, t}}.

An alternative representation of the dynamic noise
is given by
\deqn{
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
  }
where
\eqn{
    \left( \boldsymbol{\Psi}^{\frac{1}{2}} \right)
    \left( \boldsymbol{\Psi}^{\frac{1}{2}} \right)^{\prime}
    =
    \boldsymbol{\Psi} .
  }
}

\subsection{Type 1}{

The measurement model is given by
\deqn{
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
    \right) .
  }

The dynamic structure is given by
\deqn{
    \boldsymbol{\eta}_{i, t}
    =
    \boldsymbol{\alpha}
    +
    \boldsymbol{\beta}
    \boldsymbol{\eta}_{i, t - 1}
    +
    \boldsymbol{\Gamma}
    \mathbf{x}_{i, t}
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
  }
where
\eqn{\mathbf{x}_{i, t}} represents a vector of covariates
at time \eqn{t} and individual \eqn{i},
and \eqn{\boldsymbol{\Gamma}} the coefficient matrix
linking the covariates to the latent variables.
}

\subsection{Type 2}{

The measurement model is given by
\deqn{
    \mathbf{y}_{i, t}
    =
    \boldsymbol{\nu}
    +
    \boldsymbol{\Lambda}
    \boldsymbol{\eta}_{i, t}
    +
    \boldsymbol{\kappa}
    \mathbf{x}_{i, t}
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
  }
where
\eqn{\boldsymbol{\kappa}} represents the coefficient matrix
linking the covariates to the observed variables.

The dynamic structure is given by
\deqn{
    \boldsymbol{\eta}_{i, t}
    =
    \boldsymbol{\alpha}
    +
    \boldsymbol{\beta}
    \boldsymbol{\eta}_{i, t - 1}
    +
    \boldsymbol{\Gamma}
    \mathbf{x}_{i, t}
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
    \right) .
  }
}
}
\examples{
\donttest{
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
}

}
\references{
Chow, S.-M., Ho, M. R., Hamaker, E. L., & Dolan, C. V. (2010).
Equivalence and differences between structural equation modeling
and state-space modeling techniques.
\emph{Structural Equation Modeling: A Multidisciplinary Journal},
17(2), 303–332.
\doi{10.1080/10705511003661553}
}
\seealso{
Other Bootstrap for State Space Models Functions: 
\code{\link{PBSSMLinSDEFixed}()},
\code{\link{PBSSMOUFixed}()},
\code{\link{PBSSMVARFixed}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{Bootstrap for State Space Models Functions}
\keyword{boot}
\keyword{bootStateSpace}
\keyword{pb}
\keyword{ssm}
