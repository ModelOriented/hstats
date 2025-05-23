% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/H2.R
\name{h2}
\alias{h2}
\alias{h2.default}
\alias{h2.hstats}
\title{Total Interaction Strength}
\usage{
h2(object, ...)

\method{h2}{default}(object, ...)

\method{h2}{hstats}(object, normalize = TRUE, squared = TRUE, ...)
}
\arguments{
\item{object}{Object of class "hstats".}

\item{...}{Currently unused.}

\item{normalize}{Should statistics be normalized? Default is \code{TRUE}.}

\item{squared}{Should \emph{squared} statistics be returned? Default is \code{TRUE}.}
}
\value{
An object of class "hstats_matrix" containing these elements:
\itemize{
\item \code{M}: Matrix of statistics (one column per prediction dimension), or \code{NULL}.
\item \code{SE}: Matrix with standard errors of \code{M}, or \code{NULL}.
Multiply with \code{sqrt(m_rep)} to get \emph{standard deviations} instead.
Currently, supported only for \code{\link[=perm_importance]{perm_importance()}}.
\item \code{m_rep}: The number of repetitions behind standard errors \code{SE}, or \code{NULL}.
Currently, supported only for \code{\link[=perm_importance]{perm_importance()}}.
\item \code{statistic}: Name of the function that generated the statistic.
\item \code{description}: Description of the statistic.
}
}
\description{
Proportion of prediction variability unexplained by main effects of \code{v}, see Details.
Use \code{plot()} to get a barplot.
}
\details{
If the model is additive in all features, then the (centered) prediction
function \eqn{F} equals the sum of the (centered) partial dependence
functions \eqn{F_j(x_j)}, i.e.,
\deqn{
  F(\mathbf{x}) = \sum_{j}^{p} F_j(x_j)
}
(check \code{\link[=partial_dep]{partial_dep()}} for all definitions).
To measure the relative amount of variability unexplained by all main effects,
we can therefore study the test statistic of total interaction strength
\deqn{
  H^2 = \frac{\frac{1}{n} \sum_{i = 1}^n \big[F(\mathbf{x}_i) - 
  \sum_{j = 1}^p\hat F_j(x_{ij})\big]^2}{\frac{1}{n} 
  \sum_{i = 1}^n\big[F(\mathbf{x}_i)\big]^2}.
}
A value of 0 means there are no interaction effects at all.
Due to (typically undesired) extrapolation effects, depending on the model,
values above 1 may occur.

In Żółkowski et al. (2023), \eqn{1 - H^2} is called \emph{additivity index}.
A similar measure using accumulated local effects is discussed in Molnar (2020).
}
\section{Methods (by class)}{
\itemize{
\item \code{h2(default)}: Default method of total interaction strength.

\item \code{h2(hstats)}: Total interaction strength from "interact" object.

}}
\examples{
# MODEL 1: Linear regression
fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
s <- hstats(fit, X = iris[, -1])
h2(s)

# MODEL 2: Multi-response linear regression
fit <- lm(as.matrix(iris[, 1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
s <- hstats(fit, X = iris[, 3:5])
h2(s)

# MODEL 3: No interactions
fit <- lm(Sepal.Length ~ ., data = iris)
s <- hstats(fit, X = iris[, -1], verbose = FALSE)
h2(s)
}
\references{
\enumerate{
\item Żółkowski, Artur, Mateusz Krzyziński, and Paweł Fijałkowski.
\emph{Methods for extraction of interactions from predictive models.}
Undergraduate thesis. Faculty of Mathematics and Information Science,
Warsaw University of Technology (2023).
\item Molnar, Christoph, Giuseppe Casalicchio, and Bernd Bischl".
\emph{Quantifying Model Complexity via Functional Decomposition for Better Post-hoc Interpretability},
in Machine Learning and Knowledge Discovery in Databases,
Springer International Publishing (2020): 193-204.
}
}
\seealso{
\code{\link[=hstats]{hstats()}}, \code{\link[=h2_overall]{h2_overall()}}, \code{\link[=h2_pairwise]{h2_pairwise()}}, \code{\link[=h2_threeway]{h2_threeway()}}
}
