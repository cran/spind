% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WRM.R
\name{WRM}
\alias{WRM}
\alias{plot.WRM}
\alias{summary.WRM}
\alias{predict.WRM}
\title{Wavelet-revised models (WRMs)}
\usage{
WRM(
  formula,
  family,
  data,
  coord,
  level = 1,
  wavelet = "haar",
  wtrafo = "dwt",
  b.ini = NULL,
  pad = list(),
  control = list(),
  moran.params = list(),
  plot = FALSE,
  customize_plot = NULL
)

\method{plot}{WRM}(x, ...)

\method{summary}{WRM}(object, ...)

\method{predict}{WRM}(object, newdata, sm = FALSE, newcoord = NA, ...)
}
\arguments{
\item{formula}{Model formula. Variable names must match variables in \code{data}.}

\item{family}{\code{gaussian}, \code{binomial}, or \code{poisson} are supported.}

\item{data}{A data frame with variable names that match the variables specified in \code{formula}.}

\item{coord}{A matrix of two columns with corresponding cartesian
coordinates. Currently only supports integer coordinates.}

\item{level}{An integer specifying the degree of wavelet decomposition
\itemize{
     \item{0} - Without autocorrelation removal (equivalent to a GLM)
     \item{1} - For best autocorrelation removal
     \item{...} - Higher integers possible. The limit depends on sample size
  }}

\item{wavelet}{Name of wavelet family. \code{haar}, \code{d4}, and \code{la8}.
are possible. \code{haar} is the default.}

\item{wtrafo}{Type of wavelet transform. Either \code{dwt} or \code{modwt}.
\code{dwt} is the default.}

\item{b.ini}{Initial parameter values. Default is NULL.}

\item{pad}{A list of parameters for padding wavelet coefficients.
\itemize{
   \item{padform} - 0, 1, and 2 are possible.
    \code{padform} is automatically set to
    0 when either \code{level}=0 or
    a \code{formula} including an intercept and a non-gaussian family
   \itemize{
     \item{0} - Padding with 0s.
     \item{1} - Padding with mean values.
     \item{2} - Padding with mirror values.
 }
   \item{padzone} - Factor for expanding the padding zone
}}

\item{control}{a list of parameters for controlling the fitting process.
   \itemize{
      \item{\code{eps}} - Positive convergence tolerance. Smaller values of
      \code{eps} provide better parameter estimates, but also reduce the probability
      of the iterations converging. In case of issues with convergence, test larger
      values of \code{eps}. Default is 10^-5.
      \item{\code{denom.eps}} - Default is 10^-20.
      \item{\code{itmax}} - Integer giving the maximum number of iterations.
      Default is 200.
}}

\item{moran.params}{A list of parameters for calculating Moran's I.
\itemize{
  \item\code{lim1} - Lower limit for first bin. Default is 0.
  \item\code{increment} - Step size for calculating Moran's I. Default is 1.
}}

\item{plot}{A logical value indicating whether to plot autocorrelation of
residuals by distance bin. NOW DEPRECATED in favor of \code{plot.WRM} method.}

\item{customize_plot}{Additional plotting parameters passed to \code{ggplot}.
NOW DEPRECATED in favor of \code{plot.WRM} method.}

\item{x}{An object of class \code{GEE} or \code{WRM}}

\item{...}{Not used}

\item{object}{An object of class \code{WRM}}

\item{newdata}{A data frame containing variables used to make predictions.}

\item{sm}{Logical. Should part of smooth components be included?}

\item{newcoord}{New coordinates corresponding to observations in \code{newdata}.}
}
\value{
An object of class \code{WRM}. This consists of a list with the
following elements:
\describe{
      \item{\code{call}}{Call}
      \item{\code{formula}}{Model formula}
      \item{\code{family}}{Family}
      \item{\code{coord}}{Coordinates used in the model}
      \item{\code{b}}{Estimate of regression parameters}
      \item{\code{s.e.}}{Standard errors}
      \item{\code{z}}{Depending on the \code{family}, either a \emph{z} or \emph{t} value}
      \item{\code{p}}{\emph{p}-values}
      \item{\code{fitted}}{Fitted values from the model}
      \item{\code{resid}}{Pearson residuals}
      \item{\code{b.sm}}{Parameter estimates of neglected smooth part}
      \item{\code{fitted.sm}}{Fitted values of neglected smooth part}
      \item{\code{level}}{Selected level of wavelet decomposition}
      \item{\code{wavelet}}{Selected wavelet}
      \item{\code{wtrafo}}{Selected wavelet transformation}
      \item{\code{padzone}}{Selected padding zone expansion factor}
      \item{\code{padform}}{Selected matrix padding type}
      \item{\code{n.eff}}{Effective number of observations}
      \item{\code{AIC}}{Akaike information criterion}
      \item{\code{AICc}}{AIC score corrected for small sample sizes}
      \item{\code{LogLik}}{Log likelihood of the model}
      \item{\code{ac.glm}}{Autocorrelation of GLM residuals}
      \item{\code{ac.wrm}}{Autocorrelation of WRM residuals}
      \item{\code{b.ini}}{Initial parameter values}
      \item{\code{control}}{Control parameters for the fitting process}
      \item{\code{moran.params}}{Parameters for calculating Moran's I}
      \item{\code{pad}}{List of parameters for padding wavelet coefficients}
      \item{\code{plot}}{An object of class \code{ggplot} containing information
      on the autocorrelation of residuals from the fitted \code{WRM} and a
      \code{GLM}}
}
}
\description{
A wavelet-based method to remove spatial autocorrelation
in multiple linear regressions. Wavelet transforms are implemented using
\pkg{waveslim} (Whitcher, 2005).
}
\details{
WRM can be used to fit linear models for response vectors of different
distributions: \code{gaussian}, \code{binomial}, or \code{poisson}.
As a spatial model, it is a generalized linear model in which the residuals
may be autocorrelated. It corrects for  2-dimensional residual
autocorrelation for regular gridded data sets using the wavelet
decomposition technique. The grid cells are assumed to be square.
Furthermore, this function requires that \strong{all predictor variables
be continuous}.
}
\note{
For those interested in multimodel inference approaches, \code{WRM} with
\code{level = 1} is identical to \code{mmiWMRR} with \code{scale = 1}.
}
\examples{
data(musdata)
coords <- musdata[,4:5]

\dontrun{
mwrm <- WRM(musculus ~ pollution + exposure,
            family = "poisson",
            data = musdata,
            coord = coords,
            level = 1)

pred <- predict(mwrm, newdata = musdata)

summary(mwrm)

plot(mwrm)

library(ggplot2)

my_wrm_plot <- mwrm$plot

# increase axis text size
print(my_wrm_plot + ggplot2::theme(axis.text = element_text(size = 15)))

}

}
\references{
Carl, G., Kuehn, I. (2010): A wavelet-based extension of generalized
linear models to remove the effect of spatial autocorrelation.
Geographical Analysis 42 (3), 323 - 337

Whitcher, B. (2005) Waveslim: basic wavelet routines for one-, two-
and three-dimensional signal processing. R package version 1.5.
}
\seealso{
\code{\link{mmiWMRR}}, \code{\link{predict.WRM}}, \code{\link{summary.WRM}},
\code{\link{aic.calc}}
}
\author{
Gudrun Carl, Sam Levin
}
