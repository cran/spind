#' Wavelet variance analysis
#'
#' @description Calculates the wavelet variance based on a wavelet
#' multiresolution analysis.
#'
#' @param f    A vector
#' @param coord    A matrix of two columns with corresponding cartesian
#' coordinates. Currently only supports integer coordinates.
#' @param wavelet   Name of wavelet family. \code{haar}, \code{d4}, and \code{la8}.
#' are possible. \code{haar} is the default.
#' @param wtrafo    Type of wavelet transform. Either \code{dwt} or \code{modwt}.
#' \code{dwt} is the default.
#'
#' @return Wavelet variance for \code{f}.
#'
#' @seealso \pkg{waveslim}, \code{\link{WRM}}, \code{\link{covar.plot}},
#'  \code{\link{scaleWMRR}}
#'
#' @author Gudrun Carl
#'
#' @examples
#' data(carlinadata)
#'
#' coords <- carlinadata[ ,4:5]
#' pv <- covar.plot(carlina.horrida ~ aridity + land.use,
#'                  data = carlinadata,
#'                  coord = coords,
#'                  wavelet = 'd4',
#'                  wtrafo = 'modwt',
#'                  plot = 'var')
#'
#' pv$plot
#'
#' @importFrom waveslim modwt.2d dwt.2d
#' @export

wavevar<-function(f,coord,wavelet="haar",wtrafo="dwt"){

  x <- coord[ ,1]
  y <- coord[ ,2]

  n <- length(f)
  pdim <- max(max(y) - min(y) + 1, max(x) - min(x) + 1)
  power <- 0
  while(2^power < pdim) power <- power + 1
  xmargin <- as.integer((2^power - (max(x) - min(x))) / 2) - min(x) + 1
  ymargin <- as.integer((2^power - (max(y) - min(y))) / 2) - min(y) + 1
  f <- scale(f) # for scaling and centering
  Fmat <- matrix(0, 2^power, 2^power)
  for(ii in 1:n){
    kx <- x[ii] + xmargin
    ky <- y[ii] + ymargin
    Fmat[kx, ky] <- f[ii]
  } # ii loop
  level <- power
  p <- 2^power * 2^power
  if(wtrafo == "dwt") F.dwt <- waveslim::dwt.2d(Fmat, wavelet, level)
  if(wtrafo == "modwt") F.dwt <- waveslim::modwt.2d(Fmat, wavelet, level)
  # wavelet variance (1/n) * sum[abs(f.dwt)^2 ]
  Var <- rep(NA, level)
  for(ik in 1:level){
    ii <- 3 * (ik - 1) + 1
    FS1 <- (1/n) * sum(abs(F.dwt[[ii]])^2)
    FS2 <- (1/n) * sum(abs(F.dwt[[ii + 1]])^2)
    FS3 <- (1/n) * sum(abs(F.dwt[[ii + 2]])^2)
    Var[ik] <- FS1 + FS2 + FS3 # all 3 components
  }
  # windows()
  # plot(Var)
  Var <- round(Var, 4)
  Var
}
