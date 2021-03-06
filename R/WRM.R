#' @name WRM
#' @rdname WRM
#'
#' @title Wavelet-revised models (WRMs)
#'
#' @description A wavelet-based method to remove spatial autocorrelation
#' in multiple linear regressions. Wavelet transforms are implemented using
#' \pkg{waveslim} (Whitcher, 2005).
#'
#' @details
#' WRM can be used to fit linear models for response vectors of different
#' distributions: \code{gaussian}, \code{binomial}, or \code{poisson}.
#' As a spatial model, it is a generalized linear model in which the residuals
#' may be autocorrelated. It corrects for  2-dimensional residual
#' autocorrelation for regular gridded data sets using the wavelet
#' decomposition technique. The grid cells are assumed to be square.
#' Furthermore, this function requires that \strong{all predictor variables
#' be continuous}.
#'
#'
#' @param formula  Model formula. Variable names must match variables in \code{data}.
#' @param family   \code{gaussian}, \code{binomial}, or \code{poisson} are supported.
#' @param data     A data frame with variable names that match the variables specified in \code{formula}.
#' @param coord    A matrix of two columns with corresponding cartesian
#' coordinates. Currently only supports integer coordinates.
#' @param level    An integer specifying the degree of wavelet decomposition
#'      \itemize{
#'           \item{0} - Without autocorrelation removal (equivalent to a GLM)
#'           \item{1} - For best autocorrelation removal
#'           \item{...} - Higher integers possible. The limit depends on sample size
#'        }
#' @param wavelet   Name of wavelet family. \code{haar}, \code{d4}, and \code{la8}.
#' are possible. \code{haar} is the default.
#' @param wtrafo    Type of wavelet transform. Either \code{dwt} or \code{modwt}.
#' \code{dwt} is the default.
#' @param b.ini     Initial parameter values. Default is NULL.
#' @param pad       A list of parameters for padding wavelet coefficients.
#' \itemize{
#'    \item{padform} - 0, 1, and 2 are possible.
#'     \code{padform} is automatically set to
#'     0 when either \code{level}=0 or
#'     a \code{formula} including an intercept and a non-gaussian family
#'    \itemize{
#'      \item{0} - Padding with 0s.
#'      \item{1} - Padding with mean values.
#'      \item{2} - Padding with mirror values.
#'  }
#'    \item{padzone} - Factor for expanding the padding zone
#'}
#' @param control 	a list of parameters for controlling the fitting process.
#'    \itemize{
#'       \item{\code{eps}} - Positive convergence tolerance. Smaller values of
#'       \code{eps} provide better parameter estimates, but also reduce the probability
#'       of the iterations converging. In case of issues with convergence, test larger
#'       values of \code{eps}. Default is 10^-5.
#'       \item{\code{denom.eps}} - Default is 10^-20.
#'       \item{\code{itmax}} - Integer giving the maximum number of iterations.
#'       Default is 200.
#'}
#' @param moran.params    A list of parameters for calculating Moran's I.
#'   \itemize{
#'     \item\code{lim1} - Lower limit for first bin. Default is 0.
#'     \item\code{increment} - Step size for calculating Moran's I. Default is 1.
#'   }
#' @param plot     A logical value indicating whether to plot autocorrelation of
#' residuals by distance bin. NOW DEPRECATED in favor of \code{plot.WRM} method.
#' @param customize_plot Additional plotting parameters passed to \code{ggplot}.
#' NOW DEPRECATED in favor of \code{plot.WRM} method.
#'
#' @return An object of class \code{WRM}. This consists of a list with the
#' following elements:
#' \describe{
#'       \item{\code{call}}{Call}
#'       \item{\code{formula}}{Model formula}
#'       \item{\code{family}}{Family}
#'       \item{\code{coord}}{Coordinates used in the model}
#'       \item{\code{b}}{Estimate of regression parameters}
#'       \item{\code{s.e.}}{Standard errors}
#'       \item{\code{z}}{Depending on the \code{family}, either a \emph{z} or \emph{t} value}
#'       \item{\code{p}}{\emph{p}-values}
#'       \item{\code{fitted}}{Fitted values from the model}
#'       \item{\code{resid}}{Pearson residuals}
#'       \item{\code{b.sm}}{Parameter estimates of neglected smooth part}
#'       \item{\code{fitted.sm}}{Fitted values of neglected smooth part}
#'       \item{\code{level}}{Selected level of wavelet decomposition}
#'       \item{\code{wavelet}}{Selected wavelet}
#'       \item{\code{wtrafo}}{Selected wavelet transformation}
#'       \item{\code{padzone}}{Selected padding zone expansion factor}
#'       \item{\code{padform}}{Selected matrix padding type}
#'       \item{\code{n.eff}}{Effective number of observations}
#'       \item{\code{AIC}}{Akaike information criterion}
#'       \item{\code{AICc}}{AIC score corrected for small sample sizes}
#'       \item{\code{LogLik}}{Log likelihood of the model}
#'       \item{\code{ac.glm}}{Autocorrelation of GLM residuals}
#'       \item{\code{ac.wrm}}{Autocorrelation of WRM residuals}
#'       \item{\code{b.ini}}{Initial parameter values}
#'       \item{\code{control}}{Control parameters for the fitting process}
#'       \item{\code{moran.params}}{Parameters for calculating Moran's I}
#'       \item{\code{pad}}{List of parameters for padding wavelet coefficients}
#'       \item{\code{plot}}{An object of class \code{ggplot} containing information
#'       on the autocorrelation of residuals from the fitted \code{WRM} and a
#'       \code{GLM}}
#'}
#'
#' @note For those interested in multimodel inference approaches, \code{WRM} with
#' \code{level = 1} is identical to \code{mmiWMRR} with \code{scale = 1}.
#'
#' @seealso \code{\link{mmiWMRR}}, \code{\link{predict.WRM}}, \code{\link{summary.WRM}},
#' \code{\link{aic.calc}}
#'
#'
#' @references
#' Carl, G., Kuehn, I. (2010): A wavelet-based extension of generalized
#' linear models to remove the effect of spatial autocorrelation.
#' Geographical Analysis 42 (3), 323 - 337
#'
#' Whitcher, B. (2005) Waveslim: basic wavelet routines for one-, two-
#' and three-dimensional signal processing. R package version 1.5.
#' @examples
#' data(musdata)
#' coords <- musdata[,4:5]
#'
#'\dontrun{
#' mwrm <- WRM(musculus ~ pollution + exposure,
#'             family = "poisson",
#'             data = musdata,
#'             coord = coords,
#'             level = 1)
#'
#' pred <- predict(mwrm, newdata = musdata)
#'
#' summary(mwrm)
#'
#' plot(mwrm)
#'
#' library(ggplot2)
#'
#' my_wrm_plot <- mwrm$plot
#'
#' # increase axis text size
#' print(my_wrm_plot + ggplot2::theme(axis.text = element_text(size = 15)))
#'
#'}
#'
#' @author Gudrun Carl, Sam Levin
#' @importFrom ggplot2 theme element_blank element_line element_text
#' ggplot aes geom_line geom_point scale_color_manual
#' scale_x_continuous scale_y_continuous
#' @importFrom stats glm resid as.formula pt pnorm
#' @importFrom waveslim mra.2d
#' @importFrom rlang quo !!
#' @export


WRM<-function(formula,family,data,coord,
              level=1,wavelet="haar",wtrafo="dwt",
              b.ini=NULL,pad=list(),control=list(),moran.params=list(),
              plot=FALSE, customize_plot = NULL){

  if(!is.null(customize_plot) & plot) {
    warning('"customize_plot" and "plot = TRUE" arguments are now soft deprecated.\n',
            'Use plot.gee method and access the ggplot2 object using object_name$plot\n',
            'subsequent modification.')
  }


  n <- dim(data)[1]
  l <- dim(data)[2]
  x <- coord[ ,1]
  y <- coord[ ,2]
  if(length(x) != n) stop("length of data does not match length of coordinates")

  logic1 <- identical(as.numeric(x), round(x, 0))
  logic2 <- identical(as.numeric(y),round(y,0))
  if(!logic1 | !logic2) stop("coordinates not integer")

  X <- model.matrix(formula, data)
  nvar <- dim(X)[2]

  if(is.vector(model.frame(formula, data)[[1]])){
    yold <- model.frame(formula, data)[[1]]
    ntr <- 1
  }
  if(family == "binomial" & is.matrix(model.frame(formula, data)[[1]])){
    yold <- model.frame(formula, data)[[1]][ ,1]
    ntr <- model.frame(formula, data)[[1]][ ,1] +
           model.frame(formula, data)[[1]][ ,2]
  }

  n.level <- level
  length.s <- 3 * n.level + 1
  s <- rep(1, length.s)
  s[length.s] <- 0
  if(level == 0) {
    s <- c(1, 1, 1, 1)
    n.level <- 1
  }
  beta <- matrix(NA, 4, nvar)
  beta.smooth <- matrix(NA, 4, nvar)
  resi <- matrix(NA, 4, n)
  ac <- matrix(NA, 4, 10)
  acp <- matrix(NA, 4, 10)
  se <- matrix(NA, 4, nvar)

  pad <- do.call("wrm.pad", pad)
  padform <- pad$padform
  if(level == 0) padform <- 0
  if(family != "gaussian" & dimnames(X)[[2]][1] == "(Intercept)") padform<-0
  padzone <- pad$padzone
  control <- do.call("wrm.control", control)
  moran <- do.call("wrm.moran", moran.params)
  lim1 <- moran$lim1
  lim2 <- lim1 + moran$increment

  pdim <- max(max(y) - min(y) + 1,
              max(x) - min(x) + 1) * padzone
  power <- 0
  while(2^power < pdim) power <- power + 1
  xmargin0 <- as.integer((2^power - (max(x) - min(x) + 1)) /
                          2) - min(x) + 1
  ymargin0 <- as.integer((2^power - (max(y) - min(y) + 1)) /
                           2) - min(y) + 1

  if(power < n.level) stop("level is too high")
  if(log2(max(max(y) - min(y) + 1, max(x) - min(x) + 1)) == power &
     padzone == 1){
    message("warning: insufficient padding zone")
  }
  i4 <- 1
  while(i4 < 5){
    if(i4 == 1){
      xmargin <- xmargin0
      ymargin <- ymargin0
    }
    if(i4 == 2){
      xmargin <- xmargin0 + 1
      ymargin <- ymargin0
    }
    if(i4 == 3){
      xmargin <- xmargin0 + 1
      ymargin <- ymargin0 + 1
    }
    if(i4 == 4){
      xmargin <- xmargin0
      ymargin <- ymargin0 + 1
    }

    # GLM for comparison
    m0 <- stats::glm(formula, family, data)
    res0 <- stats::resid(m0, type = "pearson")
    beta0 <- m0$coeff


    if(is.null(b.ini)) {
      betaw <- rep(0, nvar)
    } else {
      betaw <- b.ini
    }
    lin <- X %*% betaw
    if(family == "gaussian") pi <- lin
    if(family == "binomial") pi <- exp(lin) / (1 + exp(lin))
    if(family == "poisson")  pi <- exp(lin)

    #if(family=="gaussian") pi<-rep(0,n)
    #if(family=="binomial") pi<-rep(.5,n)
    #if(family=="poisson") pi<-rep(1,n)

    it <- 0

    repeat{
      it <- it + 1
      if(family == "gaussian") var <- rep(1, n)
      if(family == "binomial") var <- ntr * pi*(1 - pi)
      if(family == "poisson")  var <- pi
      sigma <- as.vector(sqrt(var))
      W12 <- diag(sigma)
      Am12 <- diag(1 / sigma)
      Xnew <- W12 %*% X
      ynew <- W12 %*% X %*% betaw + Am12 %*% (yold - ntr * pi)

      F.mat <- matrix(NA, 2^power, 2^power)
      T.array <- array(NA, c(2^power, 2^power, nvar))
      for(ii in 1:n){
        kx <- x[ii] + xmargin
        ky <- y[ii] + ymargin
        F.mat[ky, kx] <- ynew[ii]
        for (i3 in 1:nvar)
          T.array[ky, kx, i3] <- Xnew[ii, i3]
      }  # ii loop

      P <- which(is.na(T.array), arr.ind = TRUE)
      if(padform == 0){
        F.mat[is.na(F.mat)] <- 0
        for(i3 in seq_len(nvar)){
          i1 <- P[which(P[ ,3] == i3), 1]
          i2 <- P[which(P[ ,3] == i3), 2]
          for(i in seq_len(length(i1))) T.array[i1[i], i2[i], i3] <- 0
        }
      }
      if(padform == 1){
        F.mat[is.na(F.mat)] <- mean(F.mat, na.rm = TRUE)
        for (i3 in seq_len(nvar)){
          i1 <- P[which(P[ ,3] == i3), 1]
          i2 <- P[which(P[ ,3] == i3), 2]
          for(i in seq_len(length(i1))){
            T.array[i1[i], i2[i], i3] <- mean(T.array[,,i3], na.rm = TRUE)
          }
        }
      }
      if(padform == 2){
        F.mat <- padding(F.mat)
        for (i3 in seq_len(nvar)){
          T.array[,,i3] <- padding(T.array[,,i3])
        }
      }

      p <- 2^power * 2^power
      tt <- matrix(0, p, nvar)
      tt0 <- matrix(0, p, nvar)
      if(is.na(max(abs(F.mat)))) {
        mdwt$coeff <- rep(NA, nvar)
        break
      }
      if(is.infinite(max(abs(F.mat)))) {
        mdwt$coeff <- rep(NA, nvar)
        break
      }

      FT <- waveslim::mra.2d(F.mat, wavelet, n.level, method = wtrafo)
      FTS <- rep(0, length(FT[[1]]))
      for(is in seq_len(length(s))){
        if(s[is] == 1) FTS <- FTS + FT[[is]]
        if(s[is] == 0) FT0 <- FT[[is]]
      }
      ft <- as.vector(FTS)
      if(level != 0) ft0 <- as.vector(FT0)
      for (i3 in seq_len(nvar)){
        TT <- waveslim::mra.2d(T.array[,,i3], wavelet, n.level,
                               method = wtrafo)
        TTS <- rep(0, length(TT[[1]]))
        TT0 <- rep(0, length(TT[[1]]))
        for(is in seq_len(length(s))){
          if(s[is] == 1) TTS <- TTS + TT[[is]]
          if(s[is] == 0) TT0 <- TT[[is]]
        }
        tt[ ,i3] <- as.vector(TTS)
        if(level != 0) tt0[ ,i3] <- as.vector(TT0)
      }

      xnam <- paste("tt[,", 1:nvar,"]", sep = "")
      formula.dwt <- as.formula(paste("ft~",
                                      paste(xnam, collapse = "+"),
                                      "-1"))
      mdwt <- lm(formula.dwt)
      if(sum(abs(tt[,1])) == 0) mdwt$coeff[1]<- beta0[1]
      if(is.na(max(abs(mdwt$coeff))) ) {
        mdwt$coeff <- rep(NA, nvar)
        break
      }
      if(max(abs(mdwt$coeff)) > 1e+10 ) {
        mdwt$coeff <- rep(NA, nvar)
        break
      }
      if(level != 0){
        xnam0 <- paste("tt0[,", 1:nvar, "]", sep = "")
        formula.dwt0 <- stats::as.formula(paste("ft0~",
                                                paste(xnam0, collapse = "+"),
                                                "-1"))
        mdwt0 <- lm(formula.dwt0)
        if(sum(abs(tt0[ ,1])) == 0) mdwt0$coeff[1] <- beta0[1]
      }

      lin<-X %*% mdwt$coeff
      if(family == "gaussian") pi <- lin
      if(family == "binomial") pi <- exp(lin) / (1 + exp(lin))
      if(family == "poisson") {
        pi <- exp(lin)
        if(min(pi) < 1e-10 | max(pi) > 1e+10) {
          mdwt$coeff <- rep(NA, nvar)
          fitted.sm  <- NA_real_
          break
        }
      }

      if (max(abs(mdwt$coeff - betaw) / (abs(betaw) + control$denom.eps))
          <= control$eps ) {
        i4 <- 4
        break
      }
      if (i4 == 4 & it > control$itmax)  stop("too many iterations")
      if (it > control$itmax) break
      betaw <- mdwt$coeff
    }  # next step of iteration

    if(!is.na(mdwt$coeff[1])){
      if(level != 0) Lin <- tt %*% mdwt$coeff + tt0 %*% mdwt0$coeff
      if(level == 0) Lin <- tt %*% mdwt$coeff
      Lin.Mat <- matrix(Lin, 2^power, 2^power)
      lin <- rep(0,n)
      for(i in seq_len(n)) lin[i] <- Lin.Mat[y[i] + ymargin, x[i] + xmargin]
      if(family == "gaussian") fitted.sm <- lin
      if(family == "binomial") fitted.sm <- exp(lin) / (1 + exp(lin))
      if(family == "poisson")  fitted.sm <- exp(lin)
    }

    if(!is.na(mdwt$coeff[1])){
      Resmdwt <- matrix(resid(mdwt), 2^power, 2^power)
      resmdwt <- rep(0,n)

      for(i in seq_len(n)) {
        resmdwt[i] <- Resmdwt[y[i] + ymargin, x[i] + xmargin]
      }

      acw <- acfft(coord, resmdwt, lim1, lim2)

      try.solve <- try(MASS::ginv(t(tt) %*% tt), silent = TRUE)
      if (inherits(try.solve, "try-error"))  var.b <- matrix(NA, nvar, nvar)
      if (!inherits(try.solve, "try-error")) var.b <- try.solve
      if(family == "gaussian"){
        df <- n-nvar
        sigma2 <- sum(resmdwt^2) / df
        var.b <- sigma2 * var.b
      }
      s.e. <- rep(NA, nvar)

      for(i in seq_len(nvar)){
        s.e.[i] <- sqrt(var.b[i,i])
      }
    }

    if(is.na(mdwt$coeff[1])) {
      acw <- NA
      acpw <- NA
      resmdwt <- NA
      s.e. <- NA
    }

    beta[i4, 1:nvar] <- mdwt$coeff[1:nvar]
    if(level != 0 & !is.na(mdwt$coeff[1])) {
      beta.smooth[i4,1:nvar] <- mdwt0$coeff[1:nvar]
    }
    resi[i4, 1:n] <- resmdwt[1:n]
    ac[i4, 1:10] <- acw[1:10]
    se[i4, 1:nvar] <- s.e.[1:nvar]

    i4 <- i4 + 1
  } # i4 loop #..........................................

  if(all(is.na(mdwt$coeff))) {
    warning("Could not estimate coefficients!",
            call. = FALSE)
  }

  glm.beta <- beta0
  wavelet.beta <- apply(beta, 2, mean, na.rm = TRUE)
  if(level != 0) wavelet.beta.smooth <- apply(beta.smooth, 2,
                                              mean, na.rm = TRUE)
  ac0 <- acfft(coord, res0, lim1, lim2)
  acw <- apply(ac, 2, mean, na.rm = TRUE)
  resw <- apply(resi, 2, mean, na.rm = TRUE)
  s.e. <- apply(se, 2, mean, na.rm = TRUE)
  lin <- X %*% wavelet.beta
  if(family == "gaussian") pi <- lin
  if(family == "binomial") pi <- exp(lin) / (1 + exp(lin))
  if(family == "poisson")  pi <- exp(lin)
  if(level == 0) n.eff <- n
  if(level != 0) n.eff <- round(n * (1 - 1 / (2^level * 2^level)))
  aic <- aic.calc(formula, family, data, mu = pi, n.eff = n.eff)
  AIC <- aic$AIC
  AICc <- aic$AICc
  LogLik <- aic$loglik


  z.value <- rep(NA, nvar)
  pr <- rep(NA, nvar)
  if(!is.na(wavelet.beta[1]) & !is.na(s.e.[1])) {
    z.value <- wavelet.beta / s.e.
    for(i in seq_len(nvar)){
      if(family == "gaussian"){
        if(z.value[i] <= 0) pr[i] <- 2 * stats::pt(z.value[i], df)
        if(z.value[i] > 0)  pr[i] <- 2 * (1 - stats::pt(z.value[i], df))
      }
      if(family == "binomial" | family == "poisson"){
        if(z.value[i] <= 0) pr[i] <- 2 * stats::pnorm(z.value[i])
        if(z.value[i] > 0)  pr[i] <- 2 * (1 - stats::pnorm(z.value[i]))
      }
    }
  }

  if(!is.na(acw[1])) {
    plt.blank <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                panel.grid.minor = ggplot2::element_blank(),
                                panel.background = ggplot2::element_blank(),
                                axis.line = ggplot2::element_line(colour = "black"),
                                legend.title = ggplot2::element_text(size = 9))

    plt.data <- data.frame(val = seq_len(length(acw)),
                           ac.wrm = acw,
                           ac.glm = ac0)

    y.breaks <- round(seq(min(plt.data[ ,2:3]) - .02,
                          max(plt.data[ ,2:3]) + .02,
                          length.out = 6), 2)

    val <- rlang::quo(val)
    ac.wrm <- rlang::quo(ac.wrm)
    ac.glm <- rlang::quo(ac.glm)

    plt <- ggplot2::ggplot(data = plt.data,
                           ggplot2::aes(x = !! val)) +
      plt.blank +
      ggplot2::geom_line(ggplot2::aes(y = ac.wrm,
                                      color = "WRM Residuals"),
                         size = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = !! ac.glm,
                                      color = "GLM Residuals"),
                         size = 0.9) +
      ggplot2::geom_point(ggplot2::aes(y = !! ac.wrm,
                                       color = "WRM Residuals"),
                          size = 2) +
      ggplot2::geom_point(ggplot2::aes(y = !! ac.glm,
                                       color = "GLM Residuals"),
                          size = 2) +
      ggplot2::scale_color_manual(paste('Correlation for level = ',
                                        level),
                                  breaks = c('WRM Residuals',
                                             'GLM Residuals'),
                                  values = c('red', 'blue')) +
      ggplot2::scale_x_continuous('Lag Distance', breaks = 1:10) +
      ggplot2::scale_y_continuous("Autocorrelation of residuals",
                                  breaks = y.breaks,
                                  limits = c(min(plt.data[ ,2:3]) - .02,
                                             max(plt.data[ ,2:3]) + .02)) +
      customize_plot

  } else {

    warning("NAs generated when calculating auto-correlation of residuals.\n",
            "Could not generate autocorrelation plot.",
            call. = FALSE)
    plt <- NA

  }

  if(plot & !is.na(acw[1])) {

    print(plt)

  }

  glm.beta <- beta0
  coef <- as.vector(wavelet.beta)
  names(coef) <- dimnames(X)[[2]]
  if(level!=0) coefsm <- as.vector(wavelet.beta.smooth)
  if(level==0) coefsm <- rep(NA,length(coef))
  names(coefsm) <- dimnames(X)[[2]]

  call <- match.call()
  fit <- list(call = call,
            formula = formula,
            family = family,
            coord = coord,
            b = coef,
            s.e. = s.e.,
            z = z.value,
            p = pr,
            fitted = pi,
            resid = resw,
            b.sm = coefsm,
            fitted.sm = fitted.sm,
            level = level,
            wavelet = wavelet,
            wtrafo = wtrafo,
            padzone = padzone,
            padform = padform,
            it = it,
            n = n,
            n.eff = n.eff,
            AIC = AIC,
            AICc = AICc,
            LogLik = LogLik,
            ac.glm = ac0,
            ac.wrm = acw,
            b.ini = b.ini,
            control = control,
            pad = pad,
            moran.params = moran.params,
            plot = plt)

  class(fit) <- "WRM"
  return(fit)
}


#' @name plot.WRM
#' @rdname WRM
#'
#' @inheritParams plot.GEE
#'
#' @export

plot.WRM <- function(x, ...) {

  print(x$plot)
  invisible(x)

}

#' @name summary.WRM
#' @rdname WRM
#'
#' @param object An object of class \code{WRM}
#' @param ... Not used
#'
#' @importFrom stats printCoefmat
#' @export

summary.WRM<-function (object,...) {
  cat("\n","Call:","\n")
  print(object$call)
  cat("\n","Pearson Residuals:","\n")
  print(summary(object$resid))
  family<-object$family
  b<-object$b
  s.e.<-object$s.e.
  z<-object$z
  p<-object$p
  it<-object$it
  n<-object$n
  n.eff<-object$n.eff
  AIC<-object$AIC
  beta<-cbind(b,s.e.,z,p)
  if(family=="gaussian")
    colnames(beta) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)")
  if(family=="binomial" | family=="poisson")
    colnames(beta) <- c("Estimate", "Std.Err", "z value", "Pr(>|z|)")
  cat("---","\n","Coefficients:","\n")
  stats::printCoefmat(beta)
  cat("---","\n","Number of observations n: ",n, ",  n.eff: ",
      n.eff,",  AIC: ",AIC,"\n" )
  cat("\n","Number of iterations: ",it,"\n")
  cat("---","\n")
  ac0<-object$ac.glm
  acw<-object$ac.wrm
  cat("Autocorrelation of glm.residuals","\n")
  print(ac0)
  cat("Autocorrelation of wavelet.residuals","\n")
  print(acw)
}

#' @inheritParams summary.WRM
#' @param newdata  A data frame containing variables used to make predictions.
#' @param sm       Logical. Should part of smooth components be included?
#' @param newcoord New coordinates corresponding to observations in \code{newdata}.
#'
#' @name predict.WRM
#' @rdname WRM
#'
#' @importFrom stats model.matrix
#' @importFrom waveslim mra.2d
#' @export
#'


predict.WRM<-function(object ,newdata,sm=FALSE,newcoord=NA, ...){

  data<-newdata
  formula<-object$formula
  family<-object$family
  b<-object$b
  bsm<-object$b.sm
  level<-object$level
  padzone<-object$padzone
  padform<-object$padform
  coord<-newcoord

  X<-stats::model.matrix(formula,data)
  nvar<-dim(X)[2]
  n<-dim(data)[1]
  l<-dim(data)[2]

  if(sm) { # add part of smooth components
    if(is.na(sum(coord))) stop("coordinates are required")
    x<-coord[,1]
    y<-coord[,2]
    if(length(x)!=n) stop("error in dimension")
    logic1<-identical(as.numeric(x),round(x,0))
    logic2<-identical(as.numeric(y),round(y,0))
    if(!logic1 | !logic2) stop("coordinates not integer")

    n.level<-level
    length.s<-3*n.level+1
    s<-rep(1,length.s)
    s[length.s]<-0
    if(level==0) {
      s<-c(1,1,1,1)
      n.level<-1
    }

    pdim<- max(max(y)-min(y)+1,max(x)-min(x)+1)*padzone
    power<-0
    while(2^power<pdim) power<-power+1
    xmargin<-as.integer((2^power-(max(x)-min(x)))/2)-min(x)+1
    ymargin<-as.integer((2^power-(max(y)-min(y)))/2)-min(y)+1

    Tmat<-array(NA,c(2^power,2^power,nvar))

    for(ii in seq_len(n)){
      kx<-x[ii]+xmargin
      ky<-y[ii]+ymargin
      for (i3 in 1:nvar)
        Tmat[ky,kx,i3]<-X[ii,i3]
    }  # ii loop

    P<-which(is.na(Tmat), arr.ind = TRUE)
    if(padform==0){
      for (i3 in seq_len(nvar)){
        i1<-P[which(P[,3]==i3),1]
        i2<-P[which(P[,3]==i3),2]
        for(i in seq_len(length(i1))) Tmat[i1[i],i2[i],i3]<-0
      }
    }
    if(padform==1){
      for (i3 in seq_len(nvar)){
        i1<-P[which(P[ ,3]==i3) ,1]
        i2<-P[which(P[ ,3]==i3) ,2]
        for(i in seq_len(length(i1))){ Tmat[i1[i],
                                            i2[i],
                                            i3]<-mean(Tmat[,,i3],
                                                      na.rm=TRUE)
        }
      }
    }
    if(padform==2){
      for (i3 in seq_len(nvar)) Tmat[,,i3] <- padding(Tmat[,,i3])
    }

    p<-2^power*2^power
    tt<-matrix(0,p,nvar)
    tt0<-matrix(0,p,nvar)
    for (i3 in seq_len(nvar)){
      TT<-waveslim::mra.2d(Tmat[,,i3],
                           object$wavelet,
                           n.level,
                           method=object$wtrafo)
      TTS<-rep(0,length(TT[[1]]))
      TT0<-rep(0,length(TT[[1]]))
      for(is in seq_len(length(s))){
        if(s[is]==1) TTS <- TTS + TT[[is]]
        if(s[is]==0) TT0 <- TT[[is]]
      }
      tt[,i3]<-as.vector(TTS)
      if(level!=0) tt0[,i3]<-as.vector(TT0)
    }

    if(level!=0) lin<- tt%*%b  + tt0%*%bsm
    if(level==0) lin<- tt%*%b
    Fitted<-matrix(lin,2^power,2^power)
    fitted<-rep(0,n)
    for(i in seq_len(n)) fitted[i]<-Fitted[y[i]+ymargin,x[i]+xmargin]
  } # add part of smooth components

  if(!sm) { # only part of detail components
    fitted<- X%*%b
  } # only part of detail components

  lin<-as.vector(fitted)
  if(family=="gaussian") pi<-lin
  if(family=="binomial") pi<-exp(lin)/(1+exp(lin))
  if(family=="poisson")  pi<-exp(lin)
  predict<-pi
  return(predict)
}

