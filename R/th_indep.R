#'@title Spatial threshold-independent accuracy measures
#'
#'@description Calculates spatially corrected, threshold-independent metrics for
#'an observational data set and model predictions (AUC, ROC, max-TSS)
#'
#'@param data A data frame or matrix with two columns. The first column
#'should contain actual presence/absence data (binary, 0 or 1) and the
#'second column should contain model predictions of probability of
#'occurrence (numeric, between 0 and 1).
#'@param coord A data frame or matrix with two columns containing x,y
#'coordinates for each actual and predicted value. Coordinates must be
#'integer and consecutively numbered.
#'@param spatial A logical value indicating whether spatial corrected
#'indices (rather than classical indices) should be computed.
#'@param plot.ROC A logical indicating whether the ROC should be plotted. NOW DEPRECATED.
#'@param customize_plot Additional plotting parameters passed to \code{ggplot}. NOW DEPRECATED.
#'
#'
#'@return A list with the following components:
#'\describe{
#'  \item{\code{AUC}}{Area under curve}
#'  \item{\code{opt.thresh}}{optimal threshold for maximum TSS value}
#'  \item{\code{TSS}}{Maximum TSS value}
#'  \item{\code{sensitivity}}{Sensitivity}
#'  \item{\code{Specificity}}{Specificity}
#'  \item{\code{AUC.plot}}{A \code{ggplot} object}
#'}
#'
#'@author
#'Gudrun Carl
#'
#'@seealso \code{\link{th.dep}}
#'
#'@examples
#'data(hook)
#'data <- hook[ ,1:2]
#'coord <- hook[ ,3:4]
#'si2 <- th.indep(data, coord, spatial = TRUE)
#'si2$AUC
#'si2$TSS
#'si2$opt.thresh
#'si2$plot
#'
#' @references Carl G, Kuehn I (2017) Spind: a package for computing spatially
#'  corrected accuracy measures. Ecography 40: 675-682.
#'  DOI: 10.1111/ecog.02593
#'
#' @importFrom ggplot2 theme element_blank element_line ggplot aes
#' geom_line scale_color_manual scale_x_continuous scale_y_continuous
#' @importFrom rlang quo !!

#' @export


th.indep <- function(data, coord, spatial = TRUE, plot.ROC = FALSE,
                     customize_plot = NULL){

  if(!is.null(customize_plot) | plot.ROC) {
    warning('"customize_plot" and "plot.ROC" arguments are now soft deprecated.\n',
            'Use object_name$plot to print the ggplot2 object and for \n',
            'subsequent modification.')
  }

  if(dim(data)[1] != dim(coord)[1]) stop("error in dimension")

  if(spatial){
    y <- adjusted.actuals(data, coord)
    split <- 4
  }
  if(!spatial){
    y <- data[ ,1]
    split <- 2
  }

  pi <- data[,2]
  n <- length(pi)
  o <- order(pi)
  piord <- pi[o]
  cutoff <- c(1.1, piord[n:1])

  sensitivity <- rep(NA, n)
  specificity <- rep(NA, n)
  splitlevel <- matrix(NA, split, 2)
  splitlevely <- matrix(NA, split, 2)

  for(i.n in 1:n){
    thresh <- cutoff[i.n]

    if(split == 4) {
      lower.split <- thresh / 2
      upper.split <- (1 + thresh) / 2
      splitlevel[1, ] <- c(0, lower.split)
      splitlevel[2, ] <- c(lower.split, thresh)
      splitlevel[3, ] <- c(thresh, upper.split)
      splitlevel[4, ] <- c(upper.split, 1)
      splitlevely[1, ] <- c(0, 0.25)
      splitlevely[2, ] <- c(0.25, 0.5)
      splitlevely[3, ] <- c(0.5, 0.75)
      splitlevely[4, ] <- c(0.75, 1)
      pipos <- matrix(0, n, 4)
      ypos <- matrix(0, n, 4)
      for(k in seq_len(n)){
        for(ksp in seq_len(3)){
          if(splitlevel[ksp,1] <= pi[k] &
             pi[k] < splitlevel[ksp,2]) {
            pipos[k,ksp] <- 1
          }
          if(splitlevely[ksp,1] <= y[k] &
             y[k]  < splitlevely[ksp,2]) {
            ypos[k,ksp] <- 1
          }
        }
        if(splitlevel[4,1] <= pi[k] &
           pi[k] <= splitlevel[4,2]){
          pipos[k,4] <- 1
        }
        if(splitlevely[4,1] <= y[k] &
           y[k]  <= splitlevely[4,2]) {
          ypos[k,4] <- 1
        }
      }
      cm <- matrix(0, 4, 4)
      for(k in seq_len(n)){
        i <- which(ypos[k, ] == 1)
        j <- which(pipos[k, ] == 1)
        cm[i,j] <- cm[i,j] + 1
      }
      cm <- matrix(rev(as.vector(cm)), 4, 4, byrow = TRUE)

      w<-matrix(NA, 4, 4)
      for (i in seq_len(4)){
        for (j in seq_len(4)){
          w[i,j] <- ifelse(abs(i - j) < 2, 1, 0)
        }}
      n <- sum(cm)
      sensitivity[i.n] <- sum(w[ ,1:2] * cm[ ,1:2]) / sum(cm[ ,1:2])
      specificity[i.n] <- sum(w[ ,3:4] * cm[ ,3:4]) / sum(cm[ ,3:4])

    }

    if(split == 2) {
      splitlevel[1, ] <- c(0, thresh)
      splitlevel[2, ] <- c(thresh, 1)
      pipos <- matrix(0, n, 2)
      ypos <- matrix(0, n, 2)
      for(k in seq_len(n)){
        if(splitlevel[1,1] <= pi[k] & pi[k] < splitlevel[1,2]) pipos[k,1] <- 1
        if(splitlevel[1,1] <= y[k]  & y[k]  < splitlevel[1,2]) ypos[k,1] <- 1
        if(splitlevel[2,1] <= pi[k] & pi[k] <= splitlevel[2,2]) pipos[k,2] <- 1
        if(splitlevel[2,1] <= y[k]  & y[k]  <= splitlevel[2,2]) ypos[k,2] <- 1
      }
      cm <- matrix(0, 2, 2)
      for(k in seq_len(n)){
        i <- which(ypos[k, ] == 1)
        j <- which(pipos[k, ] == 1)
        cm[i,j] <- cm[i,j] + 1
      }
      cm <- matrix(rev(as.vector(cm)), 2, 2, byrow = TRUE)
      w <- matrix(NA, 2, 2)
      for (i in seq_len(2)){
        for (j in seq_len(2)){
          # w = identity matrix
          if(split == 2) w[i,j] <- ifelse(abs(i - j) == 0, 1, 0)
        }}
      n<-sum(cm)
      sensitivity[i.n] <- sum(w[,1]*cm[ ,1]) / sum(cm[ ,1])
      specificity[i.n] <- sum(w[,2]*cm[ ,2]) / sum(cm[ ,2])

    }

  }


  n <- length(pi)
  sensitivity[is.na(sensitivity)] <- 0
  specificity[is.na(specificity)] <- 0
  TSS <- max(sensitivity + specificity) - 1


  plt.data <- data.frame(spec = 1-specificity,
                         sens = sensitivity)

  plt.blank <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                              panel.grid.minor = ggplot2::element_blank(),
                              panel.background = ggplot2::element_blank(),
                              axis.line = ggplot2::element_line(colour = "black"))

  spec <- rlang::quo(spec)
  sens <- rlang::quo(sens)

  plt <- ggplot2::ggplot(data = plt.data,
                         ggplot2::aes(x = !! spec)) +
    plt.blank +
    ggplot2::geom_line(ggplot2::aes(y = !! spec,
                                    color = "1:1 Line"),
                       size = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = !! sens,
                                    color = "ROC"),
                       size = 0.9) +
    ggplot2::scale_color_manual("",
                                breaks = c('1:1 Line','ROC'),
                                values = c('red', 'blue')) +
    ggplot2::scale_x_continuous('1 - Specificity', breaks = seq(0, 1, .25)) +
    ggplot2::scale_y_continuous("Sensitivity",
                                limits = c(0,1)) +
    customize_plot

  if(plot.ROC){
        print(plt)
  }

  dat <- cbind(c(1 - specificity, 1, 1, 0),
             c(sensitivity, 1, 0, 0))
  AUC <- splancs::areapl(dat)

  sensitivity <- rev(sensitivity)
  specificity <- rev(specificity)
  opt.thresh <- piord[which.max(sensitivity + specificity)]

  return(list(AUC = AUC,
              opt.thresh = opt.thresh,
              TSS = TSS,
              sensitivity = sensitivity,
              specificity = specificity,
              plot = plt))

}
