#' Contour of data points using Kernel Density Estimate.
#'
#' @description
#' This function calculates contour points and area from list of data.frames
#' with data in 2d. Basically this is a wrapper of \code{\link[ks:kde]{ks::kde}}.
#'
#' @details
#' This function (renamed as \code{kde_from_tclist}) is shared with
#' \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param dflist list of data frames
#' @param alpha contour level, an scalar from the sequence .01 to .99 by step
#'   size .01. For example, a value of .95 gives a contour level associated with
#'   the cumulative prob of 95\% (and 95\% confidence interval)
#' @param h_band optional argument for the bandwidth of the kde object. If NULL,
#' the optimal band would be selected through the \code{\link[ks:kde]{ks::kde}}
#' function. Default is NULL.
#' @param position Columns position of x/y pair. Default is 1:2
#' @param grid_size size of the grid which is going to be used for the
#' evaluation of kde object. Can be reduced to speed-up computation.
#'
#' @return \code{contour} List of contour(s) at the specified level
#' @export
kde_from_list <- function(dflist, alpha, h_band = NULL,
                            position = 1:2,
                            grid_size = rep(1000,2)) {

  dfmat <- do.call(rbind, dflist)
  kde_object <- fit_kde_object(dfmat, h_band = h_band, grid_size = grid_size,
                               position = position)
  contour <- extract_contour(kde_object, alpha = alpha)
  return(contour)
}

#' Fit kernel density estimator (KDE) object
#'
#' @description
#' This function fits a kernel density estimator to a set of data points.
#' Another wrapper of \code{\link[ks:kde]{ks::kde}}.
#'
#' @details
#' This function is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param df data.frame with all the points
#' @param h_band optional argument for the bandwidth of the kde object. If NULL,
#' the optimal band would be selected through the \code{\link[ks:kde]{ks::kde}}
#' function. Default is NULL.
#' @param position Columns position of x/y pair. Default is 1:2
#' @param grid_size size of the grid which is going to be used for the
#' evaluation of kde object. Can be reduced to speed-up computation.
#'
#' @return KDE object fitted to the df data.frame.
#'
#' @examples
#' \dontrun{
#' set.seed(8192)
#' x <- 2^rnorm(100)
#' y <- rnorm(100)
#' dfmat <- cbind(x,y)
#'
#' kde_object <- fit_kde_object(dfmat)
#'}
fit_kde_object = function(df, h_band = NULL, position = 1:2,
                          grid_size = rep(1000,2)) {

  if (!is.null(h_band)) {
    h.mat <- diag(2)*h_band
    kde_obj <- ks::kde(df[ ,position], gridsize = grid_size, H = h.mat)
  } else {
    kde_obj <- ks::kde(df[ ,position], gridsize = grid_size)
  }
  return(kde_obj)
}


#' Selection of specific contour level from KDE object
#'
#' @description
#' This function extracts a specific level of contour from a kde object.
#'
#' This function only works for some levels that the kde object calculated
#' originally (associated with the constraints in the \code{alpha} parameter).
#'
#' @details
#' This function (renamed as \code{extract_countour}) is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param kde_obj kde object
#' @param alpha contour level which needs to be extracted. Scalar in the
#'   sequence from .01 to .99 by .01. A value of .95 gives a contour level
#'   associated with the cumulative prob of 95\% (and 95\% confidence interval) -
#'   which is technically 1-alpha.
#
#' @return List of contours at (100*alpha level) for the kde object - there is a
#' contour for each disconnected contour that forms the (100*alpha level)
#contour ' level.
#'
#' @examples
#' \dontrun{
#' set.seed(8192)
#'
#' x1 <- 2^rnorm(100)
#' y1 <- rnorm(100)
#' dfmat <- cbind(x1,y1)
#' kde_object <- ks::kde(dfmat)
#'
#' cont <- extract_contour(kde_object, .05)
#' }
#' @export
extract_contour <- function(kde_obj, alpha) {
  # contour lines actually does 1-alpha
  alpha <- round((1-alpha)*100)

  cont_level <- paste0(as.character(alpha), "%")
  cont <- with(kde_obj, contourLines(x = eval.points[[1]],y = eval.points[[2]],
                                     z = estimate,levels = cont[cont_level]))
  return(cont)
}


