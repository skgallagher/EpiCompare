# List of new stats
.newstat = c(sir_aggregate        = "StatSirAggregate",
             conf_band_kde        = "StatPredBandKDE",
             conf_band_delta_ball = "StatPredBandDeltaBall",
             conf_band_spherical  = "StatPredBandSpherical",
             conf_band_chull      = "StatPredBandConvexHull")
.newgeom = c(sir       = "GeomSirAggregate",
             conf_band = "GeomPredBand")

#' import hidden function / variable from other package
#'
#' see ?`:::`
#'
#' @param pkg package to grab hidden item from
#' @param name hidden item (function / variable)
#' @return hidden object or function
#' @export
imports_hidden_from <- function(pkg, name){
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}


#' Updates approved layers of ggtern
#'
#' @param stat_names named vector of new stats.
#' @param geom_names named vector of new geoms
#'
#' @return NULL
#'
#' @import ggtern
#'
update_approved_layers <- function(stat_names = .newstat,
                                   geom_names = .newgeom){
  requireNamespace("ggtern", quietly = TRUE)
  approvestatupdate <- c(imports_hidden_from("ggtern",".approvedstat"),
                         stat_names)
  utils::assignInNamespace(".approvedstat", approvestatupdate,
                           pos = "package:ggtern")

  approvegeomupdate <- c(imports_hidden_from("ggtern",".approvedgeom"),
                         geom_names)
  utils::assignInNamespace(".approvedgeom", approvegeomupdate,
                           pos = "package:ggtern")

}

update_approved_layers()


#' logic to check if R is >= 4
#'
#' @return logical value (boolean)
#' @export
r_new_interface <- function() {
  as.numeric(R.version$major) >= 4
}


#' Explicitly draw plot (\code{ggtern} and \code{ggplot2} compatible)
#'
#' Makes sure both \code{\link[ggtern]{ggtern}} and 
#' \code{\link[ggplot2]{ggplot2}} objects 
#' display correctly with the \code{print.ggplot} function.
#'
#' @param x plot to display
#' @param newpage draw new (empty) page first?
#' @param vp viewport to draw plot in
#' @param ... other arguments not used by this method
#'
#' @return Invisibly returns the result of \code{ggplot_build()}, which is a 
#' list with components that contain the plot itself, the data, information 
#' about the scales, panels etc.
#' @export
print.ggplot <- function(x, newpage = is.null(vp), vp = NULL, ...){
  if(inherits(x$coordinates, "CoordTern")){
    ggtern:::print.ggplot(x, newpage = newpage, vp = vp, ...)
    
  } else {
    ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)
  }
}