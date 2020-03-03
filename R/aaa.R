# List of new stats
.newstat = c(sir_aggregate        = "StatSirAggregate",
             conf_band_kde        = "StatConfBandKDE",
             conf_band_delta_ball = "StatConfBandDeltaBall",
             conf_band_spherical  = "StatConfBandSpherical",
             conf_band_chull      = "StatConfBandConvexHull")
.newgeom = c(sir       = "GeomSirAggregate",
             conf_band = "GeomConfBand")

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
