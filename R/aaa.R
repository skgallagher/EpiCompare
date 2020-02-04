#' import hidden function / variable from other package
#'
#' see ?`:::`
#'
#' @param pkg package to grab hidden item from
#' @param name hidden item (function / variable)
#' @return hidden object or function
#' @export
importsHiddenFrom <- function(pkg, name){
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
#' @examples
#' .newstat = c(sir_raw       = "StatSirRaw",
#'              sir_fortified = "StatSirFortified")
#' .newgeom = NULL
#' #update_approved_layers(stat_name = .newstat, geom_name = .newgeom)
update_approved_layers <- function(stat_names = NULL, geom_names = NULL){
  requireNamespace("ggtern", quietly = TRUE)
  approvestatupdate <- c(importsHiddenFrom("ggtern",".approvedstat"),
                         stat_names)
  utils::assignInNamespace(".approvedstat", approvestatupdate,
                           pos = "package:ggtern")

  approvegeomupdate <- c(importsHiddenFrom("ggtern",".approvedgeom"),
                         geom_names)
  utils::assignInNamespace(".approvedgeom", approvegeomupdate,
                           pos = "package:ggtern")

}

# List of new stats
.newstat = c(sir_raw              = "StatSirRaw",
             sir_fortified        = "StatSirFortified",
             conf_band_kde        = "StatConfBandKDE",
             conf_band_delta_ball = "StatConfBandDeltaBall",
             conf_band_spherical =  "StatConfBandSpherical")
.newgeom = c(sir       = "GeomSIR",
             conf_band = "GeomConfBand")
update_approved_layers(stat_name = .newstat, geom_name = .newgeom)
