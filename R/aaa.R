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
  approvestatupdate <- c(ggtern:::.approvedstat,
                         stat_names)
  utils::assignInNamespace(".approvedstat", approvestatupdate,
                           pos = "package:ggtern")

  approvegeomupdate <- c(ggtern:::.approvedgeom,
                         geom_names)
  utils::assignInNamespace(".approvedgeom", approvegeomupdate,
                           pos = "package:ggtern")

}

# List of new stats
.newstat = c(sir_raw       = "StatSirRaw",
             sir_fortified = "StatSirFortified")
.newgeom = c(sir = "GeomSIR")
update_approved_layers(stat_name = .newstat, geom_name = .newgeom)
