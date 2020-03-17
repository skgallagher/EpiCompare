#' @export
#' @rdname stat_aggregate
geom_aggregate <- function(mapping = NULL, data = NULL,
                           stat = StatSirAggregate,
                           position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSirAggregate,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomSirAggregate
#'
#' @rdname GeomSirAggregate
#' @format NULL
#' @usage NULL
#' @export
GeomSirAggregate <- ggplot2::ggproto("GeomSirAggregate",
                                     ggplot2::GeomPath)
