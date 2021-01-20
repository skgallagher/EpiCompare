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
    #ggtern:::print.ggplot(x, newpage = newpage, vp = vp, ...)
    print.ggplot <- imports_hidden_from("ggtern", "print.ggplot")
    print.ggplot(x, newpage = newpage, vp = vp, ...)
  } else {
    #ggplot2:::print.ggplot(x, newpage = newpage, vp = vp, ...)
    print.ggplot <- imports_hidden_from("ggplot2", "print.ggplot")
    print.ggplot(x, newpage = newpage, vp = vp, ...)
  }
}


#' Arrange multiple grobs on a page (\code{ggtern} and \code{ggplot2} compatible)
#' 
#' @inheritParams gridExtra::arrangeGrob
#' @rdname arrangeGrob
#' @export
arrangeGrob = function (..., grobs = list(...), layout_matrix, vp = NULL, name = "arrange", 
                        as.table = TRUE, respect = FALSE, clip = "off", nrow = NULL, 
                        ncol = NULL, widths = NULL, heights = NULL, top = NULL, bottom = NULL, 
                        left = NULL, right = NULL, padding = grid::unit(0.5, "line")) ## BL (grid::)
{
  n <- length(grobs)
  if (!is.null(ncol) && !is.null(widths)) {
    stopifnot(length(widths) == ncol)
  }
  if (!is.null(nrow) && !is.null(heights)) {
    stopifnot(length(heights) == nrow)
  }
  if (is.null(ncol) && !is.null(widths)) {
    ncol <- length(widths)
  }
  if (is.null(nrow) && !is.null(heights)) {
    nrow <- length(heights)
  }
  if (is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(n/ncol)
  }
  if (is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(n/nrow)
  }
  stopifnot(nrow * ncol >= n)
  if (is.null(nrow) && is.null(ncol) && is.null(widths) && 
      is.null(heights)) {
    nm <- grDevices::n2mfrow(n)
    nrow = nm[1]
    ncol = nm[2]
  }
  inherit.ggplot <- unlist(lapply(grobs, inherits, what = "ggplot"))
  inherit.ggtern <- unlist(lapply(grobs, function(ggplot){inherits(ggplot$coordinates, "CoordTern")})) ##BL
  inherit.trellis <- unlist(lapply(grobs, inherits, what = "trellis"))
  if (any(inherit.ggplot)) {
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    toconv <- which(inherit.ggplot)
    toconv_ggtern <- which(inherit.ggtern) ## BL
    toconv_ggplot <- toconv[!(toconv %in% toconv_ggtern)] ## BL
    #grobs[toconv] <- lapply(grobs[toconv], ggplot2::ggplotGrob)
    #grobs[toconv] <- lapply(grobs[toconv], ggplotGrob) ##NH
    grobs[toconv_ggtern] <- lapply(grobs[toconv_ggtern], ggtern::ggplotGrob) ##BL
    grobs[toconv_ggplot] <- lapply(grobs[toconv_ggplot], ggplot2::ggplotGrob) ##BL
    
  }
  if (any(inherit.trellis)) {
    stopifnot(requireNamespace("lattice", quietly = TRUE))
    toconv <- which(inherit.trellis)
    latticeGrob <- imports_hidden_from("gridExtra", "latticeGrob")
    grobs[toconv] <- lapply(grobs[toconv], latticeGrob)
  }
  if (missing(layout_matrix)) {
    positions <- expand.grid(t = seq_len(nrow), l = seq_len(ncol))
    positions$b <- positions$t
    positions$r <- positions$l
    if (as.table) 
      positions <- positions[order(positions$t), ]
    positions <- positions[seq_along(grobs), ]
  }
  else {
    cells <- sort(unique(as.vector(layout_matrix)))
    range_cell <- function(ii) {
      ind <- which(layout_matrix == ii, arr.ind = TRUE)
      c(l = min(ind[, "col"]), r = max(ind[, "col"]), t = min(ind[, 
                                                                  "row"]), b = max(ind[, "row"]))
    }
    positions <- data.frame(do.call(rbind, lapply(cells, 
                                                  range_cell)))
    ncol <- max(positions$r)
    nrow <- max(positions$b)
  }
  if (is.null(widths)) 
    widths <- grid::unit(rep(1, ncol), "null") ## BL (grid::)
  if (is.null(heights)) 
    heights <- grid::unit(rep(1, nrow), "null") ## BL (grid::)
  if (!grid::is.unit(widths)) ## BL (grid::)
    widths <- grid::unit(widths, "null") ## BL (grid::)
  if (!grid::is.unit(heights)) ## BL (grid::)
    heights <- grid::unit(heights, "null") ## BL (grid::)
  gt <- gtable::gtable(name = name, respect = respect, heights = heights, ##NH
               widths = widths, vp = vp) ## BL (gtable::)
  gt <- gtable::gtable_add_grob(gt, grobs, t = positions$t, b = positions$b, 
                        l = positions$l, r = positions$r, z = seq_along(grobs), 
                        clip = clip) ## BL (gtable::)
  if (is.character(top)) {
    top <- grid::textGrob(top) ## BL (grid::)
  }
  if (grid::is.grob(top)) { ## BL (grid::)
    h <- grid::grobHeight(top) + padding ## BL (grid::)
    gt <- gtable::gtable_add_rows(gt, heights = h, 0)  ## BL (gtable::)
    gt <- gtable::gtable_add_grob(gt, top, t = 1, l = 1, r = ncol(gt), 
                          z = Inf, clip = clip) ## BL (gtable::)
  }
  if (is.character(bottom)) {
    bottom <- grid::textGrob(bottom) ## BL (grid::)
  }
  if (grid::is.grob(bottom)) { ## BL (grid::)
    h <- grid::grobHeight(bottom) + padding ## BL (grid::)
    gt <- gtable::gtable_add_rows(gt, heights = h, -1) ## BL (gtable::)
    gt <- gtable::gtable_add_grob(gt, bottom, t = nrow(gt), l = 1, 
                          r = ncol(gt), z = Inf, clip = clip) ## BL (gtable::)
  }
  if (is.character(left)) {
    left <- grid::textGrob(left, rot = 90) ## BL (grid::)
  }
  if (grid::is.grob(left)) { ## BL (grid::)
    w <- grid::grobWidth(left) + padding ## BL (grid::)
    gt <- gtable::gtable_add_cols(gt, widths = w, 0) ## BL (gtable::)
    gt <- gtable::gtable_add_grob(gt, left, t = 1, b = nrow(gt), 
                          l = 1, r = 1, z = Inf, clip = clip) ## BL (gtable::)
  }
  if (is.character(right)) {
    right <- grid::textGrob(right, rot = -90) ## BL (grid::)
  }
  if (grid::is.grob(right)) { ## BL (grid::)
    w <- grid::grobWidth(right) + padding ## BL (grid::)
    gt <- gtable::gtable_add_cols(gt, widths = w, -1) ## BL (gtable::)
    gt <- gtable::gtable_add_grob(gt, right, t = 1, b = nrow(gt), 
                          l = ncol(gt), r = ncol(gt), z = Inf, clip = clip) ## BL (gtable::)
  }
  gt
}

#' @inheritParams gridExtra::arrangeGrob
#' @inheritParams gridExtra::grid.arrange
#' @rdname arrangeGrob 
#' @aliases grid.arrange
#' @export
grid.arrange = function (..., newpage = TRUE) {
  if (newpage) 
    grid::grid.newpage() # BL (grid::)
  g <- arrangeGrob(...)
  grid::grid.draw(g) # BL (grid::)
  invisible(g)
}