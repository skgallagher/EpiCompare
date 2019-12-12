#' layout <- function(data, params) {
#'   data.frame(PANEL = c(1L, 2L), SCALE_X = c(1L,2L), SCALE_Y = c(1L,2L))
#' }
#'
#' mapping <- function(data, layout, params) {
#'   if (is.null(data) || nrow(data) == 0) {
#'     return(cbind(data, PANEL = integer(0)))
#'   }
#'   new_data <- data[]
#'
#'   rbind(
#'     cbind(data, PANEL = 1L),
#'     cbind(new_data, PANEL = 2L)
#'   )
#' }
#'
#' render <- function(panels, layout, x_scales, y_scales, ranges, coord, data,
#'                    theme, params) {
#'   # Place panels according to settings
#'   if (params$horizontal) {
#'     # Put panels in matrix and convert to a gtable
#'     panels <- matrix(panels, ncol = 2)
#'     panel_table <- gtable::gtable_matrix("layout", panels,
#'                                          widths = unit(c(1, 1), "null"),
#'                                          heights = unit(1, "null"), clip = "on")
#'     # Add spacing according to theme
#'     panel_spacing <- if (is.null(theme$panel.spacing.x)) {
#'       theme$panel.spacing
#'     } else {
#'       theme$panel.spacing.x
#'     }
#'     panel_table <- gtable::gtable_add_col_space(panel_table, panel_spacing)
#'   } else {
#'     panels <- matrix(panels, ncol = 1)
#'     panel_table <- gtable::gtable_matrix("layout", panels,
#'                                          widths = unit(1, "null"),
#'                                          heights = unit(c(1, 1), "null"),
#'                                          clip = "on")
#'     panel_spacing <- if (is.null(theme$panel.spacing.y)) {
#'       theme$panel.spacing
#'     } else {
#'       theme$panel.spacing.y
#'     }
#'     panel_table <- gtable::gtable_add_row_space(panel_table, panel_spacing)
#'   }
#'   # Name panel grobs so they can be found later
#'   panel_table$layout$name <- paste0("panel-", c(1, 2))
#'
#'   # Construct the axes
#'   axes <- ggplot2::render_axes(ranges[1], ranges[1], coord, theme,
#'                       transpose = TRUE)
#'
#'   # Add axes around each panel
#'   panel_pos_h <- panel_cols(panel_table)$l
#'   panel_pos_v <- panel_rows(panel_table)$t
#'   axis_width_l <- unit(grid::convertWidth(
#'     grid::grobWidth(axes$y$left[[1]]), "cm", TRUE), "cm")
#'   axis_width_r <- unit(grid::convertWidth(
#'     grid::grobWidth(axes$y$right[[1]]), "cm", TRUE), "cm")
#'   ## We do it reverse so we don't change the position of panels when we add axes
#'   for (i in rev(panel_pos_h)) {
#'     panel_table <- gtable::gtable_add_cols(panel_table, axis_width_r, i)
#'     panel_table <- gtable::gtable_add_grob(panel_table,
#'                                            rep(axes$y$right, length(panel_pos_v)), t = panel_pos_v, l = i + 1,
#'                                            clip = "off")
#'     panel_table <- gtable::gtable_add_cols(panel_table, axis_width_l, i - 1)
#'     panel_table <- gtable::gtable_add_grob(panel_table,
#'                                            rep(axes$y$left, length(panel_pos_v)), t = panel_pos_v, l = i,
#'                                            clip = "off")
#'   }
#'   ## Recalculate as gtable has changed
#'   panel_pos_h <- panel_cols(panel_table)$l
#'   panel_pos_v <- panel_rows(panel_table)$t
#'   axis_height_t <- unit(grid::convertHeight(
#'     grid::grobHeight(axes$x$top[[1]]), "cm", TRUE), "cm")
#'   axis_height_b <- unit(grid::convertHeight(
#'     grid::grobHeight(axes$x$bottom[[1]]), "cm", TRUE), "cm")
#'   for (i in rev(panel_pos_v)) {
#'     panel_table <- gtable::gtable_add_rows(panel_table, axis_height_b, i)
#'     panel_table <- gtable::gtable_add_grob(panel_table,
#'                                            rep(axes$x$bottom, length(panel_pos_h)), t = i + 1, l = panel_pos_h,
#'                                            clip = "off")
#'     panel_table <- gtable::gtable_add_rows(panel_table, axis_height_t, i - 1)
#'     panel_table <- gtable::gtable_add_grob(panel_table,
#'                                            rep(axes$x$top, length(panel_pos_h)), t = i, l = panel_pos_h,
#'                                            clip = "off")
#'   }
#'   panel_table
#' }
#'
#'
#' # Constructor: shrink is required to govern whether scales are trained on
#' # Stat-transformed data or not.
#' #' Title
#' #'
#' #' @param x
#' #' @param y
#' #' @param z
#' #' @param xyz
#' #' @param horizontal
#' #' @param xlim,ylim,zlim Specific zoom ranges for each axis. If present they
#' #' will override x, y, z, and/or xyz.
#' #' @param show.area boolean if we should highlight the area zoomed in on
#' #' @param shrink
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' facet_duplicate <- function(x, y, z, xyz, horizontal = F,
#'                             xlim = NULL, ylim = NULL, zlim = NULL,
#'                             show.area = TRUE, shrink = TRUE) {
#'   x <- if (missing(x)) if (missing(xyz)) NULL else enquo(xyz) else enquo(x)
#'   y <- if (missing(y)) if (missing(xyz)) NULL else enquo(xyz) else enquo(y)
#'   z <- if (missing(z)) if (missing(xyz)) NULL else enquo(xyz) else enquo(z)
#'
#'   if (is.null(x) && is.null(y) && is.null(z) &&
#'       is.null(xlim) && is.null(ylim) && is.null(zlim)) {
#'     stop('Either x- or y-zoom must be given', call. = FALSE)
#'   }
#'
#'   if (!is.null(xlim)) x <- NULL
#'   if (!is.null(ylim)) y <- NULL
#'   if (!is.null(zlim)) z <- NULL
#'
#'   ggproto(NULL, FacetDuplicate,
#'           shrink = shrink,
#'           params = list(
#'             horizontal = horizontal,
#'             x = x, y = y, z = z,
#'             xlim = xlim, ylim = ylim, zlim = zlim,
#'             show.area = show.area, shrink = shrink
#'           )
#'   )
#' }
#'
#' FacetDuplicate <- ggproto("FacetDuplicate", ggplot2::Facet,
#'                           compute_layout = layout,
#'                           map_data = mapping,
#'                           draw_panels = render
#' )
#'
#'
#' a <- hagelloch_raw %>%
#'   dplyr::filter(SEX %in% c("male", "female")) %>%
#'   ggplot(aes(y = tI, z = tR, color = SEX)) +
#'   stat_sir() +
#'   coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#' a + facet_duplicate(xlim = c(0,50), horizontal = F, show.area = TRUE)
