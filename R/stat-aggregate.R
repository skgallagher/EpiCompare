#' StatSirAggregate
#'
#' @rdname StatAgregate
#' @format NULL
#' @usage NULL
#' @export
StatSirAggregate <- ggplot2::ggproto("StatSirAggregate",
                      ggplot2::Stat,
                      setup_data = function(data, params){
                        # This is so ggplot does NOT remove NA rows
                         inner_data <- data
                         inner_data$missing_y <- data$y
                         inner_data$missing_z <- data$z
                         inner_data$y <- 100
                         inner_data$z <- 100

                         return(inner_data)
                       },
                      compute_group = function(data, scales){
                         # saving panel and group info
                         info_inner <- data[, c("PANEL", "group")] %>%
                           sapply(unique)

                         out <- agents_to_aggregate(data,
                                   states = c(.data$missing_y,
                                              .data$missing_z))

                         out <- out %>% dplyr::mutate(PANEL = info_inner[1],
                                                      piece = info_inner[2],
                                                      group = info_inner[2])
                         names(out)[names(out) %in% c("X0","X1","X2")] <-
                           c("x", "y", "z")
                         return(out)
                       },
                      required_aes = c("y", "z"))


#'aggregate SIR path visuals from agent data
#'
#'@param mapping Set of aesthetic mappings created by
#'  \code{\link[ggplot2:aes]{aes()}} or \code{\link[ggplot2:aes_]{aes_()}}. If
#'  specified and \code{inherit.aes = TRUE} (the default), it is combined with
#'  the default mapping at the top level of the plot. You must supply mapping if
#'  there is no plot mapping.
#'@param data The data to be displayed in this layer. There are three options:
#'
#'  If \code{NULL}, the default, the data is inherited from the plot data as
#'  specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.
#'
#'  A \code{data.frame}, will override the plot data.
#'
#'  A function will be called with a single argument, the plot data. The return
#'  value must be a data.frame, and will be used as the layer data. A function
#'  can be created from a formula (e.g. ~ head(.x, 10)).
#'
#'@param geom Override the default connection between \code{stat_aggregate()}
#'  and \code{\link[ggplot2:geom_path]{geom_path()}}.
#'@param stat Override the default connection between \code{geom_aggregate()}
#'  and \code{StatSirAggregate} stat to process raw
#'  data
#'@param position Position adjustment, either as a string, or the result of a
#'  call to a position adjustment function
#'@param na.rm when using the default Stat or Geom this parameter doesn't matter
#'  as \code{NA}s are seen as encoders of information and NOT removed.
#'  \strong{See details for more on this.}
#'
#'  If not using either of these (which means this geom/stat doesn't need to be
#'  used), then if \code{FALSE}, the default, missing values are removed with a
#'  warning. If \code{TRUE}, missing values are silently removed.
#'@param show.legend logical. Should this layer be included in the legends?
#'  \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'  never includes, and \code{TRUE} always includes. It can also be a named
#'  logical vector to finely select the aesthetics to display.
#'@param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'  than combining with them. This is most useful for helper functions that
#'  define both data and aesthetics and shouldn't inherit behaviour from the
#'@param ... Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}.
#'  These are often aesthetics, used to set an aesthetic to a fixed value, like
#'  \code{colour = "red"} or \code{size = 3}. They may also be parameters to the
#'  paired geom/stat.
#'
#' @section Aesthetics:
#'
#'  \itemize{ \item \strong{\code{y}} raw time when initially infected (or more
#'  generally when the agent enters the second state)
#'  \item \strong{\code{z}} raw time when started recovery (or more generally
#'  when the agent enters the third state)
#'  \item \code{group} \item ... }
#'
#'  Learn more about setting these aesthetics in
#'  \code{vignette("ggplot2-specs")}.
#'
#' @details
#' This visual leverage the function \code{\link{agents_to_aggregate}}
#' underneath. This function converts individual agents' information on when the
#' agent transitions between 3 different states (ordered), to create a temporal
#' tally/aggregation of how many agents are in what state at each integer time
#' point. In this visualization tool, the user provides \code{y} and \code{z} to
#' present the agent enters the second and third state (respectively).
#'
#' As mentioned in parameter details, \code{agents_to_aggregate} encodes
#' \code{NA} to capture information, and as such, the default
#'
#' @export
#'
#'@examples
#' library(ggplot2)
#' library(dplyr)
#' library(ggtern); EpiCompare:::update_approved_layers()
#' #                ^ this generally need not be done
#'
#'
#' # geom_aggregate
#' EpiCompare::hagelloch_raw %>% filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   geom_aggregate() + coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#'
#' # stat_aggregate
#' EpiCompare::hagelloch_raw %>% filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   geom_path(stat = StatSirAggregate) + coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#' EpiCompare::hagelloch_raw %>% dplyr::filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   stat_aggregate(geom = "path") +
#'   # note geom = "path" is the default
#'   coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#'
stat_aggregate <- function(mapping = NULL, data = NULL, geom = "path",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatSirAggregate,
    data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
