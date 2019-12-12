#' StatSirRaw
#'
#' @rdname StatSirRaw
#' @format NULL
#' @usage NULL
#' @export
StatSirRaw <- ggplot2::ggproto("StatSirRaw", ggplot2::Stat,
                   compute_group = function(data, scales,
                                            init_state = NULL){

                     # saving panel and group info
                     info_inner <- data[, c("PANEL", "group")] %>%
                       sapply(unique)

                     fortified_df <- fortify(data, c("y", "z"))
                     p <- ncol(fortified_df)
                     out <- UtoX_SIR(fortified_df[, (p-2):p])

                     out <- out %>% dplyr::mutate(PANEL = info_inner[1],
                                                  group = info_inner[2])
                     names(out)[names(out) %in% c("S","I","R")] <-
                       c("x","y", "z")
                     return(out)
                   },
                   required_aes = c("y", "z"))

#' StatSirFortified
#'
#' @rdname StatSirFortified
#' @format NULL
#' @usage NULL
#' @export
StatSirFortified <- ggplot2::ggproto("StatSirFortified", ggplot2::Stat,
                        compute_group = function(data, scales,
                                                 init_state = NULL){
                          #
                          # saving panel and group info
                          info_inner <- data[, c("PANEL", "group")] %>%
                            sapply(unique)

                          fortified_df <- data
                          idx <- sapply(c("init_state", "x", "y"),
                                        function(x) {
                                          which(names(fortified_df) == x)
                                        })
                          out <- UtoX_SIR(fortified_df, ind = idx)

                          out <- out %>% dplyr::mutate(PANEL = info_inner[1],
                                                       group = info_inner[2])
                          names(out)[names(out) %in% c("S","I","R")] <-
                            c("x","y", "z")

                          return(out)
                        },
                        required_aes = c("init_state","x","y"))

#' SIR path visuals
#'
#' @param mapping Set of aesthetic mappings created by
#' \code{\link[ggplot2:aes]{aes()}} or \code{\link[ggplot2:aes_]{aes_()}}.
#' If specified and \code{inherit.aes = TRUE} (the default), it is combined with
#' the default mapping at the top level of the plot. You must supply mapping if
#' there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If \code{NULL}, the default, the data is inherited from the plot data as
#' specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.
#'
#' A \code{data.frame}, will override the plot data.
#'
#' A function will be called with a single argument, the plot data. The return
#' value must be a data.frame, and will be used as the layer data. A function
#' can be created from a formula (e.g. ~ head(.x, 10)).
#'
#' @param geom Override the default connection between \code{stat_sir()} and
#' \code{\link[ggplot2:geom_path]{geom_path()}}.
#' @param stat Overrides state that is defined relative to \code{data_type}
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a
#' warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'  never includes, and \code{TRUE} always includes. It can also be a named
#'  logical vector to finely select the aesthetics to display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behaviour from the
#' @param data_type string. Currently can tell the stat to process the data like
#' "raw" individual data (formated like \code{\link{hagelloch_raw}}) or
#' "fortified" individual data (formated like \code{\link{hagelloch_agents}}
#' or the output of \code{\link{fortify.individuals_df}}).
#' @param ... Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}.
#' These are often aesthetics, used to set an aesthetic to a fixed value, like
#' \code{colour = "red"} or \code{size = 3}. They may also be parameters to the
#' paired geom/stat.
#'
#'@section Aesthetics:
#'
#' The required aesthetics for \code{stat_sir} depend on the \code{data_type}
#' parameter.
#'
#' If \code{data_type = "raw"} then \code{stat_sir} uses \code{stat_SirRaw()}
#' as the underlying stat and understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'  \item \strong{\code{y}} raw time when initially infected
#'  \item \strong{\code{z}} raw time when started recovery
#'  \item \code{group}
#'  }
#'
#' If \code{data_type = "fortified"} then \code{stat_sir} uses
#' \code{stat_SirFortified()} as the underlying stat and understands the
#' following aesthetics (required aesthetics are in bold):
#' \itemize{
#'  \item \strong{\code{init_state}} initial state of agent (0/1/2 for
#'  suspectable/infected/recovered)
#'  \item \strong{\code{x}} integer with maximum time suspectable for each agent
#'  \item \strong{\code{y}} integer with maximum time infected for each agent
#'  \item \code{group}
#'  }
#'
#'Learn more about setting these aesthetics in \code{vignette("ggplot2-specs")}.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(ggtern)
#'
#'
#' # geom_sir
#' timeternR::hagelloch_raw %>% filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   geom_sir(data_type = "raw") + coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#' timeternR::U_sims_tidy %>%
#'   ggplot() +
#'   geom_sir(aes(x = SMax, y = IMax, init_state = init_state,
#'                 group = sim), alpha = .1,
#'             data_type = "fortified") +
#'   coord_tern() +
#'   labs(x = "S", y = "I", z = "R")
#'
#' # stat_sir
#' timeternR::hagelloch_raw %>% filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   geom_path(stat = StatSirRaw) + coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#' timeternR::hagelloch_raw %>% dplyr::filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   stat_sir(geom = "path") +
#'   # note geom = "path" is the default (as is data_type = "raw")
#'   coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#' timeternR::U_sims_tidy %>%
#'   ggplot() +
#'   geom_path(aes(x = SMax, y = IMax, init_state = init_state, group = sim),
#'             alpha = .1, stat = timeternR::StatSirFortified) +
#'   coord_tern() +
#'   labs(x = "S", y = "I", z = "R")
#'
#' timeternR::U_sims_tidy %>%
#'   ggplot() +
#'   stat_sir(aes(x = SMax, y = IMax, init_state = init_state,
#'                 group = sim), alpha = .1,
#'             data_type = "fortified") +
#'   coord_tern() +
#'   labs(x = "S", y = "I", z = "R")
#'
stat_sir <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, data_type = c("raw", "fortified"), ...) {

  if (length(data_type) > 1){
    data_type <- data_type[1]
  }

  assertthat::assert_that(data_type %in% c("raw", "fortified"),
                          msg = paste("data_type needs to either be 'raw' or ",
                                      "'fortified'."))
  ggplot2::layer(
    stat = list(StatSirRaw, StatSirFortified)[which(c("raw", "fortified") == data_type)][[1]],
    data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @export
#' @rdname stat_sir
geom_sir <- function(mapping = NULL, data = NULL,
                     stat = c("SirRaw", "SirFortified")[which(c("raw", "fortified") %in% data_type)],
                     position = "identity",
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE,
                     data_type = c("raw", "fortified")) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSIR,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' GeomSIR
#'
#' @rdname GeomSIR
#' @format NULL
#' @usage NULL
#' @export
GeomSIR <- ggplot2::ggproto("GeomSIR", ggplot2::GeomPath)

