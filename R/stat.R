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

                     fortified_df <- fortify_agents(data, c("y", "z"))
                     p <- ncol(fortified_df)
                     out <- UtoX_SIR(fortified_df[, (p-2):p])

                     out <- out %>% dplyr::mutate(PANEL = info_inner[1],
                                                  group = info_inner[2])
                     names(out)[names(out) %in% c("S","I", "R")] <-
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

                              # saving panel and group info
                              info_inner <- data[, c("PANEL", "group")] %>%
                                sapply(unique)

                              fortified_df <- data
                              idx <- sapply(c("init_state", "y", "z"),
                                            function(x) {
                                              which(names(fortified_df) == x)
                                            })
                              out <- UtoX_SIR(fortified_df, ind = idx)

                              out <- out %>% dplyr::mutate(PANEL = info_inner[1],
                                                           group = info_inner[2])
                              names(out)[names(out) %in% c("S","I", "R")] <-
                                c("x","y", "z")
                              return(out)
                            },
                            required_aes = c("init_state", "y","z"))

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
#' @param geom Override the default connection between stat_sir() and
#' \code{\link[ggplot2:geom_path]{geom_path()}}.
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
#' @param data_type
#' default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.
#' @param ... Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}.
#' These are often aesthetics, used to set an aesthetic to a fixed value, like
#' \code{colour = "red"} or \code{size = 3}. They may also be parameters to the
#' paired geom/stat.
#'
#'@eval ggplot2:::rd_aesthetics("stat", "SirRaw")
#'@eval ggplot2:::rd_aesthetics("stat", "SirFortified")
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#'
#' timeternR::hagelloch_raw %>%
#'   dplyr::filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'     geom_path(stat = StatSirRaw) + ggtern::coord_tern() +
#'     labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
#'
#' timeternR::hagelloch_raw %>%
#'   dplyr::filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'     stat_sir(geom = "path") + # note geom = "path" is the default
#'     ggtern::coord_tern() +
#'     labs(x = "S", y = "I", z = "R",
#'        color = "Gender")
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



#
#
# library(ggtern)
# library(tidyverse)
# # using transformed data -----------------------------------
# ggtern(timeternR::hagelloch_sir, aes(x = S, y = I, z = R)) +
#   geom_path()
#
# # or, cleaner:
# ggplot(timeternR::hagelloch_sir, aes(x = S, y = I, z = R)) +
#   geom_path() + coord_tern()
# #
# #
# # # our stats -------------------------------------------------
# timeternR::hagelloch_raw %>% filter(SEX %in% c("male", "female")) %>%
# ggplot(., aes(y = tI, z = tR, color = SEX)) +
#   geom_path(stat = StatSirRaw) + coord_tern() +
#   labs(x = "S", y = "I", z = "R",
#        color = "Gender")
#
# timeternR::hagelloch_raw %>% dplyr::filter(SEX %in% c("male", "female")) %>%
#   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#   stat_sir(geom = "path",data_type = "raw") + # note geom = "path" is the default
#   coord_tern() +
#   labs(x = "S", y = "I", z = "R",
#        color = "Gender")
#
#
# my_sims <- timeternR::U_sims
#
# dimnames(my_sims) <- list(paste(1:50),
#                           c("init state", "SMax", "IMax"),
#                           paste(1:188))
# tidy_sims <- my_sims %>% as.data.frame.table %>% spread(Var2, Freq) %>%
#   rename(Sim = "Var1",
#          individual = "Var3")
#
#
# tidy_sims %>%
#   ggplot() +
#   geom_path(aes(y = SMax, z = IMax, init_state = `init state`,
#                group = Sim), alpha = .1,
#             stat = StatSirFortified) +
#   coord_tern() +
#   labs(x = "S", y = "I", z = "R")

