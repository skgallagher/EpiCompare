#' Add a portable theme for SIR ternary plots
#' 
#' @return theme to add to ggtern plots
#' @param ... additional arguments to pass
#' @export
#' 
#' @examples 
#' library(ggplot2)
#' library(dplyr)
#' library(ggtern)
#'
#' EpiCompare::hagelloch_raw %>% filter(SEX %in% c("male", "female")) %>%
#'   ggplot(., aes(y = tI, z = tR, color = SEX)) +
#'   geom_aggregate() + coord_tern() +
#'   labs(x = "S", y = "I", z = "R",
#'        color = "Gender") +
#'        theme_sir()
theme_sir <- function(...){
  ggtern::theme_bw(...) +
    ggtern::theme_hideticks() +
    ggtern::theme_showarrows() 
}
