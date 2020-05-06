## SKG
## ADDING SIR THEME
## May 6, 2020


#' Add a portable theme for SIR ternary plots
#' 
#' @return theme to add to ggtern plots
#' @export
theme_sir <- function(){
  ggplot2::theme_bw() +
    ggtern::theme_hideticks() +
    ggtern::theme_showarrows() 
}