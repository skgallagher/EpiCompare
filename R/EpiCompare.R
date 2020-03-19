#' EpiCompare: Ternary plot visualization for SIR curves (UPDATE)
#'
#' The goal of EpiCompare is to develop functional based visualization and
#' statistics that focus on use in ternary plots. (UPDATE)
#'
#' This package attempts to:
#' \itemize{
#'   \item \strong{Comparisons Model and Epidemic Comparisons}: We provide easy
#'   ways to compare multiple epidemics, epidemics vs models, or models verse
#'   models with ternary plots and confidence analysis
#'     \itemize{
#'       \item Ternary plots have been shown to help show time-independent like
#'       comparing R_0
#'       \item Added confidence band structure allows for examining variability
#'       within simulations from models cleanly (and without worrying about time
#'       variation)
#'         \itemize{
#'           \item Provides ways to moves beyond pointwise examination to
#'           uniform containment
#'         }
#'     }
#'   \item \strong{Agent (->/ vs) Aggregate}: Provide easy way to compare agents
#'   based data/modeling vs aggregation
#'     \itemize{
#'       \item Our tools provide easy ways to explore different grouping of
#'       agents and understanding how these groups different through aggregation
#'       visualizations (see Shannon’s thesis for rational of quick access to
#'       grouping abilities)
#'    }
#'   \item\strong{ggplot2/ggtern and tidyverse}: tools exist in the rapidly
#'   expanding tidyverse/ggplot2 paradigm, allowing for quick deployment and
#'   less cost in learning new assessment tools
#'  }
#' @import ggtern
#' @importFrom rlang .data
#'
#' @docType package
#' @name EpiCompare
NULL