#' EpiCompare: package overview
#' 
#' The goal of \code{EpiCompare} is to provide the epidemiology community with
#' easy-to-use tools to encourage comparing and assessing epidemics and
#' epidemiology models in a time-free manner. All tools attempt to adhere to
#' \code{tidyverse}/\code{\link[ggplot2]{ggplot2}} style to
#' enhance easy of use.
#'
#' Time free analysis allows for stronger comparison of epidemics and model
#' based simulations avoiding different scaling and shifts of time that mess up
#' time-based comparisons.
#'
#' To achieve this goal, the package contains:
#' \itemize{
#' \item \strong{Visualization tools} to visualize SIR epidemics and simulations
#' from SIR models in a time-free manner using \code{\link[ggtern]{ggtern}}’s
#' ternary plots and prediction bands. For agent-based SIR models we also
#' provide visualization tools to let the user easily explore how different
#' characteristics of the agents relate to different experiences in the
#' epidemic.
#'
#' \item \strong{General comparison} tools to compare epidemics and epidemic
#' models that have higher numbers of states (again in a time-free manner),
#' allowing for the user to examine the differences between models through
#' simulations, and if an epidemic is similar to a model through simulations and
#' prediction bands.
#' 
#' \item \strong{Conversion tools} to:
#' \itemize{
#' \item Convert and then compare models from \emph{standard epidemic packages}
#' like \code{\link[EpiModel]{EpiModel}}, \code{\link[pomp]{pomp}}, as well as
#' internal agent-based models, and epidemics in a common framework.
#' \item Convert \emph{agent-based information into aggregate} to compare in the
#' aggregate framework described above.
#' }
#' }
#' @importFrom rlang .data
#' 
#' @useDynLib EpiCompare
#' @importFrom Rcpp sourceCpp
#'
#' @docType package
#' @name EpiCompare
NULL
