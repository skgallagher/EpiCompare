#' Measles in Hagelloch, Germany, 1861
#'
#' These data comprise of 188 cases of measles among children in the German
#' city of Hagelloch, 1861. The data were
#' originally collected by Dr. Albert Pfeilsticker (1863) and augmented and
#' re-analysed by Dr. Heike Oesterle (1992). This data comes from the
#' \code{\link[outbreaks]{outbreaks}}.
#'
#' @format A data frame with 188 rows and 12 columns
#' \describe{
#'   \item{case_ID}{Case ID number}
#'   \item{infector}{Number of patient who is the putative source of infection}
#'   \item{date_of_prodrome}{\code{Date} of onset of prodromal symptoms}
#'   \item{date_of_rash}{\code{Date} of onset of rash}
#'   \item{date_of_death}{\code{Date} of death (\code{NA} implies recovered)}
#'   \item{age}{Age in years (fractions ignored)}
#'   \item{gender}{Gender of the individual (factor: f, m)}
#'   \item{family_ID}{Family ID number}
#'   \item{class}{School class (factor: 0, preschool; 1, 1st class; 2, 2nd class )}
#'   \item{complications}{Complications (factor: no, yes)}
#'   \item{x_loc}{x coordinate of house (in metres). Scaling in metres
#'   is obtained by multiplying the original coordinates by 2.5 (see details
#'   in Neal and Roberts (2004))}
#'   \item{y_loc}{y coordinate of house (in metres). See \code{x_loc} above.}
#' }
#'
#' @source \url{https://rdrr.io/cran/outbreaks/man/measles_hagelloch_1861.html}
#'
#' @examples
#' ## show first few cases
#' head(hagelloch_raw)
"hagelloch_raw"


#' Measles in Hagelloch, Germany, 1861 (SIR format)
#'
#' This data is the SIR formulation of 188 cases of measles among children in
#' the German city of Hagelloch, 1861, and is a reformulation of the data in
#' \code{\link{hagelloch_raw}}. Each row is asssociated with 1 day (where
#' \code{t} tells the number of days after the start of the outbreak), and
#' the rest of the columns report the number of individuals suspectable,
#' infected, and in recovery.
#'
#' Note that \eqn{s_t + i_t + r_t = 188} for each row \eqn{t in 0, \dots, 94}.
#'
#' @format A data frame with 95 rows and 4 columns
#' \describe{
#'   \item{t}{time since outbreak, \eqn{t = 0, \dots, T=94}}
#'   \item{S}{Number of individuals suspectable}
#'   \item{I}{Number of individuals infected}
#'   \item{R}{Number of individuals in recovery}
#' }
#'
#' @examples
#' ## show first few cases
#' head(hagelloch_sir)
#' assertthat::assert_that(all(apply(hagelloch_sir[,-1], 1, sum) == 188))
"hagelloch_sir"

#' Measles in Hagelloch, Germany, 1861 (agent format)
#'
#' This data is the agent formulation of 188 cases of measles among children in
#' the German city of Hagelloch, 1861, and is a summarization of the data in
#' \code{\link{hagelloch_raw}}. Each column is asssociated with 1 agent and
#' is a "sufficient" statistic for each agent's infection.  Each agent's
#' infection is uniquely identified by an initial state, max time before
#' infection (or time T), and max time before recovery (or time T). Note time T
#' is the final recorded time in the SIR model (in this case T = 94).
#'
#' @format A matrix with 3 row and 188 columns. Below are the description of
#' each row.
#' \describe{
#'   \item{init_state}{Initial state for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was suspectable (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#'
#' @examples
#' ## show first few cases
#' head(hagelloch_agents)
"hagelloch_agents"
