

#' Example of SIRS model generated  from the \code{pomp} package.
#' 
#' @description  This is a deterministic SIRS model where 
#' the infection parameter \eqn{\beta = 0.2} and the recovery from infection parameter is
#' parameter is \eqn{\gamma = 0.1}.  The recovery to susceptible parameter is \eqn{\eta = .05}.  The initial number of susceptible is \eqn{S(0) = 990}
#' and the initial number of infectious is \eqn{I(0)=10}.  There are 100 steps.
#' So in summary, \eqn{S^\prime = -\beta SI / N + \eta R}, \eqn{I^\prime = \beta S I /N -\gamma I}
#' and \eqn{R^\prime = \gamma I - \eta R}.
#' @format The object is a data frame.
#' @examples 
#' library(ggplot2)
#' library(tidyr)
#' head(sirs_data)
#' if(EpiCompare:::tidyr_new_interface()){
#'   ggplot(data = sirs_data %>%
#'          tidyr::pivot_longer(-c(time, .id)),
#'           aes(x = time, y = value, group = name, col = name)) + geom_line()
#' } else {
#'   ggplot(data = sirs_data %>%
#'          tidyr::gather("name", "value", -time, -.id),
#'           aes(x = time, y = value, group = name, col = name)) + geom_line()
#' }
"sirs_data"


#' Example SIS model  of class `dcm` from the \code{EpiModel} package.
#' 
#' @description  This is a deterministic SIS model where 
#' the infection parameter \eqn{\beta = 0.2} and the recovery/re-susceptibility
#' parameter is \eqn{\nu = 0.05}.  The initial number of susceptible is \eqn{S(0) = 499}
#' and the initial number of infectious is \eqn{I(0)=1}.  There are 100 steps.
#' For more details, see <<http://statnet.org/tut/BasicDCMs.html#sis_model_with_sensitivity_analyses>>.
#' @format The object is a 'dcm', which is output from the EpiModel R package.
#' @examples 
#' library(EpiModel)
#' data(sis_data)
#' plot(sis_data)
"sis_data"


#' Example SIS model converted to aggregate format
#' 
#' @description  This is a deterministic SIS model where 
#' the infection parameter \eqn{\beta = 0.2} and the recovery/re-susceptibility
#' parameter is \eqn{\nu = 0.05}.  The initial number of susceptible is \eqn{S(0) = 499}
#' and the initial number of infectious is \eqn{I(0)=1}.  There are 100 steps.
#' For more details, see <<http://statnet.org/tut/BasicDCMs.html#sis_model_with_sensitivity_analyses>>.  
#' We then use \code{fortify_aggregate()} to convert the data to output used with \code{EpiCompare} functions.
#' @format The object has the following columns
#' \describe{
#' \item{t}{time}
#' \item{orig_t}{original t in simulation}
#' \item{sim}{simulation number}
#' \item{X0}{number of Susceptible}
#' \item{X1}{number of Infectious}
#' }
#' @examples 
#' library(EpiModel)
#' data(sis_data_f)
#' head(sis_data_f)
"sis_data_f"








#' Example SIR simulation output of class ' pomp' from the \code{pomp} package
#'
#' @description The data is an example of simulation output from functions from the \href{https://kingaa.github.io/pomp/}{pomp package by King et al}.  The code for how this data was generated can be found in the \code{data-raw} folder.  We also make available the other possible output formats from pomp.  See \link{pomp_df} and \link{pomp_arr}
#'
#' @format The object is a 'pompList' which contains essentially all information from a \code{pomp} simulation.
#' @examples
#' data("pomp_pomp")
#' str(pomp_pomp)
"pomp_pomp"



#' Example SIR simulation output of class 'array' from the \code{pomp} package
#'
#' @description The data is an example of simulation output from functions from the \href{https://kingaa.github.io/pomp/}{pomp package by King et al}.  The code for how this data was generated can be found in the \code{data-raw} folder.  We also make available the other possible output formats from pomp.  See \link{pomp_df} and \link{pomp_pomp}
#'
#' @format A list of 2 entries where the first and second entries are both arrays.  The first entry has dimension 4x 100 x10 and the dimension names are respectively variable ("S", "I", "R", "H"), rep, and time.  The second entry has dimension 1 x 100 x 101 where the dimension names are respectively variable ("cases"), 'rep', and 'time'.  Thus entry ijk in either array corresponds to the number of the variable i at simulation/rep j and time k.
#' @examples
#' data("pomp_arr")
#' names(pomp_arr)
#' dimnames(pomp_arr[[1]])
"pomp_arr"

#' Example SIR simulation output of class ' data.frame' from the \code{pomp} package
#'
#' @description The data is an example of simulation output from functions from the \href{https://kingaa.github.io/pomp/}{pomp package by King et al}.  The code for how this data was generated can be found in the \code{data-raw} folder.  We also make available the other possible output formats from pomp.  See \link{pomp_arr} and \link{pomp_pomp}
#'
#' @format A data frame of dimension 10100 x 7 where the columns are
#' \describe{
#' \item{time}{an integer value between 0 and 100}
#'\item{.id}{the simulation ID number}
#' \item{S}{The number of Susceptible at given time and simulation ID}
#' \item{I}{The number of Infectious at given time and simulation ID}
#' \item{R}{The number of Recovered at given time and simulation ID}
#' \item{H}{A helper variable}
#' \item{cases}{}
#' }
#' @examples
#' data("pomp_df")
#' head(pomp_df)
#' library(ggplot2)
#' ggplot(pomp_df) + geom_line(aes(x = time, y = I, group = .id,
#' col = as.numeric(.id)))
"pomp_df"



#' Measles in Hagelloch, Germany, 1861
#'
#' @description
#' The data comprise of 188 cases of measles among children in the German
#' city of Hagelloch, 1861. The data were
#' originally collected by Dr. Albert Pfeilsticker (1863) and augmented and
#' re-analysed by Dr. Heike Oesterle (1992). This data comes from the package
#' \code{\link[outbreaks:measles_hagelloch_1861]{outbreaks}}.
#'
#' \code{hallegoch_raw} contains the exact data as desired in this
#' document. \code{hallegoch_raw2} has a few rows that have no recorded times
#' \code{tR} and \code{tI} which the user can image means the individual
#' never reached the Recovery or Infected state (note if \code{tI} is \code{NA},
#' then so is \code{tR}). Additionally, \code{hallegoch_raw2} has 2 individuals
#' that are in recovery at the start of the observational approach, and as such
#' have \code{tR}, \code{tI}, \code{tPRE}, and \code{tERU} that are negative.
#'
#'
#'
#' @format A data frame with 188 rows and 12 columns
#' \describe{
#'   \item{PN}{Patient number (Case ID) per individual}
#'   \item{NAME}{Patient's last name}
#'   \item{FN}{Family number (individuals with matching \code{FN} are in the
#'   same family)}
#'   \item{HN}{Household number ((individuals with matching \code{HN} are in the
#'   same household)}
#'   \item{AGE}{Age in years (fractions ignored)}
#'   \item{SEX}{Gender of the individual (factor: female; male)}
#'   \item{PRO}{\code{Date} of onset of prodromal symptoms}
#'   \item{ERU}{\code{Date} of onset of rash (not 100 sure of this one)}
#'   \item{CL}{School class (factor: preschool; 1st class; 2nd class )}
#'   \item{DEAD}{\code{Date} of death (with missings)}
#'   \item{IFTO}{number of patient who is the putative source of infection (0 = unknown)}
#'   \item{SI}{serial interval = number of days between dates of prodromes of infection source and infected person}
#'   \item{C}{Complications (factor: "no complications", or what complication)}
#'   \item{PR}{duration of prodromes in days}
#'   \item{CA}{number of cases in family}
#'   \item{NI}{number of initial cases}
#'   \item{GE}{generation number of the case}
#'   \item{TD}{day of max. fever (days after rush)}
#'   \item{TM}{max.fever (degree Celsius)}
#'   \item{x.loc}{x coordinate of house (in metres). Scaling in metres
#'   is obtained by multiplying the original coordinates by 2.5 (see details
#'   in Neal and Roberts (2004))}
#'   \item{y.loc}{y coordinate of house (in metres). See \code{x.loc} above}
#'   \item{tPRO}{exact time of onset of prodromal symptoms (relative to start
#'   of outbreak)}
#'   \item{tERU}{exact time of onset of rash (relative to start
#'   of outbreak) }
#'   \item{tDEAD}{time of death (relative to start of outbreak) (\code{NA}
#'   implies recovered)}
#'   \item{tR}{imputed time when individual entered into recovery}
#'   \item{tI}{imputed time when individual was infected}
#'  \item{tBORN}{Imputed time when individual was born.  (Only for \code{hagelloch_aug_births}}
#' }
#'
#' @source \url{https://rdrr.io/rforge/surveillance/man/hagelloch.html}
#'
#' @examples
#' ## show first few cases
#' head(hagelloch_raw); head(hagelloch_raw2)
"hagelloch_raw"


#' @rdname hagelloch_raw
"hagelloch_raw2"

#' @rdname hagelloch_raw
"hagelloch_aug_births"


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
#' @format A data frame with 188 rows and 3 columns. Below are the description of
#' each column.
#' \describe{
#'   \item{init_state}{Initial state (\code{factor}) for individual (at time t = 0). For the
#'   states, 0 = S, 1 = I, 2 = R.}
#'   \item{max_time_S}{maximum time individual was suspectable (S)}
#'   \item{max_time_I}{maximum time individual was infected (I)}
#' }
#'
#' @examples
#' ## show first few cases
#' head(hagelloch_agents)
"hagelloch_agents"


#' agents_sims Example output from SIR simulations
#'
#' This simulated data is an example of the output from the function \code{simulate_SIR_agents()} when \code{output_format = "array"}.  It is a 50 x 3 x 188 array where entry (i,j,k) loks at the ith simulation, the jth statistic, and the kth agent.
#' Each column is asssociated with 1 agent and
#' is a "sufficient" statistic for each agent's infection.  Each agent's
#' infection is uniquely identified by an initial state, max time before
#' infection (or time T-1), and max time before recovery (or time T-1). Note time T-1
#' is the final recorded time in the SIR model (in this case T = 50).
#'
#' @format An array with dimension 50 x 3 x 188 data frame with 188 rows and 3 columns.  Below are the descriptions of each dimension
#' \describe{
#'   \item{sim}{Simulation number is the first dimension.  They are named between 1 and 50}
#'   \item{agent_stat}{This is the sufficient statistic for each agent as described above.  The names of the statistics are \code{init_state}, \code{max_time_S}, and \code{max_time_I}}
#'   \item{agent_id}{the ID of the agent}
#' }
#'
#' @examples
#' ## show first simulation of first 10 agents
#' agents_sims[1, , 1:10]
"agents_sims"






#' pomp_sir Example output from the \code{pomp} package.
#'
#' Specifcially, this data is made from the SIR example found from \href{https://kingaa.github.io/pomp/vignettes/oaxaca.html}{this \code{pomp} vignette}.  The output format for the data is \code{data.frame}.  This data set is a data frame with dimensions 52100 x 7.  Each row consists of the time, number of individuals in each state, and simulation ID.  The birth and death rates have been set to zero.
#'
#' @format A 52100 x 7 data.frame where each row is a time and the number of individuals in each state at that time.  The columns include
#' \describe{
#' \item{time}{possibly continuous, greater than 0}
#' \item{.id}{the simulation ID}
#' \item{S}{the number of Susceptible at the given time}
#' \item{I}{the number of Infectious at the given time}
#' \item{R}{the number of Recovered at the given time}
#' \item{H}{the number of new incidence}
#' \item{cases}{the number of cases recorded within a given reporting interval}
#' }
#'
#' @examples
#' ## Show the first 6 lines
#' head(pomp_sir)
"pomp_sir"


#' EpiModel_det Example output from the \code{EpiModel} package for a deterministic model
#'
#' This data is made from the SIR DCM/ICM example in the \href{http://statnet.github.io/tut/BasicICMs.html}{EpiModel vignette found here}.  We run the discrete compartmental model (DCM) for 300 steps and set the birth and death rates (a.rate, ds.rate, di.rate, dr.rate) to zero.  This is object is of class \code{dcm}.  The output details can be found with \code{?EpiModel::dcm}.
#'
#' @format a \code{dcm} object from the \code{EpiModel} package.  This contains the following elements:
#' \describe{
#' \item{param}{the epidemic parameters passed into the model through param, with additional parameters added as necessary.}
#' \item{control}{the control settings passed into the model through control, with additional controls added as necessary.}
#' \item{epi}{a list of data frames, one for each epidemiological output from the model. Outputs for base models always include the size of each compartment, as well as flows in, out of, and between compartments.}
#' }
#'
#' @examples
#' ## Look at structure
#' str(EpiModel_det)
#'
#' ## Look at SIR values
#' head(do.call('cbind', EpiModel_det$epi[c("s.num", "i.num", "r.num")]))
"EpiModel_det"


#' EpiModel_icm Example output from the \code{EpiModel} package for a stochastic ICM
#'
#' This data is made from the SIR DCM/ICM example in the \href{http://statnet.github.io/tut/BasicICMs.html}{EpiModel vignette found here}.  We run the individual contact model (ICM) for 300 steps and set the birth and death rates (a.rate, ds.rate, di.rate, dr.rate) to zero.  This is object is of class \code{icm}.  We run the simulation 10 times.  The output details can be found with \code{?EpiModel::icm}.
#'
#' @format a \code{icm} object from the \code{EpiModel} package.  This contains the following elements:
#' \describe{
#' \item{param}{the epidemic parameters passed into the model through param, with additional parameters added as necessary.}
#' \item{control}{the control settings passed into the model through control, with additional controls added as necessary.}
#' \item{epi}{a list of data frames, one for each epidemiological output from the model. Outputs for base models always include the size of each compartment, as well as flows in, out of, and between compartments.}
#' }
#'
#' @examples
#' ## Look at structure
#' str(EpiModel_icm)
#'
#' ## Look at SIR values
#' head(EpiModel_icm$epi$s.num)
"EpiModel_icm"






#' EpiModel_agg_bd Example output from the \code{EpiModel} package for a individual model with birth and death rates.
#'
#' This data is made from the SIR DCM/ICM example in the \href{http://statnet.github.io/tut/BasicICMs.html}{EpiModel vignette found here}.  We run the discrete compartmental model (DCM) for 300 steps and set the birth and death rates (a.rate, ds.rate, di.rate, dr.rate) to .02.  This is object is of class \code{icm}.  The output details can be found with \code{?EpiModel::icm}.
#'
#' @format a \code{dcm} object from the \code{EpiModel} package.  This contains the following elements:
#' \describe{
#' \item{param}{the epidemic parameters passed into the model through param, with additional parameters added as necessary.}
#' \item{control}{the control settings passed into the model through control, with additional controls added as necessary.}
#' \item{epi}{a list of data frames, one for each epidemiological output from the model. Outputs for base models always include the size of each compartment, as well as flows in, out of, and between compartments.}
#' }
#'
#' @examples
#' ## Look at structure
#' str(EpiModel_agg_bd)
#'
#' ## Look at SIR values
#' head(do.call('cbind', EpiModel_agg_bd$epi[c("s.num", "i.num", "r.num")]))
"EpiModel_agg_bd"
