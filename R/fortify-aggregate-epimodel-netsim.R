## SKG
## March 24, 2020
## Adding the netsim model


#' Take external aggregate data and put it in a format used in this package
#'
#' @param data output from external source package.  See details
#' @param states names of states we want aggregate totals of at each time
#' @param package_source optional argument to include the package from which the
#'   output is derived from, which helps with the fortify function when outputs
#'   are of generic classes such as list or data.frame
#' @return a data frame with the following columns
#' \describe{
#' \item{t}{time}
#' \item{Xk}{columns X0, ..., X_K. which are numeric}
#' }
#' @details This function converts external data sources (we currently support
#'   output from the EpiModel and pomp R packages), which is already aggregated
#'   and puts it in a format that can be used by our exploring functions.
#' @export
#' @examples
#' ## For netsim  NOT RUN
#' ## From http://statnet.org/tut/BasicNet.html
#' ## est1 <- netest(nw, formation, target.stats, coef.diss, edapprox = TRUE)
#'
#' ## nw <- network.initialize(n = 1000, directed = FALSE)
#' ## nw <- set.vertex.attribute(nw, "race", rep(0:1, each = 50))
#' ## formation <- ~edges + nodefactor("race") + nodematch("race") + concurrent
#' ## target.stats <- c(250, 375, 225, 100)
#' ## coef.diss <- dissolution_coefs(dissolution = ~offset(edges),
#' ## duration = 25)
#' 
#' ## param <- param.net(inf.prob = 0.1, act.rate = 5, rec.rate = 0.02)
#' ## status.vector <- c(rbinom(500, 1, 0.1), rep(0, 500))
#' ## status.vector <- ifelse(status.vector == 1, "i", "s")
#' ## init <- init.net(status.vector = status.vector)
#' ## control <- control.net(type = "SIS", nsteps = 50,
#' ##nsims = 10, epi.by = "race")
#' ## sim1 <- netsim(est1, param, init, control)
#' 
#' ## out <- fortify_aggregate(sim1)
#' ## head(out)
#' @export
fortify_aggregate.netsim <- function(data,
                                  states = NULL,
                                  package_source = NULL){

    out <- fortify_aggregate.epimodel_inner(data,
                                 states = dplyr::enquo(states))
    return(out)

}


