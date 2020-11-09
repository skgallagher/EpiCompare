
#' logic to check if tidyverse (and tidyr specifically is up to version 1.0)
#'
#' @return logical value (boolean)
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}

#' Convert SEIR to XYZ coordinates fixed in a tetrahedron
#'
#' @param data data frame with the following columns \describe{ \item{time}{time
#'   step} \item{S}{Number of people in S} \item{E}{Number of people in E}
#'   \item{I}{Number of people in I} \item{R}{Number of people in R} }
#' @param var_order vector of column names corresponding to the different axes
#'   of the tetrahedron-based coordinate system:  (t, l, r, f) which stands for
#'   top, left, right, front. Can be of the form \code{c("t", "l", "r", "f")} or
#'   \code{c(t,l,r,f)}.
#' @param time_name column name that is associated with the time step. Can
#'   either be a string or a promisary symbol
#' @return original data frame along with columns x, y, and z
#' @export
#' @examples
#' seir <- data.frame(t = 0:3,
#' S = c(90, 80, 70, 60),
#' E = c(0, 10, 10, 10),
#' I = c(10, 10, 10, 10),
#' R = c(0, 0, 10, 20))
#' seir_xyz <- SEIR_to_XYZ(seir)
#' #head(seir_xyz)
SEIR_to_XYZ <- function(data,
                      var_order = c("S", "E", "I", "R"),
                      time_name = "t"){
  # quos
  var_order_q <- dplyr::enquos(var_order)
  var_order <- unname(tidyselect::vars_select(dplyr::tbl_vars(data),
                                           !!!var_order_q))

  time_name_q <- dplyr::enquo(time_name)
  time_name <- unname(tidyselect::vars_select(dplyr::tbl_vars(data),
                                              !!time_name_q))

  new_df <- data %>% dplyr::rename(time = time_name,
                                   t = var_order[1],
                                   l = var_order[2],
                                   r = var_order[3],
                                   f = var_order[4]) %>%
    dplyr::mutate(N = .data$t + .data$l + .data$r + .data$f) %>%
    dplyr::mutate(x = (.data$r + 1 - .data$l ) / 2 / .data$N,
                  y = (sqrt(3)/2 * .data$t + sqrt(3)/6 * .data$f) / .data$N,
                  z = sqrt(6) / 3 * .data$f / .data$N) %>%
    dplyr::select(-.data$N)

  names(new_df)[names(new_df) == "time"] <- time_name
  return(new_df)

}

#' Convert SEIR to XYZ coordinates fixed in a tetrahedron
#'
#' @param data data frame with the following columns \describe{ \item{t}{time
#'   step} \item{S}{Number of people in S} \item{E}{Number of people in E}
#'   \item{I}{Number of people in I} \item{R}{Number of people in R} }
#' @param ternary_vars named vector of the three variables to use as the sides
#'   of the ternary plot. Can be of the form \code{c("t", "l", "r", "f")} or
#'   \code{c(t,l,r,f)}.
#' @param group_var name of the variable to use as the color/feature/grouping
#'   vector. column name that is associated with the time step. Can either be a
#'   string or a promisary symbol
#' @return data frame with the transformed variables SEIR -> s, i, r, group
#'   variables
#' @export
#' @examples
#' seir <- data.frame(t = 0:3,
#' S = c(90, 80, 70, 60),
#' E = c(0, 10, 10, 10),
#' I = c(10, 10, 10, 10),
#' R = c(0, 0, 10, 20))
#' seir_xyz <- SEIR_to_XYZ(seir)
#' head(seir_xyz)
SEIR_to_SIR_E <- function(data,
                          ternary_vars = c("S", "I", "R"),
                          group_var = "E"){
  # quos
  ternary_vars_q <- dplyr::enquos(ternary_vars)
  ternary_vars <- unname(tidyselect::vars_select(dplyr::tbl_vars(data),
                                                 !!!ternary_vars))

  group_var_q <- dplyr::enquo(group_var)
  group_var <- unname(tidyselect::vars_select(dplyr::tbl_vars(data),
                                              !!group_var_q))

  new_df <- data %>% dplyr::rename(S = ternary_vars[1],
                                   I = ternary_vars[2],
                                   R = ternary_vars[3],
                                   group = group_var[1]) %>%
    dplyr::mutate(N = .data$S + .data$I + .data$R + .data$group,
                  n = .data$S + .data$I + .data$R) %>%
    dplyr::mutate(S = .data$S / .data$n, I = .data$I / .data$n, R = .data$R / .data$n,
                  group = .data$group / .data$N) %>%
    dplyr::select(-c(.data$N, .data$n))
  return(new_df)
}


#' check if a character is a desirable percentage value
#'
#' @param x character
#' @param name name to call \code{x} if there is an error
#'
#' @return percentage that x represents
check_character_percent <- function(x, name = "x"){
  assertthat::assert_that(stringr::str_detect(x, "%$"),
                      msg = sprintf("if %s is a character it must be '__%%'",
                                    name))
  
  percentage <- as.numeric(stringr::str_remove(x, "%$"))/100
  assertthat::assert_that(percentage <= 1 & percentage > 0,
                          msg = sprintf(paste("if %s is centered as a percent,",
                                      "it must be a percentage <= 100%% and",
                                      "greater than 0%%"),
                                      name))
  return(percentage)
}

