tidyr_new_interface <- function() FALSE

.onLoad <- function(...) {
  if (utils::packageVersion("tidyr") > "0.8.2") {
    tidyr_new_interface <<- function() TRUE
  }
}
