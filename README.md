# timeternR

<!-- badges: start -->
<!-- badges: end -->

The goal of timeternR is to develop functional based visualization and statistics
that focus on use in ternary plots.

## Installation

You can install the developmental version of timeternR from github using:

``` r
# install.package("devtools")
devtools::install_github("shannong19/timeternR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(timeternR)
## basic example code will go here
```

## Data

   Currently there are three data sets available in `timeternR`

   - `hagelloch_raw` -- One row is an agent.  This is taken from the `surveillance` R package and the variable descriptions are found [here](https://rdrr.io/cran/outbreaks/man/measles_hagelloch_1861.html) **Ben: this originally was actually called `hagelloch.df`, but I changed it to fit this name - we can change it back.**

   - `hagelloch_sir`  -- One row is a state of $(t, s_t, i_t, r_t)$ where $s_t + i_t + r_t = N$ for $t = 0, \dots, T=94$

   - `hagelloch_agents` -- One **column** is a "sufficient" statistic for each agent's infection.  Each agent's infection is uniquely identified by an initial state, max time before infection (or T), and max time before recovery (or T).  For the states, 0 = S, 1 = I, 2 = R. **Ben: why isn't this a data frame with transposed?, also is there a reason we don't change initial state into a factor if we make this into a data.frame?**

 
