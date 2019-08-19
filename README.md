# timeternR

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/shannong19/timeternR.svg?branch=master)](https://travis-ci.org/shannong19/timeternR)
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

   - `hagelloch_raw` -- One row is an agent.  This is imported from the `surveillance` R package and the variable descriptions are found [here](https://rdrr.io/rforge/surveillance/man/hagelloch.html) where it is originally labeled `hagelloch.df`.  We have renamed it here to help distinguish it from the other data sets we derive from it.
   
   
   
   - `hagelloch_sir`  -- One row is a state of $(t, s_t, i_t, r_t)$ where $s_t + i_t + r_t = N$ for $t = 0, \dots, T=94$

   - `hagelloch_agents` -- One row is a "sufficient" statistic for each agent's infection.  Each agent's infection is uniquely identified by an initial state, max time before infection (or T), and max time before recovery (or T).  For the states, 0 = S, 1 = I, 2 = R.

  - `U_sims` this is 50 x 3 x 188 array where entry (i,j,k) looks at the ith simulation, the jth statistic, and the kth agent.  The statistics are (inititial state (0/1/2), SMax, IMax)  

## functions
 
The following can nicely make visuals conditional on grouping, on the flip side
it appears to be harder to develop your own stats for `ggtern` ([issue](https://github.com/nicholasehamilton/ggtern/issues/40)).

```{r}
U_g <- timeternR::hagelloch_raw %>% fortify_agents() %>% group_by(cut(AGE,3))
data_g <- timeternR::UtoX_SIR_group(U_g)
ggplot(data_g, aes(x = S, y = I, z = R, color = `cut(AGE, 3)`)) +
  geom_path() +
  coord_tern() + facet_grid(~`cut(AGE, 3)`)
```


