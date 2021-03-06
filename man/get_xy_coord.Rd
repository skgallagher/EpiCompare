% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bands.R
\name{get_xy_coord}
\alias{get_xy_coord}
\title{get xy ternary coordinates from xyz based data frame}
\usage{
get_xy_coord(X_SIR, xyz_col = c("S", "I", "R"))
}
\arguments{
\item{X_SIR}{data.frame with columns in xyz_col}

\item{xyz_col}{string vector (length 3) to match with x, y, and z. The example
is c(S,I,R) or c("S", "I", "R"). Both styles work}
}
\value{
X_SIR motified to have columns "x" and "y" with the ternary
  coordinates
}
\description{
note that this does not need x,y,z to be scaled (but it should).
  This is just a data.frame wrapper for ggtern::tlr2xy.
}
