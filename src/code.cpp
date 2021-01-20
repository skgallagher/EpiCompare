#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double l2filamentdist(NumericMatrix m1, NumericMatrix m2) {
  int nrow1 = m1.nrow();
  int ncol = m1.ncol();
  
  if (ncol != m2.ncol()) {
    throw std::runtime_error("Incompatible number of dimensions");
  }
  
  double out = 0;
  
  for (int r1 = 0; r1 < nrow1; r1++) {
    double total = 0;
    for (int c12 = 0; c12 < ncol; c12++) {
      total += pow(m1(r1, c12) - m2(r1, c12), 2);
    }
    out += total;
  }
  
  return out;
}


// [[Rcpp::export]]
List distalongpath(NumericMatrix m1){
  int nrow = m1.nrow();
  int ncol = m1.ncol();
  
  List out(2);
  NumericVector out1(nrow-1);
  NumericMatrix out2(nrow-1,ncol);
  
  double innerdistpart;
  
  for(int r1 = 0; r1 < nrow - 1; r1++) {
    double innerdist = 0;
    for (int c1 = 0; c1 < ncol; c1++) {
      innerdistpart = m1(r1+1, c1) - m1(r1,c1);
      out2(r1,c1) = innerdistpart;
      innerdist += pow(innerdistpart, 2);
    }
    out1(r1) = sqrt(innerdist);
  }
  out(0) = out1;
  out(1) = out2;
  
  return out;
}
