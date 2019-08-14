#include <Rcpp.h>
using namespace Rcpp;


// take 3xN suff stats and convert it to (Tmax)xN for SIR
// [[Rcpp::export]]
IntegerMatrix UtoA_SIR(IntegerMatrix U, int Tmax){
  int N = U.ncol();
  IntegerMatrix A(Tmax, N);
  // Fill in initial states
  for(int nn=0; nn < N; nn++){
    A(0, nn) = U(0, nn);
  }
  // Initialize vars
  int SMaxTime = Tmax;
  int IMaxTime = Tmax;
  int A0 = 0;
  // Populate entries
  for(int nn=0; nn < N; nn++){
    A0 = U(0, nn);
    SMaxTime = U(1, nn);
    IMaxTime = U(2, nn);
    Rprintf("A0: %d, Smax: %d, IMax: %d\n", A0, SMaxTime, IMaxTime);
    for(int tt=1; tt < Tmax; tt++){
      if(A0 == 0){ // If agent starts susceptible
        if(SMaxTime < IMaxTime & SMaxTime < Tmax){  // agent gets infected and recovers
          if(tt <= SMaxTime){
            A(tt,nn) = 0;
          } else if (tt <= IMaxTime){
            A(tt,nn) = 1;
          } else{
            A(tt,nn) = 2;
          }
        } else if(SMaxTime < Tmax){ // agent gets infected but doesn't recover
          if(tt <= SMaxTime){
            A(tt,nn) = 0;
          } else{
            A(tt,nn) = 1;
          }
        } else {
          A(tt,nn) = 0; // agent doesn't get infected
        }
      } else if(A0 == 1){  // agent starts infected
        if(IMaxTime < Tmax){ // agent recovers
          if(tt <= IMaxTime){
            A(tt,nn) = 1;
          } else{
            A(tt,nn) = 2;
          }
        } else{ // agent does not recover
          A(tt, nn) = 1; // the rest are infectious
        }
      } else if(A0 == 2){ // agent starts recovered
        A(tt, nn) = 2; // all recovered
      }
    }
  }
  return A;

}

// A to U SIR
// [[Rcpp::export]]
IntegerMatrix AtoU_SIR(IntegerMatrix A){
  int Tmax = A.nrow();
  int N = A.ncol();
  IntegerMatrix U(3, N);
  // Initialize vars
  int A0;
  int SMax=Tmax;
  int IMax=Tmax;
  int Atn = -1;
  for(int nn=0; nn < N; nn++){
    SMax = Tmax -1;
    IMax = Tmax - 1;
    U(0,nn) = A(0, nn); // Initial state
    for(int tt=0; tt < Tmax; tt++){
     Atn = A(tt, nn); // current state
      if(Atn == 0){
        SMax = tt;
      } else if(Atn == 1){
        IMax = tt;
      }
    }
    U(1, nn) = SMax;
    U(2, nn) = IMax;
  }
  return U;
}

// A is a TxN matrix
// K is the number of states 0:(K-1)
// [[Rcpp::export]]
IntegerMatrix AtoX(IntegerMatrix A, int K){
  int T = A.nrow();
  int N = A.ncol();
  int count = 0;
  IntegerMatrix X(T, K);
  for(int tt=0; tt < T; tt++){
    for(int kk=0; kk < K; kk++){
      count = 0;
      for(int nn=0; nn < N; nn++){
        if(A(tt,nn) == kk){
          count++;
        }
      }
      X(tt, kk) = count;
    }
  }
  return X;
}

// Extract a single row of A
// A is a TxN matrix
// K is the number of states 0:(K-1)
// @return 1xN matrix
// [[Rcpp::export]]
IntegerMatrix extractRowA(IntegerMatrix A, int t){
  int N = A.ncol();
  IntegerMatrix row(1, N);
  for(int nn=0; nn < N; nn++){
    row(0,nn) = A(t, nn);
  }
  return row;

}

// Extract a single column of A
// A is a TxN matrix
// n is the agent we want to look at
// @return Tx1 matrix
// [[Rcpp::export]]
IntegerMatrix extractColA(IntegerMatrix A, int n){
  int T = A.nrow();
  IntegerMatrix col(T, 1);
  for(int tt=0; tt < T; tt++){
    col(tt, 0) = A(tt, n);
  }
  return col;

}

// U to X SIR
//
// U is a 3xN matrix (A0, SMax, IMax)
// T is max number of time steps (0:(T-1))
// X is a T x 3 matrix
// @return Tx3 matrix
// [[Rcpp::export]]
IntegerMatrix UtoX_SIR(IntegerMatrix U, int T){
  IntegerMatrix X(T, 3);
  int N = U.ncol();
  int SMax;
  int IMax;
  int A0;
  for(int nn=0; nn < N; nn++){
    SMax = U(1, nn);
    IMax = U(2, nn);
    A0 = U(0, nn);

    if(A0 == 0){ // agent starts S
      for(int tt=0; tt < T; tt++){
        if(tt <= SMax){
         X(tt, 0)++;
        } else if(tt <= IMax){
          X(tt,1)++;
        } else{
           X(tt,2)++;
        }
      }
    } else if (A0 == 1){ // agent starts I
      for(int tt=0; tt < T; tt++){
        if(tt <= IMax){
          X(tt, 1)++;
        } else {
          X(tt, 2)++;
        }
      }
    } else if(A0 == 2){ // agent starts R
      for(int tt=0; tt < T; tt++){
        X(tt, 2)++;
      }
    }

  }

return X;
}




//
// // You can include R code blocks in C++ files processed with sourceCpp
// // (useful for testing and development). The R code will be automatically
// // run after the compilation.
// //
//
// /*** R
// U <- matrix(c(0, 0, 1, 0,
//               0, 1, 2, 2,
//               1, 2, 2, 2), byrow = TRUE, nrow = 3)
// Tmax <- 3
// A <- UtoA_SIR(U, Tmax)
// A
//
// ## U to X
// ##
// X <- UtoX_SIR(U, Tmax)
// X
//
//
// A <- matrix(c(0, 0, 1, 2,
//               0, 1, 1, 2,
//               1, 2, 1, 2), byrow = TRUE, nrow = 3)
// U <- AtoU_SIR(A)
// U
//
// ## A to X
// X <- AtoX(A, K = 3)
//
// UtoA_SIR(AtoU_SIR(A), Tmax)
//
// U <- matrix(c(0, 0, 1, 0,
//               0, 1, 2, 2,
//               1, 2, 2, 2), byrow = TRUE, nrow = 3)
// AtoU_SIR(UtoA_SIR(U, Tmax))
//
// */
//
//
