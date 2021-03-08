#ifndef _dependlab_DEPENDLAB_h
#define _dependlab_DEPENDLAB_h

#define ARMA_NO_DEBUG
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

//function definitions

arma::vec convolve_cpp(const arma::vec &a, const arma::vec &b);
arma::vec convolve_double_gamma(const arma::vec &stimulus, double a1, double a2, double b1, double b2, double cc);
Rcpp::NumericMatrix generate_feature_armadillo(arma::vec encoding, int K);
Rcpp::NumericMatrix generate_feature(Rcpp::NumericVector encoding, int K);

//RcppExport arma::vec convolve_cpp(arma::vec a, arma::vec b);
//extern "C" arma::vec convolve_cpp(arma::vec a, arma::vec b);

#endif
