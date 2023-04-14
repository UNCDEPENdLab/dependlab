#include "dependlab.h"

//' Internal function to convolve two vectors using FFT
//'
//' @name convolve_fft_cpp
//' @param a A vector
//' @param b A vector
//' @return A vector containing the convolution of a and b
//'
//' @details This is an internal function
//'       Same result as, in R:
//'       convolve(a, b, conj=TRUE, type="open")

//' @author Michael Hallquist

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::fvec convolve_fft_cpp(const arma::fvec & a, const arma::fvec & b) {
  //Compute the FFT size as the "next power of 2" of the input vector's length (max)
  int N = std::max(a.n_elem, b.n_elem);
  //cout << "N: " << N << endl;
  int size = ceil(log2(2.0 * N - 1));
  int fftsize = pow(2, size);

  //cout << "size: " << size << endl;
  //uint32_t size = pow(2,ceil(log2(::conv_sig_size + ::conv_ir_size - 1)));

  arma::cx_fvec output = arma::ifft(arma::fft(a,fftsize) % arma::fft(b,fftsize));

  //AudioVec output_copy = arma::conv_to<AudioVec>::from(arma::real(output));
  //auto end = high_resolution_clock::now();

  return arma::real(output);
}


/*** R
convolve_fft_cpp(rnorm(100), rnorm(100))
*/

//Rcpp::NumericVector convolve_cpp(Rcpp::NumericVector a, Rcpp::NumericVector b) {
