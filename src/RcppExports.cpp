// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// convolve_cpp
arma::vec convolve_cpp(const arma::vec& a, const arma::vec& b);
RcppExport SEXP _dependlab_convolve_cpp(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(convolve_cpp(a, b));
    return rcpp_result_gen;
END_RCPP
}
// convolve_double_gamma
arma::vec convolve_double_gamma(const arma::vec& stimulus, double a1, double a2, double b1, double b2, double cc);
RcppExport SEXP _dependlab_convolve_double_gamma(SEXP stimulusSEXP, SEXP a1SEXP, SEXP a2SEXP, SEXP b1SEXP, SEXP b2SEXP, SEXP ccSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type stimulus(stimulusSEXP);
    Rcpp::traits::input_parameter< double >::type a1(a1SEXP);
    Rcpp::traits::input_parameter< double >::type a2(a2SEXP);
    Rcpp::traits::input_parameter< double >::type b1(b1SEXP);
    Rcpp::traits::input_parameter< double >::type b2(b2SEXP);
    Rcpp::traits::input_parameter< double >::type cc(ccSEXP);
    rcpp_result_gen = Rcpp::wrap(convolve_double_gamma(stimulus, a1, a2, b1, b2, cc));
    return rcpp_result_gen;
END_RCPP
}
// deconvolve_nlreg
arma::mat deconvolve_nlreg(arma::mat BOLDobs, const arma::vec& kernel, double nev_lr, double epsilon, double beta, bool normalize, bool trim_kernel);
RcppExport SEXP _dependlab_deconvolve_nlreg(SEXP BOLDobsSEXP, SEXP kernelSEXP, SEXP nev_lrSEXP, SEXP epsilonSEXP, SEXP betaSEXP, SEXP normalizeSEXP, SEXP trim_kernelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type BOLDobs(BOLDobsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type kernel(kernelSEXP);
    Rcpp::traits::input_parameter< double >::type nev_lr(nev_lrSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< bool >::type trim_kernel(trim_kernelSEXP);
    rcpp_result_gen = Rcpp::wrap(deconvolve_nlreg(BOLDobs, kernel, nev_lr, epsilon, beta, normalize, trim_kernel));
    return rcpp_result_gen;
END_RCPP
}
// dsigmoid
arma::vec dsigmoid(const arma::vec& x, double beta);
RcppExport SEXP _dependlab_dsigmoid(SEXP xSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(dsigmoid(x, beta));
    return rcpp_result_gen;
END_RCPP
}
// generate_feature
NumericMatrix generate_feature(NumericVector encoding, int K);
RcppExport SEXP _dependlab_generate_feature(SEXP encodingSEXP, SEXP KSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type encoding(encodingSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_feature(encoding, K));
    return rcpp_result_gen;
END_RCPP
}
// generate_feature_armadillo
arma::mat generate_feature_armadillo(const arma::vec& encoding, int K);
RcppExport SEXP _dependlab_generate_feature_armadillo(SEXP encodingSEXP, SEXP KSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type encoding(encodingSEXP);
    Rcpp::traits::input_parameter< int >::type K(KSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_feature_armadillo(encoding, K));
    return rcpp_result_gen;
END_RCPP
}
// sigmoid
arma::vec sigmoid(const arma::vec& x, double beta);
RcppExport SEXP _dependlab_sigmoid(SEXP xSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(sigmoid(x, beta));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dependlab_convolve_cpp", (DL_FUNC) &_dependlab_convolve_cpp, 2},
    {"_dependlab_convolve_double_gamma", (DL_FUNC) &_dependlab_convolve_double_gamma, 6},
    {"_dependlab_deconvolve_nlreg", (DL_FUNC) &_dependlab_deconvolve_nlreg, 7},
    {"_dependlab_dsigmoid", (DL_FUNC) &_dependlab_dsigmoid, 2},
    {"_dependlab_generate_feature", (DL_FUNC) &_dependlab_generate_feature, 2},
    {"_dependlab_generate_feature_armadillo", (DL_FUNC) &_dependlab_generate_feature_armadillo, 2},
    {"_dependlab_sigmoid", (DL_FUNC) &_dependlab_sigmoid, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dependlab(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
