// rcpp_segmentor.cpp
// Rcpp interface layer for the PDP segmentation algorithm.
// Replaces the PrunedSeg.cc extern-C wrappers and init.cpp registration.
// The core algorithm files (CallSegmentor.cpp, Segmentor.h, etc.) are untouched.

#include <Rcpp.h>
#include "CallSegmentor.h"

// ---- Helper: build output list for standard (non-Keep) calls ----
static Rcpp::List make_result(int K, int n,
                               std::vector<int>&    breaks,
                               std::vector<double>& params,
                               std::vector<double>& likelihood)
{
  return Rcpp::List::create(
    Rcpp::Named("Breakpoints") = breaks,
    Rcpp::Named("Parameters")  = params,
    Rcpp::Named("Likelihood")  = likelihood
  );
}

// ---- Helper: build output list for Keep calls (includes Cost/Pos) ----
static Rcpp::List make_result_keep(int K, int n,
                                    std::vector<int>&    breaks,
                                    std::vector<double>& params,
                                    std::vector<double>& likelihood,
                                    std::vector<double>& cost,
                                    std::vector<int>&    pos)
{
  return Rcpp::List::create(
    Rcpp::Named("Breakpoints") = breaks,
    Rcpp::Named("Parameters")  = params,
    Rcpp::Named("Likelihood")  = likelihood,
    Rcpp::Named("Cost")        = cost,
    Rcpp::Named("Pos")         = pos
  );
}

// =============================================================================
// Poisson  (integer data)
// =============================================================================

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_poisson(int n2, int kmax,
                                 Rcpp::IntegerVector data,
                                 Rcpp::NumericVector bounds,
                                 Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);

  CallSegmentorPoisson(&n2, &K,
                       data.begin(),
                       bounds.begin(),
                       data_comp.begin(),
                       breaks.data(), params.data(), likelihood.data());

  return make_result(K, n2, breaks, params, likelihood);
}

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_poisson_keep(int n2, int kmax,
                                      Rcpp::IntegerVector data,
                                      Rcpp::NumericVector bounds,
                                      Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);
  std::vector<double> cost(K * n2, 0.0);
  std::vector<int>    pos(K * n2, 0);

  CallSegmentorPoissonKeep(&n2, &K,
                            data.begin(),
                            bounds.begin(),
                            data_comp.begin(),
                            breaks.data(), params.data(), likelihood.data(),
                            cost.data(), pos.data());

  return make_result_keep(K, n2, breaks, params, likelihood, cost, pos);
}

// =============================================================================
// Exponential  (double data)
// =============================================================================

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_exponential(int n2, int kmax,
                                     Rcpp::NumericVector data,
                                     Rcpp::NumericVector bounds,
                                     Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);

  CallSegmentorExponential(&n2, &K,
                            data.begin(),
                            bounds.begin(),
                            data_comp.begin(),
                            breaks.data(), params.data(), likelihood.data());

  return make_result(K, n2, breaks, params, likelihood);
}

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_exponential_keep(int n2, int kmax,
                                          Rcpp::NumericVector data,
                                          Rcpp::NumericVector bounds,
                                          Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);
  std::vector<double> cost(K * n2, 0.0);
  std::vector<int>    pos(K * n2, 0);

  CallSegmentorExponentialKeep(&n2, &K,
                                data.begin(),
                                bounds.begin(),
                                data_comp.begin(),
                                breaks.data(), params.data(), likelihood.data(),
                                cost.data(), pos.data());

  return make_result_keep(K, n2, breaks, params, likelihood, cost, pos);
}

// =============================================================================
// Negative Binomial  (integer data, extra theta parameter)
// =============================================================================

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_binneg(int n2, int kmax, double theta,
                                Rcpp::IntegerVector data,
                                Rcpp::NumericVector bounds,
                                Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);

  CallSegmentorBinNeg(&n2, &K, &theta,
                      data.begin(),
                      bounds.begin(),
                      data_comp.begin(),
                      breaks.data(), params.data(), likelihood.data());

  return make_result(K, n2, breaks, params, likelihood);
}

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_binneg_keep(int n2, int kmax, double theta,
                                     Rcpp::IntegerVector data,
                                     Rcpp::NumericVector bounds,
                                     Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);
  std::vector<double> cost(K * n2, 0.0);
  std::vector<int>    pos(K * n2, 0);

  CallSegmentorBinNegKeep(&n2, &K, &theta,
                           data.begin(),
                           bounds.begin(),
                           data_comp.begin(),
                           breaks.data(), params.data(), likelihood.data(),
                           cost.data(), pos.data());

  return make_result_keep(K, n2, breaks, params, likelihood, cost, pos);
}

// =============================================================================
// Normal  (double data)
// =============================================================================

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_normal(int n2, int kmax,
                                Rcpp::NumericVector data,
                                Rcpp::NumericVector bounds,
                                Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);

  CallSegmentorNormal(&n2, &K,
                      data.begin(),
                      bounds.begin(),
                      data_comp.begin(),
                      breaks.data(), params.data(), likelihood.data());

  return make_result(K, n2, breaks, params, likelihood);
}

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_normal_keep(int n2, int kmax,
                                     Rcpp::NumericVector data,
                                     Rcpp::NumericVector bounds,
                                     Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);
  std::vector<double> cost(K * n2, 0.0);
  std::vector<int>    pos(K * n2, 0);

  CallSegmentorNormalKeep(&n2, &K,
                           data.begin(),
                           bounds.begin(),
                           data_comp.begin(),
                           breaks.data(), params.data(), likelihood.data(),
                           cost.data(), pos.data());

  return make_result_keep(K, n2, breaks, params, likelihood, cost, pos);
}

// =============================================================================
// Variance  (double data, extra mu parameter)
// =============================================================================

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_variance(int n2, int kmax, double mu,
                                  Rcpp::NumericVector data,
                                  Rcpp::NumericVector bounds,
                                  Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);

  CallSegmentorVariance(&n2, &K, &mu,
                        data.begin(),
                        bounds.begin(),
                        data_comp.begin(),
                        breaks.data(), params.data(), likelihood.data());

  return make_result(K, n2, breaks, params, likelihood);
}

//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_segment_variance_keep(int n2, int kmax, double mu,
                                       Rcpp::NumericVector data,
                                       Rcpp::NumericVector bounds,
                                       Rcpp::IntegerVector data_comp)
{
  int K = kmax;
  std::vector<int>    breaks(K * K, 0);
  std::vector<double> params(K * K, 0.0);
  std::vector<double> likelihood(K, 0.0);
  std::vector<double> cost(K * n2, 0.0);
  std::vector<int>    pos(K * n2, 0);

  CallSegmentorVarianceKeep(&n2, &K, &mu,
                             data.begin(),
                             bounds.begin(),
                             data_comp.begin(),
                             breaks.data(), params.data(), likelihood.data(),
                             cost.data(), pos.data());

  return make_result_keep(K, n2, breaks, params, likelihood, cost, pos);
}
