#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <string>

// [[Rcpp::export]]
SEXP rcpp_dump_numeric_object(SEXP x) {
  std::vector<double> x1 = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(x);
  return (Rcpp::wrap(x1));
}

// [[Rcpp::export]]
SEXP rcpp_dump_integer_object(SEXP x) {
  std::vector<int> x1 = *Rcpp::as<Rcpp::XPtr<std::vector<int>>>(x);
  return (Rcpp::wrap(x1));
}

// [[Rcpp::export]]
SEXP rcpp_dump_character_object(SEXP x) {
  std::vector<std::string> x1 = *Rcpp::as<Rcpp::XPtr<std::vector<std::string>>>(x);
  return (Rcpp::wrap(x1));
}
