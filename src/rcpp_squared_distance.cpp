#include <Rcpp.h>
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

#include <vector>
#include <RcppEigen.h>
#include <Rcpp.h>

// [[Rcpp::export]]
std::vector<double> rcpp_squared_distance(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y, Rcpp::NumericVector y_weights) {
  //// Initialization
  Eigen::MatrixXd x_MTX(Rcpp::as<Eigen::MatrixXd>(x));
  Eigen::MatrixXd y_MTX(Rcpp::as<Eigen::MatrixXd>(y));
    
  //// Preliminary processing
  // declare variables
  std::size_t N_x_INT = x_MTX.rows();
  std::size_t N_y_INT = y_MTX.rows(); 
  Eigen::ArrayXd tmp_AXD;

  //// Main processing
  /// calculate wss
  std::vector<double> d(N_y_INT);
  double curr_lowest_DBL;
  for (std::size_t j=0; j < N_y_INT; ++j) {
    curr_lowest_DBL = std::numeric_limits<double>::max();
    for (std::size_t i=0; i < N_x_INT; ++i) {
      tmp_AXD = x_MTX.row(i) - y_MTX.row(j);
      curr_lowest_DBL = std::min(tmp_AXD.square().sum() * y_weights[i], curr_lowest_DBL);
    }
    d[j] = curr_lowest_DBL;
  }

  //// Exports
  // return distances
  return (d);
}

