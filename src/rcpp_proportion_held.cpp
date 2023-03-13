#include <Rcpp.h>
// [[Rcpp::plugins(cpp14)]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

#include <vector>
#include <RcppEigen.h>
#include <Rcpp.h>

// [[Rcpp::export]]
double rcpp_proportion_held(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y, Rcpp::NumericVector y_weights) {
  //// Initialization
  Eigen::MatrixXd x_MTX(Rcpp::as<Eigen::MatrixXd>(x));
  Eigen::MatrixXd y_MTX(Rcpp::as<Eigen::MatrixXd>(y));
    
  //// Preliminary processing
  // declare variables
  std::size_t N_x_INT = x_MTX.rows();
  std::size_t N_y_INT = y_MTX.rows(); 
  std::size_t N_k_INT = y_MTX.cols(); 
  Eigen::ArrayXd tmp_AXD;

  //// Main processing  
  /// calculate tss
  // compute centroid
  Eigen::RowVectorXd y_centroid_MTX(N_k_INT);
  y_centroid_MTX = y_MTX.colwise().sum();
  y_centroid_MTX /= static_cast<double>(N_y_INT);
  // tss
  double tss_DBL=0.0;
  for (std::size_t i=0; i<N_y_INT; ++i) {
    tmp_AXD = y_centroid_MTX - y_MTX.row(i);
    tss_DBL += (tmp_AXD.square().sum() * y_weights[i]);
  }
  
  /// calculate wss
  double wss_DBL = 0.0;
  double curr_lowest_DBL;
  for (std::size_t j=0; j < N_y_INT; ++j) {
    curr_lowest_DBL = std::numeric_limits<double>::max();
    for (std::size_t i=0; i < N_x_INT; ++i) {
      tmp_AXD = x_MTX.row(i) - y_MTX.row(j);
      curr_lowest_DBL = std::min(tmp_AXD.square().sum() * y_weights[i], curr_lowest_DBL);
    }
    wss_DBL += curr_lowest_DBL;
  }

  //// Exports
  // return proportion held
  return (1.0 - (wss_DBL / tss_DBL));
}
