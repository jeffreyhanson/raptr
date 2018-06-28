#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

#include <vector>
#include <RcppEigen.h>
#include <Rcpp.h>

// [[Rcpp::export]]
double rcpp_rrap_proportion_held(Rcpp::NumericMatrix pu_coordinates, Rcpp::NumericVector pu_probabilities, Rcpp::NumericMatrix dp_coordinates, Rcpp::NumericVector dp_weights, double failure_distance, std::size_t maximum_r_level) {
  //// Initialization
  Eigen::MatrixXd pu_coordinates_MTX(Rcpp::as<Eigen::MatrixXd>(pu_coordinates));
  Eigen::MatrixXd dp_coordinates_MTX(Rcpp::as<Eigen::MatrixXd>(dp_coordinates));
    
  //// Preliminary processing
  // declare variables
  std::size_t N_pu_INT = pu_coordinates_MTX.rows();
  std::size_t N_dp_INT = dp_coordinates_MTX.rows(); 
  std::size_t N_k_INT = pu_coordinates_MTX.cols(); 

  //// Main processing
  /// calculate tss
  // compute centroid
  Eigen::RowVectorXd dp_centroid_MTX(N_k_INT);
  dp_centroid_MTX = dp_coordinates_MTX.colwise().sum();
  dp_centroid_MTX /= static_cast<double>(N_dp_INT);
  // tss
  Eigen::ArrayXd tmp_AXD(N_dp_INT);
  tmp_AXD = (dp_coordinates_MTX.rowwise() - dp_centroid_MTX.row(0)).rowwise().squaredNorm().transpose();
  tmp_AXD *= Rcpp::as<Eigen::ArrayXd>(dp_weights);
  double tss_DBL = tmp_AXD.sum();
  
  /// calculate wss
  double wss_DBL = 0.0;
  double currProb;
  double curr_value;
  Eigen::ArrayXd tmp_AXD2(N_pu_INT);
  std::vector<size_t> pu_ids(N_pu_INT);
  for (std::size_t j=0; j < N_dp_INT; ++j) {
    // init
    std::iota(pu_ids.begin(), pu_ids.end(), 0);
    // calculate distances
    tmp_AXD2 = (pu_coordinates_MTX.rowwise() - dp_coordinates_MTX.row(j)).rowwise().squaredNorm().transpose();
    
    // sort pus in order of distance
    std::partial_sort(
      pu_ids.begin(), pu_ids.begin()+maximum_r_level, pu_ids.end(),
      [&](const std::size_t p1, const std::size_t p2) {
        return(tmp_AXD2[p1] < tmp_AXD2[p2]);
      }
    );
    
    // calculate expected weighted distances for real pus
    currProb=1.0;
    curr_value=0.0;
    for (auto i = pu_ids.cbegin(); i<pu_ids.cbegin()+maximum_r_level; ++i) {
      curr_value+=(pu_probabilities[*i] * currProb * tmp_AXD2[*i] * dp_weights[j]);
      currProb*=(1.0 - pu_probabilities[*i]);
    }
    // calculate expected weighted distance for failure PU
    curr_value+=(currProb*failure_distance);
    // update value
    wss_DBL += curr_value;
  }

  //// Exports
  // return proportion held
  return (1.0 - (wss_DBL / tss_DBL));
}

