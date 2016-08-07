#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
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
		
	Rcout << "x_MTX" << std::endl;
	Rcout << x_MTX << std::endl;
	
	//// Preliminary processing
	// declare variables
	std::size_t N_x_INT = x_MTX.rows();
	std::size_t N_y_INT = y_MTX.rows(); 
	std::size_t N_k_INT = y_MTX.cols(); 
	Eigen::MatrixXd d_MTX(N_x_INT, N_y_INT);
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
	// calculate all distances between x and y
	for (std::size_t i=0; i < N_x_INT; ++i) {
		for (std::size_t j=0; j < N_y_INT; ++j) {
			tmp_AXD = x_MTX.row(i) - y_MTX.row(j);
			d_MTX(i,j) = (tmp_AXD.square().sum() * y_weights[i]);
		}
	}
	// wss
	double wss_DBL = d_MTX.colwise().minCoeff().sum();

	//// Exports
	// return proportion held
	return (1.0 - (wss_DBL / tss_DBL));
}
