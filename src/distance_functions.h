#ifndef DISTANCE_FUNCTIONS_H
#define DISTANCE_FUNCTIONS_H

#include <Rcpp.h>
using namespace Rcpp;

#include <vector>
#include <math.h>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <RcppEigen.h>

/* 
The distance functions implemented here are based on those in vegan::vegdist. 

vegan::vegdist accepts a single matrix of points and computes all
distances between them. The functions, however, accept two matrices
of points and compute distances between points in each matrix
(and not points in the same matrix).

The source code for the distance functions in vegan::vegdist is here:

http://github.com/vegandevs/vegan/blob/master/src/vegdist.c
  
*/

inline void euclidean_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// d[ij] = sqrt(sum(x[ik]-x[jk])^2)
	Eigen::ArrayXXd currArray;
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray=pu_coords.row(pu_ids[j]) - dp_coords.row(i);
			dist_matrix(i,j) = std::sqrt(currArray.square().sum());
		}
	}
}

inline void bray_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// d[ij] = (sum abs(x[ik]-x[jk])) / (sum (x[ik]+x[jk]))
	Eigen::ArrayXXd currArray1;
	Eigen::ArrayXXd currArray2;
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray1=pu_coords.row(pu_ids[j])-dp_coords.row(i);
			currArray2=pu_coords.row(pu_ids[j])+dp_coords.row(i);
			dist_matrix(i,j) = (currArray1.abs().sum()) / (currArray2.sum());
		}
	}
}

inline void manhattan_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// d[ij] = sum(abs(x[ik] - x[jk]))
	Eigen::ArrayXXd currArray;
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray=pu_coords.row(pu_ids[j])-dp_coords.row(i);
			dist_matrix(i,j) = currArray.abs().sum();
		}
	}
}

inline void gower_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	/// d[ij] = (1/M) sum(abs(x[ik]-x[jk])/(max(x[k])-min(x[k])))
	/// where M is the number of columns (excluding missing values)
	
	// init
	double m;
	double curr_min;
	double curr_max;
	double curr_diff;
	Eigen::ArrayXXd currArray;
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> pu_coords_sub;
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> pu_coords_zs;
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> dp_coords_zs;
	pu_coords_sub.resize(pu_ids.size(), pu_coords.cols());
	pu_coords_zs.resize(pu_ids.size(), pu_coords.cols());
	dp_coords_zs.resize(dp_coords.rows(), dp_coords.cols());
	// copy across relevant pus
	for (std::size_t j=0; j<pu_ids.size(); ++j) {
		pu_coords_sub.row(j)=pu_coords.row(pu_ids[j]);
	}
	// standardize values to between zero and one
	for (std::size_t k=0; k<pu_coords_zs.cols(); ++k) {
		// init
		curr_min = std::min(pu_coords_sub.col(k).array().minCoeff(), dp_coords.col(k).array().minCoeff());
		curr_max = std::max(pu_coords_sub.col(k).array().maxCoeff(), dp_coords.col(k).array().maxCoeff());
		curr_diff = curr_max - curr_min;
		if (curr_min < 0.0) {
			m=curr_min;
		} else {
			m=std::numeric_limits<double>::epsilon();
		}
		curr_diff = std::max(curr_diff, m);
		// update pus
		pu_coords_zs.col(k).array()=(pu_coords_sub.col(k).array() - curr_min);
		pu_coords_zs.col(k).array()/=curr_diff;
		// update dps
		dp_coords_zs.col(k).array()=(dp_coords.col(k).array() - curr_min);
		dp_coords_zs.col(k).array()/=curr_diff;
	}
	
	// calculate differences
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray=pu_coords_zs.row(j)-dp_coords_zs.row(i);
			dist_matrix(i,j)=currArray.abs().sum();
		}
	}
	dist_matrix/=static_cast<double>(currArray.size());
}

inline void canberra_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// d[jk] = sum (abs(x[ij]-x[ik])/(abs(x[ij])+abs(x[ik])))             
	Eigen::ArrayXXd currArray1;
	Eigen::ArrayXXd currArray2;
	Eigen::ArrayXXd currArray3;
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray1=pu_coords.row(pu_ids[j]) - dp_coords.row(i);
			currArray2=pu_coords.row(pu_ids[j]);
			currArray3=dp_coords.row(i);
			currArray1=(currArray1.abs()) / (currArray2.abs() + currArray3.abs());
			dist_matrix(i,j)=currArray1.sum();
		}
	}
}

inline void jaccard_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// Jaccard index is computed as 2B/(1+B), where B is Bray-Curtis dissimilarity.
	bray_distance(pu_ids, pu_coords, dp_coords, dist_matrix);
	dist_matrix=(2.0*dist_matrix.array())/(1.0+dist_matrix.array());
}

inline void kulczynski_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// d[jk] 1 - 0.5*((sum min(x[ij],x[ik])/(sum x[ij]) + (sum min(x[ij],x[ik])/(sum x[ik]))
	double currVal1;
	double currVal2;
	double currVal3;
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currVal1=0.0;
			currVal2=0.0;
			currVal3=0.0;
			for (std::size_t k=0; k<dp_coords.cols(); ++k) {
				currVal1+=std::min(pu_coords(pu_ids[j],k),dp_coords(i,k));
				currVal2+=pu_coords(pu_ids[j],k);
				currVal3+=dp_coords(i,k);
			}
			dist_matrix(i,j) = 1.0 - (currVal1/currVal2/2.0) - (currVal1/currVal3/2.0);
		}
	}
}


inline void mahalanobis_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// init
	double total_rows=static_cast<double>(dp_coords.rows()+pu_ids.size());
	Eigen::MatrixXd inv_cov;
	Eigen::MatrixXd total_centered(pu_ids.size() + dp_coords.rows(), dp_coords.cols());
	Eigen::MatrixXd currDiffMat(1, dp_coords.cols());
	
	// create big matrix with pu's + dp's
	std::size_t x=0;
	for (std::size_t i=0; i<dp_coords.rows(); ++i, ++x)
		total_centered.row(x) = dp_coords.row(i);
	for (std::size_t j=0; j<pu_ids.size(); ++j, ++x)
		total_centered.row(x) = pu_coords.row(pu_ids[j]);
	
	// centre matrix
	total_centered.rowwise() -= total_centered.colwise().mean();	
	
	// generate covariance matrices
	inv_cov = (total_centered.adjoint() * total_centered) / (total_rows-1.0);
	inv_cov=inv_cov.inverse();
	
	// calculate distances
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currDiffMat.row(0) = total_centered.row(j+dp_coords.rows()) - total_centered.row(i);
			currDiffMat=currDiffMat * inv_cov * currDiffMat;
			dist_matrix(i,j) = std::sqrt(currDiffMat(0,0));
		}
	}
}

inline void minkowski_distance(
		std::vector<std::size_t> &pu_ids,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &pu_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dp_coords,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &dist_matrix
) {
	// d[ij] = sum(abs(x[ik]-x[jk])^d)^(1/d)
	Eigen::ArrayXXd currArray;
	double d=static_cast<double>(pu_coords.cols());
	double d1=1.0/d;
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray=pu_coords.row(pu_ids[j]) - dp_coords.row(i);
			dist_matrix(i,j) = std::pow(currArray.abs().pow(d).sum(), d1);
		}
	}
}

#endif
