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

The distance functions implemented here are based on those implemented in vegan::vegdist. 

vegan::vegdist accepts a single matrix of points and computes all
distances between them. These functions, however, accept two matrices
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
			dist_matrix(i,j) = (currArray1.abs().sum()+1.0e-5) / (currArray2.sum()+1.0e-5);
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
	Eigen::ArrayXXd currArray1;
	Eigen::ArrayXXd currArray2;
	double inverse_M=1.0/static_cast<double>(dp_coords.cols());
	// find minimum and maximum values
	Eigen::ArrayXXd currMax=dp_coords.colwise().maxCoeff();
	Eigen::ArrayXXd currMin=dp_coords.colwise().minCoeff();
	for (std::size_t j=0; j<pu_ids.size(); ++j) {
		currArray1=pu_coords.row(pu_ids[j]);
		for (std::size_t k=0; k<dp_coords.cols(); ++k) {
			currMax(k)=std::max(currMax(k), currArray1(k));
			currMin(k)=std::min(currMin(k), currArray1(k));
		}
	}
	currArray1 = (currMax - currMin)+1.0e-5;
	// calculate distances
	for (std::size_t i=0; i<dp_coords.rows(); ++i) {
		for (std::size_t j=0; j<pu_ids.size(); ++j) {
			currArray2=pu_coords.row(pu_ids[j])-dp_coords.row(i);
			currArray2=((currArray2.abs()+1.0e-5) / currArray1);
			dist_matrix(i,j) = inverse_M * currArray2.sum();
		}
	}
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
			currArray1=(currArray1.abs()+1.0e-5) / (currArray2.abs() + currArray3.abs() + 1.0e-5);
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
	Eigen::ArrayXXd currArray;
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
			currVal1+=1.e-5;
			currVal2+=1.e-5;
			currVal3+=1.e-5;
			dist_matrix(i,j)= 1.0 - (0.5 * (
				(currVal1/currVal2) + 
				(currVal1/currVal3)
			));
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
	double total_rows=static_cast<double>(dp_coords.rows()) + static_cast<double>(pu_ids.size());
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


#endif
