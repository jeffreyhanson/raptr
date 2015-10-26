#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include <Rcpp.h>
using namespace Rcpp;

#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <RcppEigen.h>

std::vector<double> calculateConnectivity(
  std::vector<std::size_t>&,
  std::vector<std::size_t>&,
  std::vector<double>&,
  IntegerMatrix&
);

// unreliable - best space value
double unreliable_space_value(
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&
);

// unreliable - space value for a prioritisation with 1 pu
double unreliable_space_value(
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&,
	std::size_t
);

// unreliable - space value for a prioritisation with n pu's
double unreliable_space_value(
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&,
	std::vector<std::size_t>&
);

// reliable - best space value
double reliable_space_value(
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&,
	std::vector<double>&,
  std::size_t
);

// reliable - space value for a prioritisation containing 1 pu
double reliable_space_value(
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&,
	std::size_t,
	double,
	double
);

// reliable - space value for a prioritisation containing n pu's
double reliable_space_value(
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &,
	std::vector<std::size_t>,
	std::vector<double>&,
	std::size_t
);

#endif
