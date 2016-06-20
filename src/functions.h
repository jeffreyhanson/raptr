#ifndef FUNCTIONS_H
#define FUNCTIONS_H

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
	Rcpp::NumericVector&,
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
	Rcpp::NumericVector&,
	std::size_t
);

// power function, templated to use compile-time loop unrolling
template<int P>
inline double Pow(double x) {
	return (Pow<P-1>(x) * x);
}

template<>
inline double Pow<1>(double x) {
	return (x);
}

template<>
inline double Pow<0>(double x) {
	return (1.0);
}

// calculate euclidean distance
double distance(double, double, double, double);

// convert object to string
template<typename T>
inline std::string num2str(T number, int precision=10) {
	std::ostringstream ss;
	ss << std::fixed << std::setprecision(precision) << number;
	return(ss.str());
}

// remove duplicate values in a vector
template<typename T>
void remove_duplicates(std::vector<T> &v) {
	v.shrink_to_fit();
	sort(v.begin(), v.end());
	v.erase(unique(v.begin(), v.end()), v.end());
	v.shrink_to_fit();
}


#endif
