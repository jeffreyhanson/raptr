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

std::vector<double> calculateConnectivity(std::vector<std::size_t>&, std::vector<std::size_t>&, std::vector<double>&, IntegerMatrix&);


#endif
