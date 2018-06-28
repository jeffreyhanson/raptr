#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List rcpp_append_model_object(Rcpp::List model, Rcpp::List existing_sols) {
  //// Initialization
  // extract matrix
  Rcpp::List Ar=model["Ar"];
  std::vector<std::size_t> A_i=Ar["row"];
  std::vector<std::size_t> A_j=Ar["col"];
  std::vector<double> A_x=Ar["value"];

  std::vector<std::string> senseSTR=model["sense"];
  std::vector<double> rhsDBL=model["rhs"];

  //// Main processing
  // construct new constraints
  std::size_t counter=*A_i.rbegin()+1;
  IntegerVector currSolution;
  for (std::size_t i=0; i<static_cast<std::size_t>(existing_sols.size()); ++i) {
    // get current solution
    currSolution=existing_sols[i];
    for (std::size_t j=0; j<static_cast<std::size_t>(currSolution.size()); ++j)
      if (currSolution[j]==1) {
        A_i.push_back(counter);
        A_j.push_back(j);
        A_x.push_back(1.0);
      }
    for (std::size_t j=0; j<static_cast<std::size_t>(currSolution.size()); ++j)
      if (currSolution[j]==0) {
        A_i.push_back(counter);
        A_j.push_back(j);
        A_x.push_back(-1.0);
      }

    senseSTR.push_back("<=");
    rhsDBL.push_back(std::accumulate(currSolution.begin(), currSolution.end(), 0)-1);
    ++counter;
  }

  //// Exports
  model["Ar"] = Rcpp::List::create(
    Rcpp::Named("row") = Rcpp::wrap(A_i),
    Rcpp::Named("col") = Rcpp::wrap(A_j),
    Rcpp::Named("value") = Rcpp::wrap(A_x)
  );
  model["rhs"] = Rcpp::wrap(rhsDBL);
  model["sense"] = Rcpp::wrap(senseSTR);
  return(model);
}
