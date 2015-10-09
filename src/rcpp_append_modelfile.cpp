#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <Rcpp.h>


// split a string by first occurence of delim
std::vector<std::string> &split(const std::string &s, std::string delim, std::vector<std::string> &elems) {
	elems.reserve(2);
	elems.emplace_back(s, 0, s.find(delim, 0));
	elems.emplace_back(s, s.find(delim, 0)+1, s.size());
	return(elems);
}

// [[Rcpp::export]]
std::string rcpp_append_modelfile(std::string model, Rcpp::List existing_sols) {
	//// Initialization
	std::string ret;
	std::string extra_constraints="";
	std::vector<std::string> currSplit;
	//// Preliminary processing
	// split string
	split(model, "\nBinary\n", currSplit);
	
	//// Main processing
	// construct new constraints
	IntegerVector currSolution;
	for (std::size_t i=0; i<existing_sols.size(); ++i) {
		// get current solution
		currSolution=existing_sols[i];
		for (std::size_t j=0; j<currSolution.size(); ++j)
			if (currSolution[j]==1)
				extra_constraints+="pu_"+std::to_string(j)+" + ";
		extra_constraints.erase(extra_constraints.size()-3);
		for (std::size_t j=0; j<currSolution.size(); ++j)
			if (currSolution[j]==0)
				extra_constraints+=" - pu_"+std::to_string(j);
		extra_constraints+=" <= "+std::to_string(std::accumulate(currSolution.begin(), currSolution.end(), 0)-1)+"\n";
	}
	
	// stitch model components togeather
	ret=currSplit[0] + "\n" + extra_constraints + currSplit[1];
	
	//// Exports
	return(ret);
}

 
