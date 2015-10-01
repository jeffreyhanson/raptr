 #include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <Rcpp.h>


// [[Rcpp::export]]
Rcpp::S4 rcpp_collate_model_results() {
	//// Initialization
	Rcpp::DataFrame summaryDF;
	Rcpp::NumericMatrix selectionsMTX;
	Rcpp::NumericMatrix amontheldMTX;
	Rcpp::NumericMatrix occheldMTX;
	Rcpp::NumericMatrix spaceheldMTX;
	Rcpp::NumericMatrix amounttargetsheldMTX;
	Rcpp::NumericMatrix spacetargetsheldMTX;
	Rcpp::NumericMatrix getsheldMTX;
	int bestINT;
	CharacterVector logfileCHR;
	CharacterVector modelfileCHR;
	Environment cacheENV;
	
	//// Preliminary processing
	
	
	
	
	//// Main processing
	
	
	
	//// Exports
	Rcpp::S4 ret("RaspResults");
	ret.slot("summary") = summaryDF;
	ret.slot("selections") = selectionsMTX;
	ret.slot("amount.held") = amontheldMTX;
	ret.slot("occ.held") = occheldMTX;
	ret.slot("space.held") = spaceheldMTX;
	ret.slot("amount.targets.met") = amounttargetsheldMTX;
	ret.slot("space.targets.met") = spacetargetsheldMTX;
	ret.slot("best") = bestINT;
	ret.slot("log.file") = logfileCHR;
	ret.slot("model.file") = modelfileCHR;
	ret.slot(".cache") = cacheENV;
}

 

