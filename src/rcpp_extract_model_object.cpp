#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;


#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <RcppEigen.h>
#include <Rcpp.h>
#include "functions.h"

// [[Rcpp::export]]
Rcpp::S4 rcpp_extract_model_object(Rcpp::S4 opts, bool unreliable_formulation, Rcpp::S4 data, Rcpp::List model, std::vector<std::string> logging_file, Rcpp::List solution, bool verbose) {
 //// Initialization
 if (verbose) Rcout << "Initialization" << std::endl;
 // variables to store model data
 std::size_t n_attribute_spaces_INT;
 std::size_t n_pu_INT;
 std::size_t n_edges_INT;
 std::size_t n_species_INT;
 double zero_adjust=1.e-05;
 Rcpp::List cache_LST=model["cache"];

 // return variables
 double Score;
 double Cost=0.0;
 std::size_t Planning_Units=0.0;
 Environment cacheENV = Environment::base_env();

 //// Preliminary processing
 if (verbose) Rcout << "Preliminary processing" << std::endl;
 Rcpp::checkUserInterrupt();
 /// extract parameters from Rcpp::S4 opts
 if (verbose) Rcout << "\tRapOpts" << std::endl;
 double blmDBL=Rcpp::as<double>(opts.slot("BLM"));
 double failure_multiplier_DBL=0.0;
 std::size_t maxrlevel_INT=0;
 if (!unreliable_formulation) {
   failure_multiplier_DBL=Rcpp::as<double>(opts.slot("failure.multiplier"));
   maxrlevel_INT=Rcpp::as<double>(opts.slot("max.r.level")); 
 }
 
 /// extract data from Rcpp::S4 data
 // species data
 if (verbose) Rcout << "\tspecies data" << std::endl;
 Rcpp::DataFrame species_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("species"));
 n_species_INT=species_DF.nrows();

 // planning unit data
 if (verbose) Rcout << "\tplanning unit data" << std::endl;
 Rcpp::DataFrame pu_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu"));
 std::vector<double> pu_DF_area = pu_DF["area"];
 std::vector<double> pu_DF_cost = pu_DF["cost"];
 std::vector<std::size_t> pu_DF_status = pu_DF["status"];
 n_pu_INT=pu_DF_area.size();

 // pu.species.probabilities
 if (verbose) Rcout << "\tpu.species.probabilities data" << std::endl;
 Rcpp::DataFrame puvspecies_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu.species.probabilities"));
 std::vector<std::size_t> puvspecies_DF_pu = puvspecies_DF["pu"];
 std::vector<std::size_t> puvspecies_DF_species = puvspecies_DF["species"];
 std::vector<double> puvspecies_DF_value = puvspecies_DF["value"];
 for (std::size_t i=0; i<puvspecies_DF_pu.size(); ++i) {
	 puvspecies_DF_pu[i]-=1;
	 puvspecies_DF_species[i]-=1;
 }

 // boundary
 if (verbose) Rcout << "\tcurrLSTboundary data" << std::endl;
 Rcpp::DataFrame boundary_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("boundary"));
 std::vector<std::size_t> boundary_DF_id1 = boundary_DF["id1"];
 std::vector<std::size_t> boundary_DF_id2 = boundary_DF["id2"];
 std::vector<double> boundary_DF_boundary = boundary_DF["boundary"];
 n_edges_INT=boundary_DF_boundary.size();
 for (std::size_t i=0; i<n_edges_INT; ++i) {
	 boundary_DF_id1[i]-=1;
	 boundary_DF_id2[i]-=1;
 }

	// target data
	if (verbose) Rcpp::Rcout << "\ttarget data" << std::endl;
	Rcpp::DataFrame target_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("targets"));
	std::vector<std::size_t> target_DF_species = target_DF["species"];
	std::vector<std::size_t> target_DF_target = target_DF["target"];
	std::vector<double> target_DF_value = target_DF["proportion"];
	
 /// attribute.space
 if (verbose) Rcout << "\tattribute space data" << std::endl;
 Rcpp::List attributespace_LST=Rcpp::as<Rcpp::List>(data.slot("attribute.spaces"));
 n_attribute_spaces_INT=attributespace_LST.size();

 // cache integer string conversions
 std::size_t maxINT;
 maxINT=std::max(
	 n_pu_INT,
	 std::max(
		 n_species_INT,
		 n_attribute_spaces_INT
		)
	) + 2;
 std::vector<std::string> intSTR(maxINT);
 for (std::size_t i=0; i<maxINT; i++)
	 intSTR[i] = num2str<std::size_t>(i);

 /// load cached data
 if (verbose) Rcout << "\tcached data" << std::endl;

 // create distance variables
	std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> species_space_weightdist_MTX = *Rcpp::as<Rcpp::XPtr<std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>>>(cache_LST["wdist"]);
	std::size_t n_species_attributespace_INT = species_space_weightdist_MTX.size();
	

	// store ids for attribute spaces and species
	std::vector<std::size_t> species_attributespace_species_INT=*Rcpp::as<Rcpp::XPtr<std::vector<std::size_t>>>(cache_LST["species_attributespace_species_INT"]);
	std::vector<std::size_t> species_attributespace_space_INT=*Rcpp::as<Rcpp::XPtr<std::vector<std::size_t>>>(cache_LST["species_attributespace_space_INT"]);
	
	// load best values
	std::vector<double> species_totalarea_DBL = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(cache_LST["species_totalarea_DBL"]);
	std::vector<double> species_areatargets_DBL = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(cache_LST["species_areatargets_DBL"]);
	std::vector<double> species_space_tss_DBL = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(cache_LST["species_space_tss_DBL"]);
	std::vector<double> species_space_rawtargets_DBL = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(cache_LST["species_space_rawtargets_DBL"]);
	std::vector<double> species_space_proptargets_DBL = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(cache_LST["species_space_proptargets_DBL"]);
	std::vector<double> species_space_best_DBL = *Rcpp::as<Rcpp::XPtr<std::vector<double>>>(cache_LST["species_space_best_DBL"]);

 //// Main processing
 if (verbose) Rcout << "Main processing" << std::endl;
 /// simple vars
 // extract selections
 if (verbose) Rcout << "\tselections" << std::endl;
 Rcpp::IntegerMatrix selections_MTX(1, n_pu_INT);
 Rcpp::IntegerVector solutions_RIV=solution["x"];
 for (std::size_t i=0; i<n_pu_INT; ++i) {
	 selections_MTX(0, i)=solutions_RIV[i];
	 Planning_Units+=selections_MTX(0, i);
	 Cost+=(selections_MTX(0, i) * pu_DF_cost[i]);
 }
 
 //  extract amountheld
 if (verbose) Rcout << "\tamount held" << std::endl;
 Rcpp::NumericMatrix species_amountheld_MTX(1, n_species_INT);
 for (std::size_t i=0; i<puvspecies_DF_pu.size(); ++i)
	 if (selections_MTX(0, puvspecies_DF_pu[i])>0)
		 species_amountheld_MTX(0, puvspecies_DF_species[i]) = species_amountheld_MTX(0, puvspecies_DF_species[i]) + puvspecies_DF_value[i] * pu_DF_area[puvspecies_DF_pu[i]];
 for (std::size_t i=0; i<n_species_INT; ++i)
	 species_amountheld_MTX(0, i)=species_amountheld_MTX(0, i)  / species_totalarea_DBL[i];

 // extract spaceheld
 if (verbose) Rcout << "\tspace held" << std::endl;
 if (verbose) Rcout << "\t\trestoring variables from cache" << std::endl;
 // restore variables from cache
  std::vector<std::size_t> species_space_rlevel_INT = *Rcpp::as<Rcpp::XPtr<std::vector<std::size_t>>>(cache_LST["species_space_rlevel_INT"]);
	std::vector<Rcpp::NumericVector> species_space_pu_probs_RDV = *Rcpp::as<Rcpp::XPtr<std::vector<Rcpp::NumericVector>>>(cache_LST["species_space_pu_probs_RDV"]);
	std::vector<Rcpp::IntegerVector> species_space_puids_RIV = *Rcpp::as<Rcpp::XPtr<std::vector<Rcpp::IntegerVector>>>(cache_LST["species_space_puids_RIV"]);
	std::vector<Rcpp::IntegerVector> species_space_pupos_RIV = *Rcpp::as<Rcpp::XPtr<std::vector<Rcpp::IntegerVector>>>(cache_LST["species_space_pupos_RIV"]);
	std::size_t n_selected_INT=std::accumulate(selections_MTX.begin(), selections_MTX.end(), 0);
 
	// subset variables to those included in the solution
	std::vector<std::vector<std::size_t>> selected_species_space_puids_INT(n_species_attributespace_INT);
	std::vector<std::vector<std::size_t>> selected_species_space_pupos_INT(n_species_attributespace_INT);
	for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
		for (std::size_t l=0; l<species_space_puids_RIV[a].size(); ++l) {
			if (selections_MTX(0,species_space_puids_RIV[a][l])>0) {
				selected_species_space_pupos_INT[a].push_back(l);
				selected_species_space_puids_INT[a].push_back(species_space_puids_RIV[a][l]);
			}
		}
	}
		
	// calculate spatial representation metrics
 if (verbose) Rcout << "\t\tcalculating representation props." << std::endl;
 Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> species_space_propheld_MTX(1, n_species_INT*n_attribute_spaces_INT);
 species_space_propheld_MTX.fill(NAN);
	if (unreliable_formulation) {
		for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
			species_space_propheld_MTX(
				0,
				(species_attributespace_species_INT[a]*n_attribute_spaces_INT)+species_attributespace_space_INT[a]
			) = ( 
				1.0-(
					unreliable_space_value(
						species_space_weightdist_MTX[a],
						selected_species_space_pupos_INT[a]
					) / species_space_tss_DBL[a]
				)
			);
		}
  } else {
		for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
			species_space_propheld_MTX(
				0,
				(species_attributespace_species_INT[a]*n_attribute_spaces_INT)+species_attributespace_space_INT[a]
			) = ( 
				1.0-(
					reliable_space_value(
							species_space_weightdist_MTX[a],
							selected_species_space_pupos_INT[a],
							species_space_pu_probs_RDV[a],
							species_space_rlevel_INT[a]
					) / species_space_tss_DBL[a]
				)
			);
		}
	}

 /// calculated vars
 // extract summaryDF
 if (verbose) Rcout << "\tcalculating connectivity data" << std::endl;
 std::vector<double>Connectivity=calculateConnectivity(boundary_DF_id1, boundary_DF_id2, boundary_DF_boundary, selections_MTX);

 // calculate Score
 Score=Rcpp::as<Rcpp::NumericVector>(solution["objval"])[0];
 if (Rcpp::NumericVector::is_na(Score))
  Score=Cost + (blmDBL * Connectivity[2]);

 // extract solution status
 std::string solution_status=solution["status"];
 
 //// Exports
 if (verbose) Rcout << "Exporting data to R" << std::endl;
 Rcpp::S4 ret("RapResults");
 ret.slot("summary") = Rcpp::DataFrame::create(
	 Rcpp::Named("Run_Number") = Rcpp::wrap(1),
	 Rcpp::Named("Status") = Rcpp::wrap(solution_status),
	 Rcpp::Named("Score")=Rcpp::wrap(Score),
	 Rcpp::Named("Cost")= Rcpp::wrap(Cost),
	 Rcpp::Named("Planning_Units")= Rcpp::wrap(Planning_Units),
	 Rcpp::Named("Connectivity_Total")= std::accumulate(Connectivity.begin(), Connectivity.end(), 0.0),
	 Rcpp::Named("Connectivity_In")= Connectivity[0],
	 Rcpp::Named("Connectivity_Edge")= Connectivity[1],
	 Rcpp::Named("Connectivity_Out")= Connectivity[2],
	 Rcpp::Named("Connectivity_In_Fraction")= Connectivity[0] / (Connectivity[0]+Connectivity[1]+Connectivity[2])
 );
 ret.slot("selections") = selections_MTX;
 ret.slot("amount.held") = species_amountheld_MTX;
 ret.slot("space.held") = Rcpp::wrap(species_space_propheld_MTX);
 ret.slot("logging.file") = Rcpp::wrap(logging_file);
 ret.slot("best") = 1;
 ret.slot(".cache") = cacheENV;
 return(ret);
}
