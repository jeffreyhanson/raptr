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
Rcpp::S4 rcpp_extract_model_object(Rcpp::S4 opts, bool unreliable_formulation, Rcpp::S4 data, Rcpp::List model, std::vector<std::string> logging_file, Rcpp::List solution) {
 //// Initialization
 // variables to store model data
 std::size_t n_attribute_spaces;
 std::size_t n_pu;
 std::size_t n_edges;
 std::size_t n_species;
 std::vector<std::size_t> n_demand_points;
 double zero_adjust=1.e-05;

 // return variables
 double Score;
 double Cost;
 double Planning_Units;
 double Connectivity_Total;
 double Connectivity_In;
 double Connectivity_Out;
 double Connectivity_Edge;
 double Connectivity_In_Fraction;
 Environment cacheENV = Environment::base_env();
 const double boundary_threshold=1.0e-05;

 //// Preliminary processing
 Rcpp::checkUserInterrupt();
 /// extract parameters from Rcpp::S4 opts
 double blmDBL=Rcpp::as<double>(opts.slot("BLM"));
 double failure_multiplier;
 std::size_t maxrlevelINT;
 if (!unreliable_formulation) {
   failure_multiplier=Rcpp::as<double>(opts.slot("FAILUREMULTIPLIER"));
   maxrlevelINT=Rcpp::as<double>(opts.slot("MAXRLEVEL"));
 }

 /// extract data from Rcpp::S4 data
 // species data
// 	Rcpp::Rcout << "\tSpecies data" << std::endl;
 Rcpp::DataFrame speciesDF=Rcpp::as<Rcpp::DataFrame>(data.slot("species"));
 n_species=speciesDF.nrows();

 // planning unit data
// 	Rcpp::Rcout << "\tpu data" << std::endl;
 Rcpp::DataFrame puDF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu"));
 std::vector<double> puDF_area = puDF["area"];
 std::vector<double> puDF_cost = puDF["cost"];
 std::vector<std::size_t> puDF_status = puDF["status"];
 n_pu=puDF_area.size();

 // pu.species.probabilities
// 	Rcpp::Rcout << "\tpuvspecies data" << std::endl;
 Rcpp::DataFrame puvspeciesDF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu.species.probabilities"));
 std::vector<std::size_t> puvspeciesDF_pu = puvspeciesDF["pu"];
 std::vector<std::size_t> puvspeciesDF_species = puvspeciesDF["species"];
 std::vector<double> puvspeciesDF_value = puvspeciesDF["value"];
 for (std::size_t i=0; i<puvspeciesDF_pu.size(); ++i) {
	 puvspeciesDF_pu[i]-=1;
	 puvspeciesDF_species[i]-=1;
 }

 // boundary
// 	Rcpp::Rcout << "\tboundary data" << std::endl;
 Rcpp::DataFrame boundaryDF=Rcpp::as<Rcpp::DataFrame>(data.slot("boundary"));
 std::vector<std::size_t> boundaryDF_id1 = boundaryDF["id1"];
 std::vector<std::size_t> boundaryDF_id2 = boundaryDF["id2"];
 std::vector<double> boundaryDF_boundary = boundaryDF["boundary"];
 n_edges=boundaryDF_boundary.size();
 for (std::size_t i=0; i<n_edges; ++i) {
	 boundaryDF_id1[i]-=1;
	 boundaryDF_id2[i]-=1;
 }

 /// attribute.space
// 	Rcpp::Rcout << "\tattribute space data" << std::endl;
 Rcpp::List attributespaceLST=Rcpp::as<Rcpp::List>(data.slot("attribute.spaces"));
 n_attribute_spaces=attributespaceLST.size();
 Rcpp::S4 currS4;
 Rcpp::S4 currS4_2;
 Rcpp::List currLST;

 // planning unit points
// 	Rcpp::Rcout << "\tpu points data" << std::endl;
 Rcpp::NumericMatrix tmp;
 std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> pupointsMTX(n_attribute_spaces);
 for (std::size_t i=0; i<n_attribute_spaces; ++i) {
	 currS4=Rcpp::as<Rcpp::S4>(Rcpp::as<Rcpp::S4>(attributespaceLST[i]).slot("pu"));
	 tmp=Rcpp::as<Rcpp::NumericMatrix>(currS4.slot("coords"));
	 double *pcv = &tmp(0,0);
	 Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> tmpmat(pcv, tmp.nrow(), tmp.ncol());
	 pupointsMTX[i] = tmpmat;

 }

 // demand points
// 	Rcpp::Rcout << "\tdemand points data" << std::endl;
 Eigen::Matrix<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> demandpoints_coords_MTX(n_species, n_attribute_spaces);
 Eigen::Matrix<NumericVector, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> demandpoints_weights_MTX(n_species, n_attribute_spaces);
 Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> currCoordinates;
 Rcpp::NumericVector currWeights;
 std::vector<std::size_t> species_ndp(n_species);
 for (std::size_t j=0; j<n_attribute_spaces; ++j) {
	 currLST=Rcpp::as<Rcpp::S4>(attributespaceLST[j]).slot("dp");
	 for (std::size_t i=0; i<n_species; ++i) {
		 // extract species i demand points for space j
		 currS4=Rcpp::as<Rcpp::S4>(currLST[i]);
		 currS4_2=Rcpp::as<Rcpp::S4>(currS4.slot("points"));

		 // coords
		 tmp=Rcpp::as<Rcpp::NumericMatrix>(currS4_2.slot("coords"));
		 double *pcv = &tmp(0,0);
		 Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> tmpmat(pcv, tmp.nrow(), tmp.ncol());
		 demandpoints_coords_MTX(i,j) = tmpmat;

		 // weights
		 demandpoints_weights_MTX(i,j) = Rcpp::as<NumericVector>(currS4.slot("weights"));

		 // store number dp for species i
		 species_ndp[i]=demandpoints_weights_MTX(i,j).size();
	 }
 }

 // target data
 Rcpp::DataFrame targetDF=Rcpp::as<Rcpp::DataFrame>(data.slot("targets"));
 std::vector<std::size_t> targetDF_species = targetDF["species"];
 std::vector<std::size_t> targetDF_target = targetDF["target"];
 std::vector<std::size_t> targetDF_value = targetDF["proportion"];
 Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor> spacetargetsMTX(n_species, n_attribute_spaces);
 spacetargetsMTX.setZero(n_species, n_attribute_spaces);
 std::vector<double> areatargetsDBL(n_species, 0.0);
 for (std::size_t i=0; i<targetDF_species.size(); ++i) {
	 if (targetDF_target[i]==0) {
		 areatargetsDBL[targetDF_species[i]-1] = targetDF_value[i];
	 } else {
		 spacetargetsMTX(targetDF_species[i]-1, targetDF_target[i]-1) = targetDF_value[i];
	 }
 }

 // cache integer string conversions
 std::size_t maxINT;
 maxINT=std::max(
	 n_pu,
	 std::max(
		 n_species,
		 std::max(
			 n_attribute_spaces,
			 *std::max_element(
				 species_ndp.begin(),
				 species_ndp.end()
			 )
		 )
	 )
 ) + 2;
 std::vector<std::string> intSTR(maxINT);
 for (std::size_t i=0; i<maxINT; i++)
	 intSTR[i] = std::to_string(i);

   // load cached data
 Rcpp::List cacheLST=model["cache"];
 std::vector<double> best_amountheld = cacheLST["best_amount_values"];
 Eigen::Map<Eigen::MatrixXd> best_speciesspaceMTX = Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(Rcpp::as<Rcpp::NumericMatrix>(cacheLST["best_space_values"]));
 Eigen::Map<Eigen::MatrixXd> worst_speciesspaceMTX = Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(Rcpp::as<Rcpp::NumericMatrix>(cacheLST["worst_space_values"]));


 //// Main processing
 /// simple vars
 // extract selections
 Rcpp::IntegerMatrix selectionsMTX(1, n_pu);
 Rcpp::IntegerVector solutions=solution["x"];
 for (std::size_t i=0; i<n_pu; ++i) {
	 selectionsMTX(0, i)=solutions[i];
	 Planning_Units+=selectionsMTX(0, i);
	 Cost+=(selectionsMTX(0, i) * puDF_cost[i]);
 }

 //  extract amountheld
 Rcpp::NumericMatrix amountheldMTX(1, n_species);
 for (std::size_t i=0; i<puvspeciesDF_pu.size(); ++i)
	 if (selectionsMTX(0, puvspeciesDF_pu[i])>0)
		 amountheldMTX(0, puvspeciesDF_species[i]) = amountheldMTX(0, puvspeciesDF_species[i]) + puvspeciesDF_value[i] * puDF_area[puvspeciesDF_pu[i]];
 for (std::size_t i=0; i<n_species; ++i)
	 amountheldMTX(0, i)=amountheldMTX(0, i)  / best_amountheld[i];

 // extract spaceheld
 // ini
 std::vector<std::vector<std::size_t>> species_pu_ids(n_species);
 std::vector<std::vector<std::size_t>> selected_species_pu_ids(n_species);
 std::vector<std::vector<std::size_t>> selected_species_pu_pos(n_species);
 std::vector<std::vector<double>> selected_species_pu_probs(n_species);
 std::vector<std::vector<double>> species_pu_probs(n_species);
 std::vector<std::size_t> species_npu(n_species);
 std::size_t n_sel;
 for (std::size_t i=0; i<n_species; ++i) {
   n_sel=std::accumulate(selectionsMTX.begin(), selectionsMTX.end(), 0);
	 species_pu_ids[i].reserve(puvspeciesDF_pu.size());
	 selected_species_pu_pos[i].reserve(n_sel);
	 selected_species_pu_ids[i].reserve(n_sel);
	 selected_species_pu_probs[i].reserve(n_sel);
	 species_pu_probs[i].reserve(puvspeciesDF_pu.size());
 }

 // calcs
// 	Rcpp::Rcout << "\t\tcalculate numbers" << std::endl;
 for (std::size_t i=0; i<puvspeciesDF_pu.size(); ++i) {
	 // assign pu to species vector
	 species_pu_ids[puvspeciesDF_species[i]].push_back(puvspeciesDF_pu[i]);
	 // assign prob to species vector
	 species_pu_probs[puvspeciesDF_species[i]].push_back(puvspeciesDF_value[i]);
 }
 for (std::size_t i=0; i<n_species; ++i) {
	 species_pu_ids[i].shrink_to_fit();
	 species_pu_probs[i].shrink_to_fit();
 }

 // create pu species selections
 for (std::size_t i=0; i<n_species; ++i) {
	 for (std::size_t j=0; j<species_pu_ids[i].size(); ++j) {
		 if (selectionsMTX(0,species_pu_ids[i][j])>0) {
			 selected_species_pu_pos[i].push_back(j);
			 selected_species_pu_ids[i].push_back(species_pu_ids[i][j]);
			 selected_species_pu_probs[i].push_back(species_pu_probs[i][j]);
		 }
	 }
   selected_species_pu_pos[i].shrink_to_fit();
	 selected_species_pu_ids[i].shrink_to_fit();
   selected_species_pu_probs[i].shrink_to_fit();
 }

 // resize vectors
 if (unreliable_formulation) {
   for (std::size_t i=0; i<n_species; ++i) {
  	 species_pu_ids[i].push_back(n_pu);
  	 species_pu_probs[i].push_back(1.0);
  	 species_npu[i]=species_pu_ids[i].size()-1;
   }
 }
 /// create distance variables
 /// create distance variables
 Eigen::ArrayXXd currArray;
 Eigen::Matrix<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> weightdistMTX(n_species, n_attribute_spaces);
 double currFailDist;
 if (unreliable_formulation) {
	 for (std::size_t i=0; i<n_species; ++i) {
		 for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			 // resize matrix
			 weightdistMTX(i,j).resize(species_ndp[i],species_npu[i]);
			 // calculate distances
			 for (std::size_t l=0; l<species_npu[i]; ++l) {
				 for (std::size_t k=0; k<species_ndp[i]; ++k) {
					 currArray=pupointsMTX[j].row(species_pu_ids[i][l]) - demandpoints_coords_MTX(i,j).row(k);
					 weightdistMTX(i,j)(k,l) = zero_adjust + (demandpoints_weights_MTX(i,j)[k] * std::sqrt(currArray.square().sum()));
				 }
			 }
		 }
	 }
 } else {
	 for (std::size_t i=0; i<n_species; ++i) {
 		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
 			// resize matrix
 			weightdistMTX(i,j).resize(species_ndp[i],species_npu[i]+1);
 			// calculate distances
 			for (std::size_t l=0; l<species_npu[i]; ++l) {
 				for (std::size_t k=0; k<species_ndp[i]; ++k) {
 					currArray=pupointsMTX[j].row(species_pu_ids[i][l]) - demandpoints_coords_MTX(i,j).row(k);
 					weightdistMTX(i,j)(k,l) = zero_adjust + (demandpoints_weights_MTX(i,j)[k] * std::sqrt(currArray.square().sum()));
 				}
 			}
 			// failure pu
			weightdistMTX(i,j).col(species_npu[i]).setZero();
 			currFailDist=weightdistMTX(i,j).maxCoeff() * failure_multiplier;
 			for (std::size_t k=0; k<species_ndp[i]; ++k) {
 				weightdistMTX(i,j)(k,species_npu[i]) = currFailDist;
 			}
 		}
 	}
 }

 /// calculate spatial representation metrics
 // 	Rcpp::Rcout << "\tspaceheld vars" << std::endl;
 Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> spaceheldMTX(1, n_species*n_attribute_spaces);
 std::size_t currCol=-1;
	if (unreliable_formulation) {
	 for (std::size_t i=0; i<n_species; ++i) {
		 for (std::size_t j=0; j<n_attribute_spaces; ++j) {
       spaceheldMTX(0, currCol)=(
         (unreliable_space_value(weightdistMTX(i,j),selected_species_pu_pos[i])-best_speciesspaceMTX(i,j)) /
         (worst_speciesspaceMTX(i,j)-best_speciesspaceMTX(i,j))
       );
     }
   }
 } else {
	 for (std::size_t i=0; i<n_species; ++i) {
 		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
      spaceheldMTX(0, currCol)=(
        (
            reliable_space_value(
              weightdistMTX(i,j),
              selected_species_pu_pos[i],
              selected_species_pu_probs[i],
              maxrlevelINT
            )- best_speciesspaceMTX(i,j)
        ) / (worst_speciesspaceMTX(i,j)-best_speciesspaceMTX(i,j))
      );
    }
   }
 }

 /// calculated vars
 // extract summaryDF
 std::vector<double>Connectivity=calculateConnectivity(boundaryDF_id1, boundaryDF_id2, boundaryDF_boundary, selectionsMTX);

 //// Exports
 Rcpp::S4 ret("RaspResults");
 ret.slot("summary") = Rcpp::DataFrame::create(
	 Rcpp::Named("Run_Number") = Rcpp::wrap(1),
	 Rcpp::Named("Score")= Rcpp::wrap(solution["objval"]),
	 Rcpp::Named("Cost")= Rcpp::wrap(Cost),
	 Rcpp::Named("Planning_Units")= Rcpp::wrap(Planning_Units),
	 Rcpp::Named("Connectivity_Total")= std::accumulate(Connectivity.begin(), Connectivity.end(), 0.0),
	 Rcpp::Named("Connectivity_In")= Connectivity[0],
	 Rcpp::Named("Connectivity_Edge")= Connectivity[1],
	 Rcpp::Named("Connectivity_Out")= Connectivity[2],
	 Rcpp::Named("Connectivity_In_Fraction")= Connectivity[0] / (Connectivity[0]+Connectivity[1]+Connectivity[2])
 );
 ret.slot("selections") = selectionsMTX;
 ret.slot("amount.held") = amountheldMTX;
 ret.slot("space.held") = Rcpp::wrap(spaceheldMTX);
 ret.slot("logging.file") = Rcpp::wrap(logging_file);
 ret.slot("best") = 1;
 ret.slot(".cache") = cacheENV;
 return(ret);
}
