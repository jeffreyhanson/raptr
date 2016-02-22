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
#include "functions.h"
#include "distance_functions.h"
#include <RcppEigen.h>
#include <Rcpp.h>


// [[Rcpp::export]]
Rcpp::List rcpp_generate_model_object(Rcpp::S4 opts, bool unreliable_formulation, Rcpp::S4 data, bool verbose) {
	//// Initialization
	if (verbose) Rcpp::Rcout << "Initialization" << std::endl;
	// create variables
	std::size_t counter=0;
	std::size_t n_attribute_spaces;
	std::size_t n_pu;
	std::size_t n_edges;
	std::size_t n_edges2=0;
	std::size_t n_species;
	std::vector<std::size_t> n_demand_points;
	bool boundary;
	double boundary_threshold=1.0e-10;
	double zero_adjust=0.0;
	std::vector<std::string> distance_metrics;

	//// Preliminary processing
	if (verbose) Rcpp::Rcout << "Preliminary processing" << std::endl;
	Rcpp::checkUserInterrupt();
	/// extract parameters from Rcpp::S4 opts
	double blmDBL=Rcpp::as<double>(opts.slot("BLM"));
	boundary = blmDBL > boundary_threshold;
  double failure_multiplier=0.0;
  std::size_t maxrlevelINT=0;
  if (!unreliable_formulation) {
    failure_multiplier=Rcpp::as<double>(opts.slot("failure.multiplier"));
  	maxrlevelINT=Rcpp::as<double>(opts.slot("max.r.level"));
  }

  // assign function pointer for distance calculations
	void (*distPtr)(
		std::vector<std::size_t>&,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&,
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>&
	);
		
	/// extract data from Rcpp::S4 data
	// species data
	if (verbose) Rcpp::Rcout << "\tSpecies data" << std::endl;
	Rcpp::DataFrame speciesDF=Rcpp::as<Rcpp::DataFrame>(data.slot("species"));
	n_species=speciesDF.nrows();
	// planning unit data
	if (verbose) Rcpp::Rcout << "\tpu data" << std::endl;
	Rcpp::DataFrame puDF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu"));
	std::vector<double> puDF_area = puDF["area"];
	std::vector<double> puDF_cost = puDF["cost"];
	std::vector<std::size_t> puDF_status = puDF["status"];
	n_pu=puDF_area.size();
	std::vector<std::string> puDF_id_STR(n_pu+1);
	for (std::size_t i=0; i<n_pu; ++i)
		puDF_id_STR[i]="pu_"+num2str<std::size_t>(i);

	// pu.species.probabilities
	if (verbose) Rcpp::Rcout << "\tpuvspecies data" << std::endl;
	Rcpp::DataFrame puvspeciesDF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu.species.probabilities"));
	std::vector<std::size_t> puvspeciesDF_pu = puvspeciesDF["pu"];
	std::vector<std::size_t> puvspeciesDF_species = puvspeciesDF["species"];
	std::vector<double> puvspeciesDF_value = puvspeciesDF["value"];
	for (std::size_t i=0; i<puvspeciesDF_pu.size(); ++i) {
		puvspeciesDF_pu[i]-=1;
		puvspeciesDF_species[i]-=1;
	}

	// boundary
	if (verbose) Rcpp::Rcout << "\tboundary data" << std::endl;
	Rcpp::DataFrame boundaryDF=Rcpp::as<Rcpp::DataFrame>(data.slot("boundary"));
	std::vector<std::size_t> boundaryDF_id1 = boundaryDF["id1"];
	std::vector<std::size_t> boundaryDF_id2 = boundaryDF["id2"];
	std::vector<double> boundaryDF_boundary = boundaryDF["boundary"];
	n_edges=boundaryDF_boundary.size();
	std::vector<std::size_t> edge_pos;
	std::vector<std::string> boundaryDF_idpair_STR;
	if (boundary) {
		boundaryDF_idpair_STR.reserve(n_edges);
		edge_pos.reserve(n_edges);
		for (std::size_t i=0; i<n_edges; ++i) {
			// convert to base-0 indexing
			boundaryDF_id1[i]-=1;
			boundaryDF_id2[i]-=1;
			// main processing
			if (boundaryDF_id1[i]!=boundaryDF_id2[i]) {
				/// if boundaryDF_id1[i] != boundaryDF_id2[i]
				// store quadratic variable
				boundaryDF_idpair_STR.push_back(
					"pu_" + num2str<std::size_t>(boundaryDF_id1[i]) + "_" + num2str<std::size_t>(boundaryDF_id2[i])
				);
				// cache location of quadratic variable
				edge_pos.push_back(i);
			}
			// increase cost variable with boundary
			puDF_cost[boundaryDF_id1[i]] += (blmDBL * boundaryDF_boundary[i]);
		}
		boundaryDF_idpair_STR.shrink_to_fit();
		n_edges2=boundaryDF_idpair_STR.size();
	}

	/// attribute.space
	if (verbose) Rcpp::Rcout << "\tattribute space data" << std::endl;
	Rcpp::List attributespaceLST=Rcpp::as<Rcpp::List>(data.slot("attribute.spaces"));
	n_attribute_spaces=attributespaceLST.size();
	distance_metrics.reserve(n_attribute_spaces);
	Rcpp::S4 currS4;
	Rcpp::S4 currS4_2;
	Rcpp::List currLST;
	
	// planning unit points
	if (verbose) Rcpp::Rcout << "\tpu points data" << std::endl;
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
	if (verbose) Rcpp::Rcout << "\tdemand points data" << std::endl;
	Eigen::Matrix<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> demandpoints_coords_MTX(n_species, n_attribute_spaces);
	Eigen::Matrix<Eigen::VectorXd, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> demandpoints_weights_MTX(n_species, n_attribute_spaces);
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> currCoordinates;
	Rcpp::NumericVector currWeights;
	Eigen::Matrix<std::size_t, Eigen::Dynamic, Eigen::Dynamic> species_ndp(n_species, n_attribute_spaces);
	for (std::size_t j=0; j<n_attribute_spaces; ++j) {
		currLST=Rcpp::as<Rcpp::S4>(attributespaceLST[j]).slot("demand.points");
		currS4=Rcpp::as<Rcpp::S4>(attributespaceLST[j]);
		distance_metrics.push_back(Rcpp::as<std::string>(currS4.slot("distance.metric")));
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
			Eigen::Map<Eigen::VectorXd> tmpvec(Rcpp::as<Eigen::Map<Eigen::VectorXd>>(currS4.slot("weights")));
			demandpoints_weights_MTX(i,j) = tmpvec;
			// store number dp for species i
			species_ndp(i,j)=tmpvec.size();
		}
	}

	// target data
	if (verbose) Rcpp::Rcout << "\ttarget data" << std::endl;
	Rcpp::DataFrame targetDF=Rcpp::as<Rcpp::DataFrame>(data.slot("targets"));
	std::vector<std::size_t> targetDF_species = targetDF["species"];
	std::vector<std::size_t> targetDF_target = targetDF["target"];
	std::vector<double> targetDF_value = targetDF["proportion"];
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

	/// preprocessing for area targets
	if (verbose) Rcpp::Rcout << "\tpre-process area targets" << std::endl;
	// ini
	if (verbose) Rcpp::Rcout << "\t\tinitializing" << std::endl;
	double currArea;
	std::vector<double> speciesareaDBL(n_species, 0.0);
	std::vector<std::vector<std::size_t>> species_pu_ids(n_species);
	std::vector<std::vector<double>> species_pu_probs(n_species);
	std::vector<std::size_t> species_npu(n_species);
	std::vector<std::size_t> species_rlevel(n_species);
	for (std::size_t i=0; i<n_species; ++i) {
		species_pu_ids[i].reserve(puvspeciesDF_pu.size());
		species_pu_probs[i].reserve(puvspeciesDF_pu.size());
	}

	// calcs
	if (verbose) Rcpp::Rcout << "\t\tcalculate numbers" << std::endl;
	for (std::size_t i=0; i<puvspeciesDF_pu.size(); ++i) {
		// calculate area
		currArea=puvspeciesDF_value[i] * puDF_area[puvspeciesDF_pu[i]];
		// sum area occupied by species
		speciesareaDBL[puvspeciesDF_species[i]]+=currArea;
		// assign pu to species vector
		species_pu_ids[puvspeciesDF_species[i]].push_back(puvspeciesDF_pu[i]);
		// assign prob to species vector
		species_pu_probs[puvspeciesDF_species[i]].push_back(puvspeciesDF_value[i]);
	}
	if (verbose) Rcpp::Rcout << "\t\tresize vectors" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		species_pu_ids[i].shrink_to_fit();
		species_pu_probs[i].shrink_to_fit();
		species_npu[i]=species_pu_ids[i].size();
	}

	/// create centroid variables
	if (verbose) Rcpp::Rcout << "\tdistance vars" << std::endl;
	Eigen::Matrix<Eigen::VectorXd, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> centroidMTX(n_species, n_attribute_spaces);
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			// calculate centroids
			centroidMTX(i,j).resize(demandpoints_coords_MTX(i,j).cols());
			centroidMTX(i,j)=demandpoints_coords_MTX(i,j).colwise().sum();
			centroidMTX(i,j)/=static_cast<double>(species_ndp(i,j));
		}
	}

	if (verbose) Rcpp::Rcout << "\tdistance vars" << std::endl;
	Eigen::Matrix<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> weightdistMTX(n_species, n_attribute_spaces);
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> tss_speciesspaceMTX(n_species, n_attribute_spaces);
	{
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> tmpMTX;
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> tmpMTX2;
		std::vector<std::size_t> tmp_species_pu_ids;
		double currFailDist;
		for (std::size_t i=0; i<n_species; ++i) {
			for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				/// initialization
				// resize matrices
				tmpMTX.resize(pupointsMTX[j].rows()+1, pupointsMTX[j].cols());
				if (unreliable_formulation) {
					weightdistMTX(i,j).resize(species_ndp(i,j),species_npu[i]);
					tmpMTX2.resize(species_ndp(i,j),species_npu[i]+1);
				} else {
					weightdistMTX(i,j).resize(species_ndp(i,j),species_npu[i]+1);
					tmpMTX2.resize(species_ndp(i,j),species_npu[i]+1);
				}
				tmp_species_pu_ids = species_pu_ids[i];
				tmp_species_pu_ids.push_back(pupointsMTX[j].rows());
				// set starting matrix values
				weightdistMTX(i,j).setZero();
				tmpMTX2.setZero();
				tmpMTX.topRows(pupointsMTX[j].rows())=pupointsMTX[j];
				tmpMTX.bottomRows<1>()=centroidMTX(i,j);
				
				/// preliminary processing
				// assign distance metric
				if (distance_metrics[j]=="euclidean") {
					distPtr=&euclidean_distance;
				} else if (distance_metrics[j]=="bray") {
					distPtr=&bray_distance;
				} else if (distance_metrics[j]=="manhattan") {
					distPtr=&manhattan_distance;
				} else if (distance_metrics[j]=="gower") {
					distPtr=&gower_distance;
				} else if (distance_metrics[j]=="canberra") {
					distPtr=&canberra_distance;
				} else if (distance_metrics[j]=="jaccard") {
					distPtr=&jaccard_distance;
				} else if (distance_metrics[j]=="kulczynski") {
					distPtr=&kulczynski_distance;
				} else if (distance_metrics[j]=="mahalanobis") {
					distPtr=&mahalanobis_distance;
				} else {
					Rf_error("distance string not recognised");
				}
				/// calculate distances between demand points and (planning units + demand point centroid)
				// calculate all distances
				(*distPtr)(tmp_species_pu_ids, tmpMTX, demandpoints_coords_MTX(i,j), tmpMTX2);
				// check all distances greater than zero
				if (tmpMTX2.minCoeff() < 0.0) Rcpp::stop("Distances between demand points and planning units for species "+num2str<std::size_t>(i)+" and space "+num2str<std::size_t>(j)+" are negative. You must use a different distance metric or transform your data.");
				// extract distances and lambda to give weighted distances
				weightdistMTX(i,j).leftCols(species_npu[i]) = tmpMTX2.leftCols(species_npu[i]);
				for (std::size_t k=0; k<species_ndp(i,j); ++k)
					weightdistMTX(i,j).row(k)*=demandpoints_weights_MTX(i,j)[k];
				// add small number to avoid zeros
				weightdistMTX(i,j).array()+=zero_adjust;
				// failure pu if reliable formulation
				if (!unreliable_formulation) {
					// failure pu
					currFailDist=weightdistMTX(i,j).maxCoeff() * failure_multiplier;
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						weightdistMTX(i,j)(k,species_npu[i])+=currFailDist;
					}
				}
				tmpMTX2.rightCols(1).array()*=demandpoints_weights_MTX(i,j).array();
				tss_speciesspaceMTX(i,j) = tmpMTX2.rightCols(1).array().square().sum();
			}
		}
	}

  // resize vectors
  if (!unreliable_formulation) {
    for (std::size_t i=0; i<n_species; ++i) {
      species_pu_ids[i].push_back(n_pu);
  		species_pu_ids[i].shrink_to_fit();
  		species_pu_probs[i].push_back(1.0);
  		species_pu_probs[i].shrink_to_fit();
			species_rlevel[i]=std::min(maxrlevelINT, species_npu[i]-1);
		}
	}

	/// transitional probabilities
	if (verbose) Rcpp::Rcout << "\ttransitional probs" << std::endl;
	std::vector<std::vector<double>> species_pu_tprobs(n_species);
  if (!unreliable_formulation) {
  	for (std::size_t i=0; i<n_species; ++i) {
  		species_pu_tprobs[i].reserve(species_npu[i]);
  		for (std::size_t j=0; j<species_npu[i]; ++j) {
  			species_pu_tprobs[i][j] = (1.0 - species_pu_probs[i][j]) / species_pu_probs[i][j];
  		}
  	}
  }
 
  // calculate targets
	if (verbose) Rcpp::Rcout << "\tspace targets" << std::endl;
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> speciesspaceMTX(n_species, n_attribute_spaces);
	for (std::size_t i=0; i<n_species; ++i)
		for (std::size_t j=0; j<n_attribute_spaces; ++j)
			speciesspaceMTX(i,j) = (spacetargetsMTX(i,j) - 1.0) * tss_speciesspaceMTX(i,j) * -1.0;
 
	// cache integer string conversions
	if (verbose) Rcout << "\tcaching integer/string conversions" << std::endl;
	std::size_t maxINT;
	maxINT=std::max(
		n_pu,
		std::max(
			n_species,
			std::max(
				n_attribute_spaces,
				species_ndp.maxCoeff()
			)
		)
	) + 2;

	std::vector<std::string> intSTR(maxINT);
	for (std::size_t i=0; i<maxINT; i++)
		intSTR[i] = num2str<std::size_t>(i);

	/// create unordered map with variable names
	if (verbose) Rcout << "\tcreating undordered_map with variable names" << std::endl;
	std::unordered_map<std::string, std::size_t> variableMAP;
	std::string currSTR;
	// pu vars
	for (std::size_t i=0; i<n_pu; ++i) {
		variableMAP[puDF_id_STR[i]] = counter;
		++counter;
	}

	// pu_pu boundary vars
	if (boundary) {
		for (std::size_t i=0; i<n_edges2; ++i) {
			variableMAP[boundaryDF_idpair_STR[i]] = counter;
			++counter;
		}
	}

	// space vars
  if (unreliable_formulation) {
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<species_npu[i]; ++l) {
							// Y_var
							currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l];
							variableMAP[currSTR] = counter;
							++counter;
						}
					}
				}
  		}
  	}
  } else {
    for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								// Y_var
								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								variableMAP[currSTR] = counter;
								++counter;

								// P_var
								currSTR="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								variableMAP[currSTR] = counter;
								++counter;

								// W_var
								currSTR="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								variableMAP[currSTR] = counter;
								++counter;
							}
						}
					}
				}
  		}
  	}
  }

	//// Main processing
	if (verbose) Rcpp::Rcout << "Main processing" << std::endl;
	Rcpp::checkUserInterrupt();

	/// objective function
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tobjective function" << std::endl;
	std::vector<double> objDBL(variableMAP.size());

	// cost variables
	if (verbose) Rcpp::Rcout << "\t\tcost variables" << std::endl;
	for (std::size_t i=0; i<n_pu; ++i) {
		objDBL[variableMAP[puDF_id_STR[i]]] = puDF_cost[i];
	}

	// boundary variables
	if (verbose) Rcpp::Rcout << "\t\tboundary variables" << std::endl;
	if (boundary) {
		for (std::size_t i=0; i<n_edges2; ++i) {
			objDBL[variableMAP[boundaryDF_idpair_STR[i]]] = -(blmDBL * boundaryDF_boundary[edge_pos[i]]);
		}
	}

	/// constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tconstraints" << std::endl;
	std::vector<std::size_t> model_rows_INT;
	std::vector<std::size_t> model_cols_INT;
	std::vector<double> model_vals_DBL;
	std::vector<std::string> senseSTR;
	std::vector<double> rhsDBL;
  if (unreliable_formulation) {
  	model_rows_INT.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum());
  	model_cols_INT.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum());
  	model_vals_DBL.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum());
  	senseSTR.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum());
  	rhsDBL.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum());
  } else {
    model_rows_INT.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum() * maxrlevelINT);
  	model_cols_INT.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum() * maxrlevelINT);
  	model_vals_DBL.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum() * maxrlevelINT);
  	senseSTR.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum() * maxrlevelINT);
  	rhsDBL.reserve(n_pu * n_species * n_attribute_spaces * species_ndp.sum() * maxrlevelINT);
  }

	// area target constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tarea constraints" << std::endl;
	counter=0;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<species_pu_ids[i].size(); ++j) {
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[species_pu_ids[i][j]]]);
			model_vals_DBL.push_back(species_pu_probs[i][j] * puDF_area[species_pu_ids[i][j]]);
		}
		senseSTR.push_back(">=");
		rhsDBL.push_back(speciesareaDBL[i] * areatargetsDBL[i]);
		++counter;
	}

	// space target constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tspace constraints" << std::endl;
  if (unreliable_formulation) {
    for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<species_npu[i]; ++l) {
							currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variableMAP[currSTR]);
							model_vals_DBL.push_back(Pow<2>(weightdistMTX(i,j)(k,l)));
						}
					}
					senseSTR.push_back("<=");
					rhsDBL.push_back(speciesspaceMTX(i,j));
					++counter;
				}
			}
  	}
  } else {
    for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								currSTR="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currSTR]);
								model_vals_DBL.push_back(Pow<2>(weightdistMTX(i,j)(k,l)));
							}
						}
					}
					senseSTR.push_back("<=");
					rhsDBL.push_back(speciesspaceMTX(i,j));
					++counter;
				}
			}
  	}
  }

	// pu status constraints
	if (verbose) Rcpp::Rcout << "\t\tpu status constraints" << std::endl;
	for (std::size_t i=0; i<n_pu; ++i) {
		if (puDF_status[i]==2) {
			// locked in
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[i]]);
			model_vals_DBL.push_back(1);
			senseSTR.push_back("=");
			rhsDBL.push_back(1.0);
			++counter;
		} else if (puDF_status[i]==3) {
			// locked  out
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[i]]);
			model_vals_DBL.push_back(1);
			senseSTR.push_back("=");
			rhsDBL.push_back(0.0);
			++counter;
		}
	}

	// boundary constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tboundary constraints" << std::endl;
	if (boundary) {
		for (std::size_t i=0; i<n_edges2; ++i) {
			/// constraints if boundaryDF_id1[i] != boundaryDF_id2[i]				
			// model+=boundaryDF_idpair_STR[i] + " - " + puDF_id_STR[boundaryDF_id1[i]] + " <= 0\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[boundaryDF_idpair_STR[i]]);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id1[edge_pos[i]]]]);
			model_vals_DBL.push_back(1.0);
			model_vals_DBL.push_back(-1.0);
			senseSTR.push_back("<=");
			rhsDBL.push_back(0.0);
			++counter;

			// model+=boundaryDF_idpair_STR[i] + " - " + puDF_id_STR[boundaryDF_id2[i]] + " <= 0\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[boundaryDF_idpair_STR[i]]);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id2[edge_pos[i]]]]);
			model_vals_DBL.push_back(1.0);
			model_vals_DBL.push_back(-1.0);
			senseSTR.push_back("<=");
			rhsDBL.push_back(0.0);
			++counter;

			// model+=boundaryDF_idpair_STR[i] + " - " +
				// puDF_id_STR[boundaryDF_id1[i]] + " - " +
				// puDF_id_STR[boundaryDF_id2[i]] + " >= -1\n";
			
			// constraints not strictly needed since decision variables are binary
// 			model_rows_INT.push_back(counter);
// 			model_rows_INT.push_back(counter);
// 			model_rows_INT.push_back(counter);
// 			model_cols_INT.push_back(variableMAP[boundaryDF_idpair_STR[i]]);
// 			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id1[edge_pos[i]]]]);
// 			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id2[edge_pos[i]]]]);
// 			model_vals_DBL.push_back(1);
// 			model_vals_DBL.push_back(-1);
// 			model_vals_DBL.push_back(-1);
// 			senseSTR.push_back(">=");
// 			rhsDBL.push_back(-1.0);
// 			++counter;
		}
	}

  std::string currW;
  std::string currP;
  std::string currY;
  if (unreliable_formulation) {
  	// 1b
	  Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn. 1b constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
							for (std::size_t l=0; l<species_npu[i]; ++l) {
								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currSTR]);
								model_vals_DBL.push_back(1.0);
							}
							senseSTR.push_back("=");
							rhsDBL.push_back(1.0);
							++counter;
					}
				}
  		}
  	}

  	// 1c
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn. 1c constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<species_npu[i]; ++l) {
							currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variableMAP[currSTR]);
							model_vals_DBL.push_back(1.0);

							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variableMAP[puDF_id_STR[species_pu_ids[i][l]]]);
							model_vals_DBL.push_back(-1.0);

							senseSTR.push_back("<=");
							rhsDBL.push_back(0.0);
							++counter;
						}
					}
				}
  		}
  	}
  } else {
    if (verbose) Rcpp::Rcout << "\t\teqn. 1b constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
							
							// original formulation
// 							for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
// 								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
// 								model_rows_INT.push_back(counter);
// 								model_cols_INT.push_back(variableMAP[currSTR]);
// 								model_vals_DBL.push_back(1.0);
// 							}
// 							for (std::size_t r2=0; r2<r; ++r2) {
// 								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[r2];
// 								model_rows_INT.push_back(counter);
// 								model_cols_INT.push_back(variableMAP[currSTR]);
// 								model_vals_DBL.push_back(1.0);
// 							}
// 							senseSTR.push_back("=");
// 							rhsDBL.push_back(1.0);
// 							++counter;
// 						}

							// force each R-level to be assigned to a pu
							for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currSTR]);
								model_vals_DBL.push_back(1.0);
							}
							senseSTR.push_back("=");
							rhsDBL.push_back(1.0);
							++counter;
						}
					}
				}
  		}
  	}

  	// 1b extra - force each pu to be assigned to only 1 r-level
    if (verbose) Rcpp::Rcout << "\t\teqn. 1b (extra) constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currSTR]);
								model_vals_DBL.push_back(1.0);
							}
							senseSTR.push_back("<=");
							rhsDBL.push_back(1.0);
							++counter;
						}
					}
				}
			}
		}
  	
  	// 1c
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn. 1c constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<species_npu[i]; ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currSTR]);
								model_vals_DBL.push_back(1.0);
							}
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variableMAP[puDF_id_STR[species_pu_ids[i][l]]]);
							model_vals_DBL.push_back(-1.0);
							senseSTR.push_back("<=");
							rhsDBL.push_back(0.0);
							++counter;
						}
					}
				}
			}
  	}

  	// 1d
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn. 1d constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						
						// original formulation
// 						for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
// 							currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[r];
// 							model_rows_INT.push_back(counter);
// 							model_cols_INT.push_back(variableMAP[currSTR]);
// 							model_vals_DBL.push_back(1.0);
// 						}
// 						senseSTR.push_back("=");
// 						rhsDBL.push_back(1.0);
// 						++counter;
						
						// ensure that failure planning unit is assigned to last r-level
						currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[species_rlevel[i]];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variableMAP[currSTR]);
						model_vals_DBL.push_back(1.0);
						senseSTR.push_back("=");
						rhsDBL.push_back(1.0);
						++counter;
						
						
					}
				}
			}
  	}

  	// 1e
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn. 1e constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<species_npu[i]; ++l) {
							// real pu
							currSTR="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_0";
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variableMAP[currSTR]);
							model_vals_DBL.push_back(1.0);
							senseSTR.push_back("=");
							rhsDBL.push_back(species_pu_probs[i][l]);
							++counter;
						}

						// failure pu
						currSTR="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_0";
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variableMAP[currSTR]);
						model_vals_DBL.push_back(1.0);

						senseSTR.push_back("=");
						rhsDBL.push_back(1.0);
						++counter;
					}
				}
			}
  	}

  	// 1f
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn. 1f constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					// assign values
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=1, r2=0; r<(species_rlevel[i]+1); ++r, ++r2) {

								currSTR="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currSTR]);
								model_vals_DBL.push_back(1.0);

								for (std::size_t l2=0; l2<species_npu[i]; ++l2) {
									currSTR="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l2]+"_"+intSTR[r2];
									model_rows_INT.push_back(counter);
									model_cols_INT.push_back(variableMAP[currSTR]);
									model_vals_DBL.push_back(-(species_pu_probs[i][l] * species_pu_tprobs[i][l2]));
								}

								senseSTR.push_back("=");
								rhsDBL.push_back(0.0);
								++counter;
							}
						}
					}
				}
  		}
  	}

  	/// W variables
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\teqn 2. constraints" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {				
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								// init
								currW="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								currP="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								currY="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								// 2a
								//model+=currW + " - " + currP + " <= 0\n";
								model_rows_INT.push_back(counter);
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currW]);
								model_cols_INT.push_back(variableMAP[currP]);
								model_vals_DBL.push_back(1.0);
								model_vals_DBL.push_back(-1.0);
								senseSTR.push_back("<=");
								rhsDBL.push_back(0.0);
								++counter;
								// 2b
								//model+=currW + " - " + currY + " <= 0\n";
								model_rows_INT.push_back(counter);
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currW]);
								model_cols_INT.push_back(variableMAP[currY]);
								model_vals_DBL.push_back(1.0);
								model_vals_DBL.push_back(-1.0);
								senseSTR.push_back("<=");
								rhsDBL.push_back(0.0);
								++counter;
								// 2c
								//model+=currW + " >= 0\n";
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currW]);
								model_vals_DBL.push_back(1.0);
								senseSTR.push_back(">=");
								rhsDBL.push_back(0.0);
								++counter;
								// 2d
								//model+= currW + " - " + currP + " - " + currY + " >= -1\n";
								//model+= currW + " >= " + currP + " + " + currY + " - 1\n";
								model_rows_INT.push_back(counter);
								model_rows_INT.push_back(counter);
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variableMAP[currW]);
								model_cols_INT.push_back(variableMAP[currP]);
								model_cols_INT.push_back(variableMAP[currY]);
								model_vals_DBL.push_back(1.0);
								model_vals_DBL.push_back(-1.0);
								model_vals_DBL.push_back(-1.0);
								senseSTR.push_back(">=");
								rhsDBL.push_back(-1.0);
								++counter;
							}
						}
					}
				}
  		}
  	}
  }
  
	////// variable types and bounds
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tvariable types" << std::endl;
	std::vector<std::string> vtypeSTR(objDBL.size());
	std::vector<std::size_t> lbINT(objDBL.size(), 0);
	std::vector<std::size_t> ubINT(objDBL.size(), 1);

	///// binary
	if (verbose) Rcpp::Rcout << "\t\t binary vars" << std::endl;

	// 1g
	Rcpp::checkUserInterrupt();
	for (std::size_t i=0; i<n_pu; ++i) {
		vtypeSTR[variableMAP[puDF_id_STR[i]]]="B";
	}

	// boundary variables
	Rcpp::checkUserInterrupt();
	if (boundary) {
		for (std::size_t i=0; i<n_edges2; ++i)
			vtypeSTR[variableMAP[boundaryDF_idpair_STR[i]]]="B";
	}

  // 1g constraints
  Rcpp::checkUserInterrupt();
  if (unreliable_formulation) {
  	// 1g
    for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<species_npu[i]; ++l) {
							currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l];
							vtypeSTR[variableMAP[currSTR]]="B";
						}
					}
				}
  		}
  	}
  } else {
    // 1g
  	Rcpp::checkUserInterrupt();
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								vtypeSTR[variableMAP[currSTR]]="B";
							}
						}
					}
				}
  		}
  	}

  	///// semi-continuous
  	Rcpp::checkUserInterrupt();
  	if (verbose) Rcpp::Rcout << "\t\tsemi-continuous vars" << std::endl;
  	for (std::size_t i=0; i<n_species; ++i) {
  		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				if (!isnan(spacetargetsMTX(i,j))) {
					for (std::size_t k=0; k<species_ndp(i,j); ++k) {
						for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
							for (std::size_t r=0; r<(species_rlevel[i]+1); ++r) {
								// w variables
								currW="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								vtypeSTR[variableMAP[currW]]="S";
								// p variables
								currP="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
								vtypeSTR[variableMAP[currP]]="S";
							}
						}
					}
				}
  		}
  	}
  }
  
  // calculate best space proportions
  if (verbose) Rcpp::Rcout << "\tcalculating best possible space targets" << std::endl;
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> best_speciesspaceMTX(1, n_species*n_attribute_spaces);
	std::size_t currCol=-1;
	if (unreliable_formulation) {
		for (std::size_t i=0; i<n_species; ++i) {
			for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				++currCol;
				best_speciesspaceMTX(0,currCol)=1.0-(unreliable_space_value(weightdistMTX(i,j),true) / tss_speciesspaceMTX(i,j));
		}
   }
	} else {
		for (std::size_t i=0; i<n_species; ++i) {
			for (std::size_t j=0; j<n_attribute_spaces; ++j) {
				++currCol;
				best_speciesspaceMTX(0, currCol)=1.0-(reliable_space_value(weightdistMTX(i,j),species_pu_probs[i],species_rlevel[i],true) / tss_speciesspaceMTX(i,j));
			}
		}
	}

	// fix variablesSTR
	std::vector<std::string> variablesSTR(variableMAP.size());
	for (auto i : variableMAP)
		variablesSTR[i.second]=i.first;
	if (!unreliable_formulation)
		variablesSTR.pop_back();

	//// Exports
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "Caching distance calculations" << std::endl;
	std::vector<std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>> wdistLST(n_species);
	for (std::size_t i=0; i<n_species; ++i) {
		wdistLST[i].resize(n_attribute_spaces);
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			wdistLST[i][j]=weightdistMTX(i,j);
		}
	}
		
	if (verbose) Rcpp::Rcout << "Sending data to R" << std::endl;
	return(
		Rcpp::List::create(
			Rcpp::Named("Ar") = Rcpp::List::create(
				Rcpp::Named("row") = Rcpp::wrap(model_rows_INT),
				Rcpp::Named("col") = Rcpp::wrap(model_cols_INT),
				Rcpp::Named("value") = Rcpp::wrap(model_vals_DBL)
			),
			Rcpp::Named("obj") = Rcpp::wrap(objDBL),
			Rcpp::Named("sense") = Rcpp::wrap(senseSTR),
			Rcpp::Named("rhs") = Rcpp::wrap(rhsDBL),
			Rcpp::Named("vtype") = Rcpp::wrap(vtypeSTR),
			Rcpp::Named("lb") = Rcpp::wrap(lbINT),
			Rcpp::Named("ub") = Rcpp::wrap(ubINT),
			Rcpp::Named("cache") = Rcpp::List::create(
				Rcpp::Named("best_amount_values") = Rcpp::wrap(speciesareaDBL),
				Rcpp::Named("space_targets") = Rcpp::wrap(speciesspaceMTX),
				Rcpp::Named("tss_space_values") = Rcpp::wrap(tss_speciesspaceMTX),
				Rcpp::Named("best_space_values") = Rcpp::wrap(best_speciesspaceMTX),
				Rcpp::Named("variables") = Rcpp::wrap(variablesSTR),
				Rcpp::Named("wdist") = Rcpp::wrap(wdistLST)
			),
			Rcpp::Named("modelsense") = "min"
		)
	);
}
