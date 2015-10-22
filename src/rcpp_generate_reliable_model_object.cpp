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
#include <RcppEigen.h>
#include <Rcpp.h>


// [[Rcpp::export]]
Rcpp::List rcpp_generate_reliable_model_object(Rcpp::S4 opts, Rcpp::S4 data, bool verbose) {
	//// Initialization
	if (verbose) Rcpp::Rcout << "Initialization" << std::endl;
	// create variables
	std::size_t counter=0;
	std::size_t n_attribute_spaces;
	std::size_t n_pu;
	std::size_t n_edges;
	std::size_t n_species;
	std::vector<std::size_t> n_demand_points;
	bool boundary;
	double boundary_threshold=1.0e-05;


	//// Preliminary processing
	if (verbose) Rcpp::Rcout << "Preliminary processing" << std::endl;
	Rcpp::checkUserInterrupt();
	/// extract parameters from Rcpp::S4 opts
	double failure_multiplier=Rcpp::as<double>(opts.slot("FAILUREMULTIPLIER"));
	std::size_t maxrlevelINT=Rcpp::as<double>(opts.slot("MAXRLEVEL"));
	double blmDBL=Rcpp::as<double>(opts.slot("BLM"));
	boundary = blmDBL > boundary_threshold;

	/// extract data from Rcpp::S4 data
	// species data
	if (verbose) Rcpp::Rcout << "\tSpecies data" << std::endl;
	Rcpp::DataFrame speciesDF=Rcpp::as<Rcpp::DataFrame>(data.slot("species"));
	std::vector<double> speciesDF_areatarget = speciesDF["area.target"];
	std::vector<double> speciesDF_spacetarget = speciesDF["space.target"];
	n_species=speciesDF_areatarget.size();

	// planning unit data
	if (verbose) Rcpp::Rcout << "\tpu data" << std::endl;
	Rcpp::DataFrame puDF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu"));
	std::vector<double> puDF_area = puDF["area"];
	std::vector<double> puDF_cost = puDF["cost"];
	std::vector<std::size_t> puDF_status = puDF["status"];
	n_pu=puDF_area.size();
	std::vector<std::string> puDF_id_STR(n_pu+1);
	for (std::size_t i=0; i<(n_pu+1); ++i)
		puDF_id_STR[i]="pu_"+std::to_string(i);


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
	std::vector<std::string> boundaryDF_idpair_CHR;
	if (boundary) {
		boundaryDF_idpair_CHR.reserve(n_edges);
		for (std::size_t i=0; i<n_edges; ++i) {
			boundaryDF_idpair_CHR.push_back(
				"pu_" + std::to_string(boundaryDF_id1[i]-1) + "_" + std::to_string(boundaryDF_id2[i]-1)
			);
		}
	}

	/// attribute.space
	if (verbose) Rcpp::Rcout << "\tattribute space data" << std::endl;
	Rcpp::List attributespaceLST=Rcpp::as<Rcpp::List>(data.slot("attribute.spaces"));
	n_attribute_spaces=attributespaceLST.size();
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

// 			Rcout << "species_ndp[i] = " << species_ndp[i] << std::endl;
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
		species_pu_ids[i].push_back(n_pu);
		species_pu_ids[i].shrink_to_fit();
		species_pu_probs[i].push_back(1.0);
		species_pu_probs[i].shrink_to_fit();
		species_npu[i]=species_pu_ids[i].size()-1;
	}

	/// transitional probabilities
	if (verbose) Rcpp::Rcout << "\ttransitional probs" << std::endl;
	std::vector<std::vector<double>> species_pu_tprobs(n_species);
	for (std::size_t i=0; i<n_species; ++i) {
		species_pu_tprobs[i].reserve(species_npu[i]);
		for (std::size_t j=0; j<species_npu[i]; ++j) {
			species_pu_tprobs[i][j] = (1.0 - species_pu_probs[i][j]) / species_pu_probs[i][j];
		}
	}

	/// create distance variables
	if (verbose) 	Rcpp::Rcout << "\tdistance vars" << std::endl;
	double currFailDist;
	Eigen::ArrayXXd currArray;
	Eigen::Matrix<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> weightdistMTX(n_species, n_attribute_spaces);
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			// resize matrix
			weightdistMTX(i,j).resize(species_ndp[i],species_npu[i]+1);
			// calculate distances
			for (std::size_t l=0; l<species_npu[i]; ++l) {
				for (std::size_t k=0; k<species_ndp[i]; ++k) {
					currArray=pupointsMTX[j].row(species_pu_ids[i][l]) - demandpoints_coords_MTX(i,j).row(k);
					weightdistMTX(i,j)(k,l) = demandpoints_weights_MTX(i,j)[k] * std::sqrt(currArray.square().sum());
				}
			}
			// failure pu
			for (std::size_t k=0; k<species_ndp[i]; ++k)
				weightdistMTX(i,j)(k,species_npu[i])=0.0;
			currFailDist=weightdistMTX(i,j).maxCoeff() * failure_multiplier;
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				weightdistMTX(i,j)(k,species_npu[i]) = currFailDist;
			}

		}
	}

	/// calculate spacetargets
	if (verbose) Rcpp::Rcout << "\tspace targets" << std::endl;
	double currProb;
	Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> speciesspaceMTX(n_species, n_attribute_spaces);
	std::vector<std::size_t> currPUs;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			speciesspaceMTX(i,j)=0;
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				// get indices for first R level planning units
				currPUs=species_pu_ids[i];
				std::partial_sort(currPUs.begin(), currPUs.begin()+(maxrlevelINT+1), currPUs.end(), [&](const std::size_t p1, const std::size_t p2) {
					return(weightdistMTX(i,j)(k,p1) < weightdistMTX(i,j)(k,p2));
				});

				// calculate values for real pus
				currProb=1.0;
				for (std::size_t r=0; r < maxrlevelINT; ++r) {
					speciesspaceMTX(i,j)+= (species_pu_probs[i][currPUs[r]] * currProb * weightdistMTX(i,j)(k,currPUs[r]));
					currProb*=(1.0 - species_pu_probs[i][currPUs[r]]);
				}
				// caculate values for failure pu
				speciesspaceMTX(i,j)+= currProb*weightdistMTX(i,j)(k,species_npu[i]);
			}
		}
	}

	// cache integer string conversions
	if (verbose) Rcout << "\tcaching integer/string conversions" << std::endl;
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
		for (std::size_t i=0; i<n_edges; ++i) {
			variableMAP[boundaryDF_idpair_CHR[i]] = counter;
			++counter;
		}
	}

	// space vars
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
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

	//// Main processing
	if (verbose) Rcpp::Rcout << "Main processing" << std::endl;
	Rcpp::checkUserInterrupt();
	/// comments to cache variable values
	if (verbose) Rcpp::Rcout << "\tcaching variables" << std::endl;
	std::vector<std::string> cacheSTR;
	cacheSTR.reserve(n_species + (n_species * n_attribute_spaces));
	std::vector<double> cacheDBL;
	cacheDBL.reserve(n_species + (n_species * n_attribute_spaces));
	for (std::size_t i=0; i<n_species; ++i) {
		cacheSTR.push_back("species_best_areaamountheld_"+intSTR[i]);
		cacheDBL.push_back(speciesareaDBL[i]);
	}
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			cacheSTR.push_back("species_best_spaceheld_"+intSTR[i]+"_"+intSTR[j]);
			cacheDBL.push_back(speciesspaceMTX(i,j));
		}
	}

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
		for (std::size_t i=0; i<n_edges; ++i) {
			objDBL[variableMAP[boundaryDF_idpair_CHR[i]]] = blmDBL * boundaryDF_boundary[i];
		}
	}

	/// constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tconstraints" << std::endl;
	std::vector<std::size_t> model_rows_INT;
	model_rows_INT.reserve(n_pu * n_species * n_attribute_spaces * std::accumulate(species_ndp.begin(), species_ndp.end(), 0) * maxrlevelINT);
	std::vector<std::size_t> model_cols_INT;
	model_cols_INT.reserve(n_pu * n_species * n_attribute_spaces * std::accumulate(species_ndp.begin(), species_ndp.end(), 0) * maxrlevelINT);
	std::vector<double> model_vals_DBL;
	model_vals_DBL.reserve(n_pu * n_species * n_attribute_spaces * std::accumulate(species_ndp.begin(), species_ndp.end(), 0) * maxrlevelINT);
	std::vector<std::string> senseSTR;
	senseSTR.reserve(n_pu * n_species * n_attribute_spaces * std::accumulate(species_ndp.begin(), species_ndp.end(), 0) * maxrlevelINT);
	std::vector<double> rhsDBL;
	rhsDBL.reserve(n_pu * n_species * n_attribute_spaces * std::accumulate(species_ndp.begin(), species_ndp.end(), 0) * maxrlevelINT);

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
		rhsDBL.push_back(speciesareaDBL[i] * speciesDF_areatarget[i]);
		++counter;
	}

// 	Rcout << "rhsDBL.size() = " << rhsDBL.size() << "; counter = " << counter << std::endl;

	// space target constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tspace constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						currSTR="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variableMAP[currSTR]);
						model_vals_DBL.push_back(weightdistMTX(i,j)(k,l));
					}
				}
			}
			senseSTR.push_back("<=");
			rhsDBL.push_back(1e-5/((1.0e-5/(speciesspaceMTX(i,j)+1.0e-1)) * speciesDF_spacetarget[i]));
			++counter;
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
		for (std::size_t i=0; i<n_edges; ++i) {
			// model+=boundaryDF_idpair_CHR[i] + " - " + puDF_id_STR[boundaryDF_id1[i]] + " <= 0\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[boundaryDF_idpair_CHR[i]]);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id1[i]]]);
			model_vals_DBL.push_back(1.0);
			model_vals_DBL.push_back(-1.0);
			senseSTR.push_back("<=");
			rhsDBL.push_back(0.0);
			++counter;

			// model+=boundaryDF_idpair_CHR[i] + " - " + puDF_id_STR[boundaryDF_id2[i]] + " <= 0\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[boundaryDF_idpair_CHR[i]]);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id2[i]]]);
			model_vals_DBL.push_back(1.0);
			model_vals_DBL.push_back(-1.0);
			senseSTR.push_back("<=");
			rhsDBL.push_back(0.0);
			++counter;

			// model+=boundaryDF_idpair_CHR[i] + " - " +
				// puDF_id_STR[boundaryDF_id1[i]] + " - " +
				// puDF_id_STR[boundaryDF_id2[i]] + " >= -1\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variableMAP[boundaryDF_idpair_CHR[i]]);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id1[i]]]);
			model_cols_INT.push_back(variableMAP[puDF_id_STR[boundaryDF_id2[i]]]);
			model_vals_DBL.push_back(1);
			model_vals_DBL.push_back(-1);
			model_vals_DBL.push_back(-1);
			senseSTR.push_back(">=");
			rhsDBL.push_back(-1.0);
			++counter;
		}
	}

	// 1b
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\teqn. 1b constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
					for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
						currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variableMAP[currSTR]);
						model_vals_DBL.push_back(1.0);
					}
					for (std::size_t r2=0; r2<r; ++r2) {
						currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[r2];
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
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<species_npu[i]; ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variableMAP[currSTR]);
						model_vals_DBL.push_back(1.0);
					}
					model_rows_INT.push_back(counter);
					model_cols_INT.push_back(variableMAP[puDF_id_STR[l]]);
					model_vals_DBL.push_back(-1.0);
					senseSTR.push_back("<=");
					rhsDBL.push_back(0.0);
					++counter;
				}
			}
		}
	}

	// 1d
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\teqn. 1d constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
					currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[r];
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

	// 1e
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\teqn. 1e constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
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

	// 1f
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\teqn. 1f constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=1, r2=0; r<(maxrlevelINT+1); ++r, ++r2) {

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

	/// W variables
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\teqn 2. constraints" << std::endl;
	std::string currW;
	std::string currP;
	std::string currY;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
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

	////// variable types
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tvariable types" << std::endl;
	std::vector<std::string> vtypeSTR(objDBL.size());

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
		for (std::size_t i=0; i<n_edges; ++i)
			vtypeSTR[variableMAP[boundaryDF_idpair_CHR[i]]]="B";
	}

	// 1g
	Rcpp::checkUserInterrupt();
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						currSTR="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r];
						vtypeSTR[variableMAP[currSTR]]="B";
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
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
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

	//// Exports
	Rcpp::checkUserInterrupt();
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
			Rcpp::Named("cache") = Rcpp::List::create(
				Rcpp::Named("name") = Rcpp::wrap(cacheSTR),
				Rcpp::Named("value") = Rcpp::wrap(cacheDBL)
			),
			Rcpp::Named("modelsense") = "min"
		)
	);
}
