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
std::string rcpp_generate_modelfile(Rcpp::S4 opts, Rcpp::S4 data) {
	//// Initialization
// 	Rcpp::Rcout << "Initialization" << std::endl;
	// create variables
	std::string model = "";
	std::size_t n_attribute_spaces;
	std::size_t n_pu;
	std::size_t n_edges;
	std::size_t n_species;
	std::vector<std::size_t> n_demand_points;
	bool boundary;
	double boundary_threshold=1.0e-05;
	
	//// Preliminary processing
// 	Rcpp::Rcout << "Preliminary processing" << std::endl;
	/// extract parameters from Rcpp::S4 opts
	double failure_multiplier=Rcpp::as<double>(opts.slot("FAILUREMULTIPLIER"));
	std::size_t maxrlevelINT=Rcpp::as<double>(opts.slot("MAXRLEVEL"));
	double blmDBL=Rcpp::as<double>(opts.slot("BLM"));
	boundary = blmDBL > boundary_threshold;

	/// extract data from Rcpp::S4 data
	// species data
// 	Rcpp::Rcout << "\tSpecies data" << std::endl;
	Rcpp::DataFrame speciesDF=Rcpp::as<Rcpp::DataFrame>(data.slot("species"));
	std::vector<double> speciesDF_areatarget = speciesDF["area.target"];
	std::vector<double> speciesDF_spacetarget = speciesDF["space.target"];
	n_species=speciesDF_areatarget.size();
	
	// planning unit data
// 	Rcpp::Rcout << "\tpu data" << std::endl;
	Rcpp::DataFrame puDF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu"));
	std::vector<double> puDF_area = puDF["area"];
	std::vector<double> puDF_cost = puDF["cost"];
	std::vector<std::size_t> puDF_status = puDF["status"];
	n_pu=puDF_area.size();
	std::vector<std::string> puDF_id_CHR(n_pu+1);
	for (std::size_t i=0; i<(n_pu+1); ++i)
		puDF_id_CHR[i]="pu_"+std::to_string(i);
	
	
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
	std::vector<std::string> boundaryDF_idpair_CHR;
	if (boundary) {
		boundaryDF_idpair_CHR.reserve(n_edges);
		for (std::size_t i=0; i<n_edges; ++i) {
			boundaryDF_idpair_CHR.push_back(
				"pu" + std::to_string(boundaryDF_id1[i]-1) + "_" + std::to_string(boundaryDF_id2[i]-1)
			);
		}
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
	for (std::size_t i=0; i<n_attribute_spaces; ++i) {
		currLST=Rcpp::as<Rcpp::S4>(attributespaceLST[i]).slot("dp");
		for (std::size_t j=0; j<n_species; ++j) {
			// extract species i demand points for space j
			currS4=Rcpp::as<Rcpp::S4>(currLST[j]);
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
		
	/// preprocessing for area targets
// 	Rcpp::Rcout << "\tpre-process area targets" << std::endl;
	// ini
// 	Rcpp::Rcout << "\t\tinit" << std::endl;	
	double currArea;
	std::vector<std::string> puvspeciesSTR(n_species, "");
	std::vector<double> speciesareaDBL(n_species, 0.0);
	std::vector<std::vector<std::size_t>> species_pu_ids(n_species);
	std::vector<std::vector<double>> species_pu_probs(n_species);
	std::vector<std::size_t> species_npu(n_species);
	for (std::size_t i=0; i<n_species; ++i) {
		species_pu_ids[i].reserve(puvspeciesDF_pu.size());
		species_pu_probs[i].reserve(puvspeciesDF_pu.size());
	}
	
	// calcs
// 	Rcpp::Rcout << "\t\tcalculate numbers" << std::endl;
	for (std::size_t i=0; i<puvspeciesDF_pu.size(); ++i) {
		// calculate area
		currArea=puvspeciesDF_value[i] * puDF_area[puvspeciesDF_pu[i]];
		// generate target strings for model
		puvspeciesSTR[puvspeciesDF_species[i]]+=std::to_string(currArea) + " " +  puDF_id_CHR[i] + " + ";
		// sum area occupied by species
		speciesareaDBL[puvspeciesDF_species[i]]+=currArea;
		// assign pu to species vector
		species_pu_ids[puvspeciesDF_species[i]].push_back(puvspeciesDF_pu[i]);
		// assign prob to species vector
		species_pu_probs[puvspeciesDF_species[i]].push_back(puvspeciesDF_value[i]);
	}
// 	Rcpp::Rcout << "\t\tresize vectors" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		puvspeciesSTR[i].erase(puvspeciesSTR[i].size()-3);
		species_pu_ids[i].push_back(n_pu);
		species_pu_ids[i].shrink_to_fit();
		species_pu_probs[i].push_back(1.0);
		species_pu_probs[i].shrink_to_fit();
		species_npu[i]=species_pu_ids[i].size()-1;
	}
	
	/// transitional probabilities
// 	Rcpp::Rcout << "\ttransitional probs" << std::endl;
	std::vector<std::vector<double>> species_pu_tprobs(n_species);
	for (std::size_t i=0; i<n_species; ++i) {
		species_pu_tprobs[i].reserve(species_npu[i]);
		for (std::size_t j=0; j<species_npu[i]; ++j) {
			species_pu_tprobs[i][j] = (1.0 - species_pu_probs[i][j]) / species_pu_probs[i][j];
		}
	}
	
	/// create distance variables
// 	Rcpp::Rcout << "\tdistance vars" << std::endl;
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
// 	Rcpp::Rcout << "\tspace targets" << std::endl;
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
	
	// Rcout << "best case space representation = " << speciesspaceMTX(0,0) << std::endl;	
	
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
// 	Rcout << "maxINT = " << maxINT << std::endl;
	for (std::size_t i=0; i<maxINT; i++)
		intSTR[i] = std::to_string(i);
	
	//// Main processing
// 	Rcpp::Rcout << "Main processing" << std::endl;
	/// comments to cache variable values
// 	Rcpp::Rcout << "\tcaching variables" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		model+="\\species_best_areaamountheld_"+intSTR[i]+" "+std::to_string(speciesareaDBL[i]) + "\n";
	}
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			model+="\\species_best_spaceheld_"+intSTR[i]+"_"+intSTR[j]+" "+std::to_string(speciesspaceMTX(i,j)) + "\n";
		}
	}
	
	/// objective function
	model+="Minimize\n";
// 	Rcpp::Rcout << "\tobjective function" << std::endl;
	// cost variables
// 	Rcpp::Rcout << "\t\tcost variables" << std::endl;
	for (std::size_t i=0; i<n_pu; ++i) {
		model+=std::to_string(puDF_cost[i]) + " " + puDF_id_CHR[i] + " + ";
	}
	
	// boundary variables
// 	Rcpp::Rcout << "\t\tboundary variables" << std::endl;
	if (boundary) {
		for (std::size_t i=0; i<n_edges; ++i) {
			model+=std::to_string(blmDBL * boundaryDF_boundary[i]) + " " + boundaryDF_idpair_CHR[i] + " + ";
		}
	}
	model.erase(model.size()-3);
	// add new line
	model+="\n";
	
	/// constraints
// 	Rcpp::Rcout << "\tconstraints" << std::endl;
	
	model+="\nSubject To\n";
	// area target constraints
// 	Rcpp::Rcout << "\t\tarea constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		model+=puvspeciesSTR[i]+" >= " + std::to_string(speciesareaDBL[i] * speciesDF_areatarget[i]) + "\n";
	}
	
	// space target constraints
// 	Rcpp::Rcout << "\t\tspace constraints" << std::endl;
	std::string tempSTR;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					tempSTR=std::to_string(weightdistMTX(i,j)(k,l));
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						model+=tempSTR+" W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" + "; 
					}
				}
			}
			model.erase(model.size()-2);
			model+="<= "+std::to_string(speciesspaceMTX(i,j) * (1.0+speciesDF_spacetarget[i])) + "\n";
		}
	}
	
	// pu status constraints
// 	Rcpp::Rcout << "\t\tpu status constraints" << std::endl;
	for (std::size_t i=0; i<n_pu; ++i) {
		if (puDF_status[i]==2) {
			// locked in
			model+=puDF_id_CHR[i] + " = 1\n";
		} else if (puDF_status[i]==3) {
			// locked  out
			model+=puDF_id_CHR[i] + " = 0\n";
		}
	}
	
	// boundary constraints
// 	Rcpp::Rcout << "\t\tboundary constraints" << std::endl;
	if (boundary) {
		for (std::size_t i=0; i<n_edges; ++i) {
			model+=boundaryDF_idpair_CHR[i] + " - " + puDF_id_CHR[boundaryDF_id1[i]] + " <= 0\n";
			model+=boundaryDF_idpair_CHR[i] + " - " + puDF_id_CHR[boundaryDF_id2[i]] + " <= 0\n";
			model+=boundaryDF_idpair_CHR[i] + " - " + 
				puDF_id_CHR[boundaryDF_id1[i]] + " - " +
				puDF_id_CHR[boundaryDF_id2[i]] + " >= -1\n";
		}
	}

	// 1b
// 	Rcpp::Rcout << "\t\t1b constraints" << std::endl;	
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
					for (std::size_t l=0; l<(species_npu[i]+1); ++l)
						model+="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" + ";  
					for (std::size_t r2=0; r2<r; ++r2)
						model+="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[r2]+" + ";
					model.erase(model.size()-3);
					model+=" = 1\n";
				}
			}
		}
	}
	
	// 1c
// 	Rcpp::Rcout << "\t\t1c constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<species_npu[i]; ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						model+="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" + ";  
					}
					model.erase(model.size()-3);
					model+=" - " +puDF_id_CHR[l]+" <= 0\n";
				}
			}
		}
	}

	// 1d
// 	Rcpp::Rcout << "\t\t1d constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t r=0; r<(maxrlevelINT+1); ++r)
					model+="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_"+intSTR[r]+" + ";
				model.erase(model.size()-3);
				model+=" = 1\n";
			}
		}
	}
	
	// 1e
// 	Rcpp::Rcout << "\t\t1e constraints" << std::endl;
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				// real pu
				for (std::size_t l=0; l<species_npu[i]; ++l) {
					model+="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_0 = "+std::to_string(species_pu_probs[i][l]) + "\n";
				}
				// failure pu
				model+="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[species_npu[i]]+"_0 = 1.0\n";
			}
		}
	}

	// 1f
// 	Rcpp::Rcout << "\t\t1f constraints" << std::endl;	
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=1, r2=0; r<(maxrlevelINT+1); ++r, ++r2) {
						model+="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" - ";
						for (std::size_t l2=0; l2<species_npu[i]; ++l2) {
							model+=std::to_string(species_pu_probs[i][l] * species_pu_tprobs[i][l2]) + " " +
							"W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l2]+"_"+intSTR[r2]+
							" - ";
						}
						model.erase(model.size()-3);
						model+=" = 0\n";
					}
				}
			}
		}
	}
	
	/// W variables
// 	Rcpp::Rcout << "\t\t2 constraints" << std::endl;	
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
						model+=currW + " - " + currP + " <= 0\n";
						// 2b
						model+=currW + " - " + currY + " <= 0\n";
						// 2c
						model+=currW + " >= 0\n";
						// 2d
						model+= currW + " - " + currP + " - " + currY + " >= -1\n";
// 						model+= currW + " >= " + currP + " + " + currY + " - 1\n";
					}
				}
			}
		}
	}
	
	////// variable types
// 	Rcpp::Rcout << "\tvariable types" << std::endl;	
	///// binary
// 	Rcpp::Rcout << "\t\t binary vars" << std::endl;	
	model+="\nBinary\n";

	// 1g
	for (std::size_t i=0; i<n_pu; ++i)
		model+=puDF_id_CHR[i] + " ";
	
	// boundary variables
	if (boundary) {
		for (std::size_t i=0; i<n_edges; ++i)
			model+=boundaryDF_idpair_CHR[i] + " ";
	}
	
	// 1g
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						model+="Y_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" ";
					}
				}
			}
		}
	}
	model.erase(model.size()-1);
	
	///// semi-continuous
// 	Rcpp::Rcout << "\t\tsemi-continuous vars" << std::endl;	

	model+="\nSemi-continuous\n";
	for (std::size_t i=0; i<n_species; ++i) {
		for (std::size_t j=0; j<n_attribute_spaces; ++j) {
			for (std::size_t k=0; k<species_ndp[i]; ++k) {
				for (std::size_t l=0; l<(species_npu[i]+1); ++l) {
					for (std::size_t r=0; r<(maxrlevelINT+1); ++r) {
						// w variables
						model+="W_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" ";
						// p variables
						model+="P_"+intSTR[i]+"_"+intSTR[j]+"_"+intSTR[k]+"_"+intSTR[l]+"_"+intSTR[r]+" ";
					}
				}
			}
		}
	}
	model.erase(model.size()-1);
	
	//// Exports
// 	Rcpp::Rcout << "Exports" << std::endl;
	model+="\nEnd\n";
	return(model);
}


