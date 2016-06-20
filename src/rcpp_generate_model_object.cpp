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
	std::size_t n_attribute_spaces_INT;
	std::size_t n_pu_INT;
	std::size_t n_edges_INT;
	std::size_t n_edges2_INT=0;
	std::size_t n_species_INT;
	bool boundary;
	double boundary_threshold=1.0e-10;
	double zero_adjust=0.0;

	//// Preliminary processing
	if (verbose) Rcpp::Rcout << "Preliminary processing" << std::endl;
	Rcpp::checkUserInterrupt();
	/// extract parameters from Rcpp::S4 opts
	double blm_DBL=Rcpp::as<double>(opts.slot("BLM"));
	boundary = blm_DBL > boundary_threshold;
  double failure_multiplier_DBL=0.0;
  std::size_t maxrlevel_INT=0;
  if (!unreliable_formulation) {
    failure_multiplier_DBL=Rcpp::as<double>(opts.slot("failure.multiplier"));
  	maxrlevel_INT=Rcpp::as<double>(opts.slot("max.r.level"));
  }

	/// extract data from Rcpp::S4 data
	// species data
	if (verbose) Rcpp::Rcout << "\tSpecies data" << std::endl;
	Rcpp::DataFrame species_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("species"));
	n_species_INT=species_DF.nrows();
	// planning unit data
	if (verbose) Rcpp::Rcout << "\tpu data" << std::endl;
	Rcpp::DataFrame pu_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu"));
	std::vector<double> pu_DF_area = pu_DF["area"];
	std::vector<double> pu_DF_cost = pu_DF["cost"];
	std::vector<std::size_t> pu_DF_status = pu_DF["status"];
	n_pu_INT=pu_DF_area.size();
	std::vector<std::string> pu_DF_id_STR(n_pu_INT+1);
	for (std::size_t i=0; i<n_pu_INT; ++i)
		pu_DF_id_STR[i]="pu_"+num2str<std::size_t>(i);

	// pu.species.probabilities
	if (verbose) Rcpp::Rcout << "\tpuvspecies data" << std::endl;
	Rcpp::DataFrame puvspecies_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("pu.species.probabilities"));
	std::vector<std::size_t> puvspecies_DF_pu = puvspecies_DF["pu"];
	std::vector<std::size_t> puvspecies_DF_species = puvspecies_DF["species"];
	std::vector<double> puvspecies_DF_value = puvspecies_DF["value"];
	for (std::size_t i=0; i<puvspecies_DF_pu.size(); ++i) {
		puvspecies_DF_pu[i]-=1;
		puvspecies_DF_species[i]-=1;
	}

	// boundary
	if (verbose) Rcpp::Rcout << "\tboundary data" << std::endl;
	Rcpp::DataFrame boundary_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("boundary"));
	std::vector<std::size_t> boundary_DF_id1 = boundary_DF["id1"];
	std::vector<std::size_t> boundary_DF_id2 = boundary_DF["id2"];
	std::vector<double> boundary_DF_boundary = boundary_DF["boundary"];
	n_edges_INT=boundary_DF_boundary.size();
	std::vector<std::size_t> edge_pos_INT;
	std::vector<std::string> boundary_DF_idpair_STR;
	if (boundary) {
		boundary_DF_idpair_STR.reserve(n_edges_INT);
		edge_pos_INT.reserve(n_edges_INT);
		for (std::size_t i=0; i<n_edges_INT; ++i) {
			// convert to base-0 indexing
			boundary_DF_id1[i]-=1;
			boundary_DF_id2[i]-=1;
			// main processing
			if (boundary_DF_id1[i]!=boundary_DF_id2[i]) {
				/// if boundary_DF_id1[i] != boundary_DF_id2[i]
				// store quadratic variable
				boundary_DF_idpair_STR.push_back(
					"pu_" + num2str<std::size_t>(boundary_DF_id1[i]) + "_" + num2str<std::size_t>(boundary_DF_id2[i])
				);
				// cache location of quadratic variable
				edge_pos_INT.push_back(i);
			}
			// increase cost variable with boundary
			pu_DF_cost[boundary_DF_id1[i]] += (blm_DBL * boundary_DF_boundary[i]);
		}
		boundary_DF_idpair_STR.shrink_to_fit();
		n_edges2_INT=boundary_DF_idpair_STR.size();
	}

	/// attribute.space
	if (verbose) Rcpp::Rcout << "\tattribute space data" << std::endl;
	Rcpp::List attributespaces_LST=Rcpp::as<Rcpp::List>(data.slot("attribute.spaces"));
	n_attribute_spaces_INT=attributespaces_LST.size();
	
	/// extract objects from S4 AttributeSpaces objects
	if (verbose) Rcpp::Rcout << "\tAttributeSpaces data" << std::endl;
	// important variables
	std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> species_space_dpcoords_MTX;
	std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> species_space_pucoords_MTX;
	std::vector<Eigen::VectorXd> species_space_dpweights_VXD;
	std::vector<Rcpp::IntegerVector> species_space_puids_RIV;
	std::vector<std::size_t> species_space_ndp_INT;
	std::vector<std::size_t> species_space_npu_INT;
	std::vector<std::size_t> species_attributespace_species_INT;
	std::vector<std::size_t> species_attributespace_space_INT;
	
	std::size_t preallocate_INT=n_species_INT*n_attribute_spaces_INT;
	species_space_dpcoords_MTX.reserve(preallocate_INT);
	species_space_pucoords_MTX.reserve(preallocate_INT);
	species_space_dpweights_VXD.reserve(preallocate_INT);
	species_space_puids_RIV.reserve(preallocate_INT);
	species_space_ndp_INT.reserve(preallocate_INT);
	species_space_npu_INT.reserve(preallocate_INT);
	species_attributespace_species_INT.reserve(preallocate_INT);
	species_attributespace_space_INT.reserve(preallocate_INT);
	
	{
		// temporary variables
		std::size_t curr_spp;
		Rcpp::S4 tmp_dp_S4;
		Rcpp::S4 tmp_pu_S4;
		Rcpp::S4 tmp1_S4;
		Rcpp::S4 tmp2_S4;
		Rcpp::List tmp_LST;
		Rcpp::NumericVector currWeights_DBL;
		Rcpp::IntegerVector currIds_INT;
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> tmp1_MTX;
		Rcpp::NumericMatrix tmp2_MTX;
		
		for (std::size_t i=0; i<n_attribute_spaces_INT; ++i) {
			if (verbose) Rcpp::Rcout << "\t\tstarting AttributeSpaces " << i << std::endl;
			// init
			tmp_LST=Rcpp::as<Rcpp::List>(Rcpp::as<Rcpp::S4>(attributespaces_LST[i]).slot("spaces"));
			for (std::size_t j=0; j<tmp_LST.size(); ++j) {
				// init
				if (verbose) Rcpp::Rcout << "\t\t\tstarting species " << j << std::endl;
				tmp1_S4=Rcpp::as<Rcpp::S4>(tmp_LST[j]);
				tmp_pu_S4=Rcpp::as<Rcpp::S4>(tmp1_S4.slot("planning.unit.points"));
				tmp_dp_S4=Rcpp::as<Rcpp::S4>(tmp1_S4.slot("demand.points"));
				curr_spp=Rcpp::as<std::size_t>(tmp1_S4.slot("species"));
				species_attributespace_species_INT.push_back(curr_spp-1);
				species_attributespace_space_INT.push_back(i);

				// planning unit points
				if (verbose) Rcpp::Rcout << "\t\t\tpu points coordinates" << std::endl;
				tmp2_MTX=Rcpp::as<Rcpp::NumericMatrix>(tmp_pu_S4.slot("coords"));
				double *pcv = &tmp2_MTX(0,0);
				Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> tmp1_MTX(pcv, tmp2_MTX.nrow(), tmp2_MTX.ncol());
				species_space_pucoords_MTX.push_back(tmp1_MTX);

				if (verbose) Rcpp::Rcout << "\t\t\tpu points ids " << std::endl;
				Rcpp::IntegerVector tmpvec(tmp_pu_S4.slot("ids"));
				species_space_puids_RIV.push_back(tmpvec-1);

				if (verbose) Rcpp::Rcout << "\t\t\tnumber of planning units" << std::endl;
				species_space_npu_INT.push_back(tmpvec.size());
				
				// demand points
				if (verbose) Rcpp::Rcout << "\t\t\tdemand point coordinates" << std::endl;
				tmp2_MTX=Rcpp::as<Rcpp::NumericMatrix>(tmp_dp_S4.slot("coords"));
				double *pcv2 = &tmp2_MTX(0,0);
				Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> tmp3_MTX(pcv2, tmp2_MTX.nrow(), tmp2_MTX.ncol());
				species_space_dpcoords_MTX.push_back(tmp3_MTX);
				
				if (verbose) Rcpp::Rcout << "\t\t\tdemand point weights" << std::endl;
				Eigen::Map<Eigen::VectorXd> tmpvec2(Rcpp::as<Eigen::Map<Eigen::VectorXd>>(tmp_dp_S4.slot("weights")));
				species_space_dpweights_VXD.push_back(tmpvec2);
				
				if (verbose) Rcpp::Rcout << "\t\t\tnumber of demand points" << std::endl;
				species_space_ndp_INT.push_back(tmpvec2.size());
			}
		}
	}

	species_space_dpcoords_MTX.shrink_to_fit();
	species_space_pucoords_MTX.shrink_to_fit();
	species_space_dpweights_VXD.shrink_to_fit();
	species_space_puids_RIV.shrink_to_fit();
	species_space_ndp_INT.shrink_to_fit();
	species_space_npu_INT.shrink_to_fit();
	species_attributespace_species_INT.shrink_to_fit();
	species_attributespace_space_INT.shrink_to_fit();
	std::size_t n_species_attributespace_INT=species_attributespace_species_INT.size();
		
	// target data
	if (verbose) Rcpp::Rcout << "\ttarget data" << std::endl;
	Rcpp::DataFrame target_DF=Rcpp::as<Rcpp::DataFrame>(data.slot("targets"));
	std::vector<std::size_t> target_DF_species = target_DF["species"];
	std::vector<std::size_t> target_DF_target = target_DF["target"];
	std::vector<double> target_DF_value = target_DF["proportion"];
	std::vector<double> species_space_proptargets_DBL(n_species_attributespace_INT, NAN);
	std::vector<double> species_areatargets_DBL(n_species_INT, 0.0);
	for (std::size_t i=0; i<target_DF_species.size(); ++i) {
		if (target_DF_target[i]==0) {
			species_areatargets_DBL[target_DF_species[i]-1] = target_DF_value[i];
		} else {
			if (!std::isnan(target_DF_value[i])) {
				for (std::size_t a=0, ii=species_attributespace_species_INT[a], jj=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, ii=species_attributespace_species_INT[a], jj=species_attributespace_space_INT[a]) {
					if (ii==(target_DF_species[i]-1) & jj==(target_DF_target[i]-1)) {
						species_space_proptargets_DBL[a]=target_DF_value[i];
						break;
					}
				}
			}
		}
	}
	
	/// preprocessing for area targets
	if (verbose) Rcpp::Rcout << "\tpre-process area targets" << std::endl;
	// ini
	if (verbose) Rcpp::Rcout << "\t\tinitializing" << std::endl;
	double currArea;
	std::vector<double> species_totalarea_DBL(n_species_INT, 0.0);
	std::vector<Rcpp::IntegerVector> species_pu_ids_INT(n_species_INT);
	std::vector<std::vector<double>> species_pu_probs_DBL(n_species_INT);
	std::vector<std::size_t> species_npu_INT(n_species_INT);
	for (std::size_t i=0; i<n_species_INT; ++i) {
		species_pu_probs_DBL[i].reserve(n_pu_INT);
	}

	// calcs
	if (verbose) Rcpp::Rcout << "\t\tcalculate numbers" << std::endl;
	for (std::size_t i=0; i<puvspecies_DF_pu.size(); ++i) {
		// calculate area
		currArea=puvspecies_DF_value[i] * pu_DF_area[puvspecies_DF_pu[i]];
		// sum area occupied by species
		species_totalarea_DBL[puvspecies_DF_species[i]]+=currArea;
		// assign pu to species vector
		species_pu_ids_INT[puvspecies_DF_species[i]].push_back(puvspecies_DF_pu[i]);
		// assign prob to species vector
		species_pu_probs_DBL[puvspecies_DF_species[i]].push_back(puvspecies_DF_value[i]);
	}
	if (verbose) Rcpp::Rcout << "\t\tresize vectors" << std::endl;
	for (std::size_t i=0; i<n_species_INT; ++i) {
		species_pu_probs_DBL[i].shrink_to_fit();
	}
	
	/// create centroid variables
	if (verbose) Rcpp::Rcout << "\tcentroid positions" << std::endl;
	std::vector<Eigen::RowVectorXd> species_space_centroid_VXD(n_species_attributespace_INT);
	for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
		species_space_centroid_VXD[a]=species_space_dpcoords_MTX[a].colwise().sum();
		species_space_centroid_VXD[a]/=static_cast<double>(species_space_ndp_INT[a]);
	}

	if (verbose) Rcpp::Rcout << "\tdistance vars" << std::endl;
	std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> species_space_weightdist_MTX(n_species_attributespace_INT);
	std::vector<double> species_space_tss_DBL(n_species_attributespace_INT);
	{
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> tmp1_MTX;
		Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> tmp2_MTX;
		Eigen::ArrayXd tmp_AXD;
		double currFailDist;
		for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
			/// initialization
			/// preliminary processing
			if (unreliable_formulation) {
				species_space_weightdist_MTX[a].resize(species_space_ndp_INT[a], species_space_npu_INT[a]);
			} else {
				species_space_weightdist_MTX[a].resize(species_space_ndp_INT[a], species_space_npu_INT[a]+1);
			}
			species_space_weightdist_MTX[a].setZero();
			// calculate all distances between pus and dps
			for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
				for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
					tmp_AXD=species_space_pucoords_MTX[a].row(l) - species_space_dpcoords_MTX[a].row(k);
					species_space_weightdist_MTX[a](k,l) = tmp_AXD.square().sum() * species_space_dpweights_VXD[a][k];
				}
			}
			species_space_weightdist_MTX[a].array()+=zero_adjust; 
			// calculate distances for centroid
			species_space_tss_DBL[a]=zero_adjust;
			for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
				tmp_AXD=species_space_centroid_VXD[a] - species_space_dpcoords_MTX[a].row(k);
				species_space_tss_DBL[a]+=(tmp_AXD.square().sum() * species_space_dpweights_VXD[a][k]);				
			}
			// failure pu if reliable formulation
			if (!unreliable_formulation) {
				// failure pu
				currFailDist=species_space_weightdist_MTX[a].maxCoeff()*failure_multiplier_DBL;
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					species_space_weightdist_MTX[a](k,species_space_npu_INT[a])+=currFailDist;
				}
			}
		}
	}

  // resize vectors
  std::vector<std::size_t> species_space_rlevel_INT(n_species_attributespace_INT);
  if (!unreliable_formulation) {
    for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
			species_space_rlevel_INT[a]=std::min(maxrlevel_INT, species_space_npu_INT[a]-1);
		}
	}
	
	

	/// species space probs and transitional probabilities
	if (verbose) Rcpp::Rcout << "\ttransitional probs" << std::endl;
	std::vector<Rcpp::NumericVector> species_space_pu_probs_RDV(n_species_attributespace_INT);
	std::vector<Rcpp::NumericVector> species_space_pu_tprobs_RDV(n_species_attributespace_INT);
	std::vector<Rcpp::IntegerVector> species_space_pupos_RIV(n_species_attributespace_INT);
	if (!unreliable_formulation) {
		for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
			/// initialization
			species_space_pupos_RIV[a]=Rcpp::match(species_space_puids_RIV[a],species_pu_ids_INT[species_attributespace_species_INT[a]])-1;
			// add in real pus
			for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
				species_space_pu_probs_RDV[a].push_back(species_pu_probs_DBL[species_attributespace_species_INT[a]][species_space_pupos_RIV[a][l]]);
				species_space_pu_tprobs_RDV[a].push_back((1.0 - species_space_pu_probs_RDV[a][l]) / species_space_pu_probs_RDV[a][l]);
			}
			// add in failure pu
			species_space_pu_probs_RDV[a].push_back(1.0);
		}
	}
	
  // calculate targets
	if (verbose) Rcpp::Rcout << "\tspace targets" << std::endl;
	std::vector<double> species_space_rawtargets_DBL(n_species_attributespace_INT);
	for (std::size_t a=0; a<n_species_attributespace_INT; ++a)
		species_space_rawtargets_DBL[a] = (species_space_proptargets_DBL[a] - 1.0) * species_space_tss_DBL[a] * -1.0;
	
	// cache integer string conversions
	if (verbose) Rcout << "\tcaching integer/string conversions" << std::endl;
	std::size_t maxINT;
	maxINT=std::max(
		n_pu_INT,
		std::max(
			n_species_INT,
			std::max(
				n_species_attributespace_INT,
				static_cast<std::size_t>(std::accumulate(species_space_ndp_INT.begin(), species_space_ndp_INT.end(), 0))
			)
		)
	) + 2;

	std::vector<std::string> int_STR(maxINT);
	for (std::size_t i=0; i<maxINT; i++)
		int_STR[i] = num2str<std::size_t>(i);

	/// create unordered map with variable names
	if (verbose) Rcout << "\tcreating undordered_map with variable names" << std::endl;
	std::unordered_map<std::string, std::size_t> variable_MAP;
	std::string curr_STR;
	
	// pu vars
	for (std::size_t i=0; i<n_pu_INT; ++i) {
		variable_MAP[pu_DF_id_STR[i]] = counter;
		++counter;
	}

	// pu_pu boundary vars
	if (boundary) {
		for (std::size_t i=0; i<n_edges2_INT; ++i) {
			variable_MAP[boundary_DF_idpair_STR[i]] = counter;
			++counter;
		}
	}

	// space vars
	if (unreliable_formulation) {
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						// Y_var
						curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l];
						variable_MAP[curr_STR] = counter;
						++counter;
					}
				}
			}
		}
	} else {
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							// Y_var
							curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							variable_MAP[curr_STR] = counter;
							++counter;

							// P_var
							curr_STR="P_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							variable_MAP[curr_STR] = counter;
							++counter;

							// W_var
							curr_STR="W_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							variable_MAP[curr_STR] = counter;
							++counter;
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
	std::vector<double> obj_DBL(variable_MAP.size());

	// cost variables
	if (verbose) Rcpp::Rcout << "\t\tcost variables" << std::endl;
	for (std::size_t i=0; i<n_pu_INT; ++i) {
		obj_DBL[variable_MAP[pu_DF_id_STR[i]]] = pu_DF_cost[i];
	}

	// boundary variables
	if (verbose) Rcpp::Rcout << "\t\tboundary variables" << std::endl;
	if (boundary) {
		for (std::size_t i=0; i<n_edges2_INT; ++i) {
			obj_DBL[variable_MAP[boundary_DF_idpair_STR[i]]] = -(blm_DBL * boundary_DF_boundary[edge_pos_INT[i]]);
		}
	}

	/// constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tconstraints" << std::endl;
	std::vector<std::size_t> model_rows_INT;
	std::vector<std::size_t> model_cols_INT;
	std::vector<double> model_vals_DBL;
	std::vector<std::string> sense_STR;
	std::vector<double> rhs_DBL;
	counter=0;

	// preallocate variables storing model matrix
	preallocate_INT=n_pu_INT * n_species_INT * n_attribute_spaces_INT * 
		static_cast<std::size_t>(std::accumulate(species_space_ndp_INT.cbegin(), species_space_ndp_INT.cend(), 0));
  if (!unreliable_formulation) {
		preallocate_INT*=maxrlevel_INT;
  }
	model_rows_INT.reserve(preallocate_INT);
	model_cols_INT.reserve(preallocate_INT);
	model_vals_DBL.reserve(preallocate_INT);
	sense_STR.reserve(preallocate_INT);
	rhs_DBL.reserve(preallocate_INT);

	// area target constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tarea constraints" << std::endl;
	for (std::size_t i=0; i<n_species_INT; ++i) {
		for (std::size_t j=0; j<species_pu_ids_INT[i].size(); ++j) {
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[species_pu_ids_INT[i][j]]]);
			model_vals_DBL.push_back(species_pu_probs_DBL[i][j] * pu_DF_area[species_pu_ids_INT[i][j]]);
		}
		sense_STR.push_back(">=");
		rhs_DBL.push_back(species_totalarea_DBL[i] * species_areatargets_DBL[i]);
		++counter;
	}

	// space target constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tspace constraints" << std::endl;
	if (unreliable_formulation) {
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variable_MAP[curr_STR]);
						model_vals_DBL.push_back(species_space_weightdist_MTX[a](k,l));
					}
				}
				sense_STR.push_back("<=");
				rhs_DBL.push_back(species_space_rawtargets_DBL[a]);
				++counter;
			}
		}
	} else {
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							curr_STR="W_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[curr_STR]);
							model_vals_DBL.push_back(species_space_weightdist_MTX[a](k,l));
						}
					}
				}
				sense_STR.push_back("<=");
				rhs_DBL.push_back(species_space_rawtargets_DBL[a]);
				++counter;
			}
		}
	}
  
	// pu status constraints
	if (verbose) Rcpp::Rcout << "\t\tpu status constraints" << std::endl;
	for (std::size_t i=0; i<n_pu_INT; ++i) {
		if (pu_DF_status[i]==2) {
			// locked in
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[i]]);
			model_vals_DBL.push_back(1);
			sense_STR.push_back("=");
			rhs_DBL.push_back(1.0);
			++counter;
		} else if (pu_DF_status[i]==3) {
			// locked  out
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[i]]);
			model_vals_DBL.push_back(1);
			sense_STR.push_back("=");
			rhs_DBL.push_back(0.0);
			++counter;
		}
	}

	// boundary constraints
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\t\tboundary constraints" << std::endl;
	if (boundary) {
		for (std::size_t i=0; i<n_edges2_INT; ++i) {
			/// constraints if boundary_DF_id1[i] != boundary_DF_id2[i]
			// model+=boundary_DF_idpair_STR[i] + " - " + pu_DF_id_STR[boundary_DF_id1[i]] + " <= 0\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variable_MAP[boundary_DF_idpair_STR[i]]);
			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[boundary_DF_id1[edge_pos_INT[i]]]]);
			model_vals_DBL.push_back(1.0);
			model_vals_DBL.push_back(-1.0);
			sense_STR.push_back("<=");
			rhs_DBL.push_back(0.0);
			++counter;

			// model+=boundary_DF_idpair_STR[i] + " - " + pu_DF_id_STR[boundary_DF_id2[i]] + " <= 0\n";
			model_rows_INT.push_back(counter);
			model_rows_INT.push_back(counter);
			model_cols_INT.push_back(variable_MAP[boundary_DF_idpair_STR[i]]);
			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[boundary_DF_id2[edge_pos_INT[i]]]]);
			model_vals_DBL.push_back(1.0);
			model_vals_DBL.push_back(-1.0);
			sense_STR.push_back("<=");
			rhs_DBL.push_back(0.0);
			++counter;

			// model+=boundary_DF_idpair_STR[i] + " - " +
				// pu_DF_id_STR[boundary_DF_id1[i]] + " - " +
				// pu_DF_id_STR[boundary_DF_id2[i]] + " >= -1\n";
			
			// constraints not strictly needed since decision variables are binary
// 			model_rows_INT.push_back(counter);
// 			model_rows_INT.push_back(counter);
// 			model_rows_INT.push_back(counter);
// 			model_cols_INT.push_back(variable_MAP[boundary_DF_idpair_STR[i]]);
// 			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[boundary_DF_id1[edge_pos_INT[i]]]]);
// 			model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[boundary_DF_id2[edge_pos_INT[i]]]]);
// 			model_vals_DBL.push_back(1);
// 			model_vals_DBL.push_back(-1);
// 			model_vals_DBL.push_back(-1);
// 			sense_STR.push_back(">=");
// 			rhs_DBL.push_back(-1.0);
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
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variable_MAP[curr_STR]);
						model_vals_DBL.push_back(1.0);
					}
					sense_STR.push_back("=");
					rhs_DBL.push_back(1.0);
					++counter;
				}
			}
		}

		// 1c
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\teqn. 1c constraints" << std::endl;
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l];
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variable_MAP[curr_STR]);
						model_vals_DBL.push_back(1.0);

						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[species_space_puids_RIV[a][l]]]);
						model_vals_DBL.push_back(-1.0);

						sense_STR.push_back("<=");
						rhs_DBL.push_back(0.0);
						++counter;
					}
				}
			}
		}
	} else {
		if (verbose) Rcpp::Rcout << "\t\teqn. 1b constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
						
						// original formulation
// 							for (std::size_t l=0; l<(species_space_npu_INT(i,j)+1); ++l) {
// 								curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
// 								model_rows_INT.push_back(counter);
// 								model_cols_INT.push_back(variable_MAP[curr_STR]);
// 								model_vals_DBL.push_back(1.0);
// 							}
// 							for (std::size_t r2=0; r2<r; ++r2) {
// 								curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[species_space_npu_INT(i,j)]+"_"+int_STR[r2];
// 								model_rows_INT.push_back(counter);
// 								model_cols_INT.push_back(variable_MAP[curr_STR]);
// 								model_vals_DBL.push_back(1.0);
// 							}
// 							sense_STR.push_back("=");
// 							rhs_DBL.push_back(1.0);
// 							++counter;
// 						}

						// force each R-level to be assigned to a pu
						for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
							curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[curr_STR]);
							model_vals_DBL.push_back(1.0);
						}
						sense_STR.push_back("=");
						rhs_DBL.push_back(1.0);
						++counter;
					}
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;
		
		// 1b extra - force each pu to be assigned to only 1 r-level
		if (verbose) Rcpp::Rcout << "\t\teqn. 1b (extra) constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[curr_STR]);
							model_vals_DBL.push_back(1.0);
						}
						sense_STR.push_back("<=");
						rhs_DBL.push_back(1.0);
						++counter;
					}
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;
		
		// 1c
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\teqn. 1c constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[curr_STR]);
							model_vals_DBL.push_back(1.0);
						}
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[species_space_puids_RIV[a][l]]]);
						model_vals_DBL.push_back(-1.0);
						sense_STR.push_back("<=");
						rhs_DBL.push_back(0.0);
						++counter;
					}
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;
		
		// 1d
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\teqn. 1d constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
						
						// original formulation
	// 						for (std::size_t r=0; r<(species_space_rlevel_INT(i,j)+1); ++r) {
	// 							curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[species_space_npu_INT(i,j)]+"_"+int_STR[r];
	// 							model_rows_INT.push_back(counter);
	// 							model_cols_INT.push_back(variable_MAP[curr_STR]);
	// 							model_vals_DBL.push_back(1.0);
	// 						}
	// 						sense_STR.push_back("=");
	// 						rhs_DBL.push_back(1.0);
	// 						++counter;
						
					// ensure that failure planning unit is assigned to last r-level
					curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[species_space_npu_INT[a]]+"_"+int_STR[species_space_rlevel_INT[a]];
					model_rows_INT.push_back(counter);
					model_cols_INT.push_back(variable_MAP[curr_STR]);
					model_vals_DBL.push_back(1.0);
					sense_STR.push_back("=");
					rhs_DBL.push_back(1.0);
					++counter;
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;

		// 1e
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\teqn. 1e constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						// real pu
						curr_STR="P_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_0";
						model_rows_INT.push_back(counter);
						model_cols_INT.push_back(variable_MAP[curr_STR]);
						model_vals_DBL.push_back(1.0);

						sense_STR.push_back("=");
						rhs_DBL.push_back(species_space_pu_probs_RDV[a][l]);
						++counter;
					}

					// failure pu
					curr_STR="P_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[species_space_npu_INT[a]]+"_0";
					model_rows_INT.push_back(counter);
					model_cols_INT.push_back(variable_MAP[curr_STR]);
					model_vals_DBL.push_back(1.0);

					sense_STR.push_back("=");
					rhs_DBL.push_back(1.0);
					++counter;
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;

		// 1f
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\teqn. 1f constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=1, r2=0; r<(species_space_rlevel_INT[a]+1); ++r, ++r2) {
							curr_STR="P_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[curr_STR]);
							model_vals_DBL.push_back(1.0);

							for (std::size_t l2=0; l2<species_space_npu_INT[a]; ++l2) {
								curr_STR="W_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l2]+"_"+int_STR[r2];
								model_rows_INT.push_back(counter);
								model_cols_INT.push_back(variable_MAP[curr_STR]);
								model_vals_DBL.push_back(-(species_space_pu_probs_RDV[a][l] * species_space_pu_tprobs_RDV[a][l2]));
							}

							sense_STR.push_back("=");
							rhs_DBL.push_back(0.0);
							++counter;
						}
					}
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;

		/// W variables
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\teqn 2. constraints (rows " << counter << ", ";
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							// init
							currW="W_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							currP="P_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							currY="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							// 2a
							//model+=currW + " - " + currP + " <= 0\n";
							model_rows_INT.push_back(counter);
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[currW]);
							model_cols_INT.push_back(variable_MAP[currP]);
							model_vals_DBL.push_back(1.0);
							model_vals_DBL.push_back(-1.0);
							sense_STR.push_back("<=");
							rhs_DBL.push_back(0.0);
							++counter;
							// 2b
							//model+=currW + " - " + currY + " <= 0\n";
							model_rows_INT.push_back(counter);
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[currW]);
							model_cols_INT.push_back(variable_MAP[currY]);
							model_vals_DBL.push_back(1.0);
							model_vals_DBL.push_back(-1.0);
							sense_STR.push_back("<=");
							rhs_DBL.push_back(0.0);
							++counter;
							// 2c
							//model+=currW + " >= 0\n";
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[currW]);
							model_vals_DBL.push_back(1.0);
							sense_STR.push_back(">=");
							rhs_DBL.push_back(0.0);
							++counter;
							// 2d
							//model+= currW + " - " + currP + " - " + currY + " >= -1\n";
							//model+= currW + " >= " + currP + " + " + currY + " - 1\n";
							model_rows_INT.push_back(counter);
							model_rows_INT.push_back(counter);
							model_rows_INT.push_back(counter);
							model_cols_INT.push_back(variable_MAP[currW]);
							model_cols_INT.push_back(variable_MAP[currP]);
							model_cols_INT.push_back(variable_MAP[currY]);
							model_vals_DBL.push_back(1.0);
							model_vals_DBL.push_back(-1.0);
							model_vals_DBL.push_back(-1.0);
							sense_STR.push_back(">=");
							rhs_DBL.push_back(-1.0);
							++counter;
						}
					}
				}
			}
		}
		if (verbose) Rcpp::Rcout << counter << ")" << std::endl;
	}
	
	////// variable types and bounds
	Rcpp::checkUserInterrupt();
	if (verbose) Rcpp::Rcout << "\tvariable types" << std::endl;
	std::vector<std::string> vtype_STR(obj_DBL.size());
	std::vector<std::size_t> lb_INT(obj_DBL.size(), 0);
	std::vector<std::size_t> ub_INT(obj_DBL.size(), 1);

	///// binary
	if (verbose) Rcpp::Rcout << "\t\t binary vars" << std::endl;

	// 1g
	Rcpp::checkUserInterrupt();
	for (std::size_t i=0; i<n_pu_INT; ++i) {
		vtype_STR[variable_MAP[pu_DF_id_STR[i]]]="B";
	}

	// boundary variables
	Rcpp::checkUserInterrupt();
	if (boundary) {
		for (std::size_t i=0; i<n_edges2_INT; ++i)
			vtype_STR[variable_MAP[boundary_DF_idpair_STR[i]]]="B";
	}

  // 1g constraints
  Rcpp::checkUserInterrupt();
	if (unreliable_formulation) {
		// 1g
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
						curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l];
						vtype_STR[variable_MAP[curr_STR]]="B";
					}
				}
			}
		}
	} else {
		// 1g
		Rcpp::checkUserInterrupt();
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							vtype_STR[variable_MAP[curr_STR]]="B";
						}
					}
				}
			}
		}
		
		///// semi-continuous
		Rcpp::checkUserInterrupt();
		if (verbose) Rcpp::Rcout << "\t\tsemi-continuous vars" << std::endl;
		for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]) {
			if (!std::isnan(species_space_proptargets_DBL[a])) {
				for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
					for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
						for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
							// w variables
							currW="W_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							vtype_STR[variable_MAP[currW]]="S";
							// p variables
							currP="P_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
							vtype_STR[variable_MAP[currP]]="S";
						}
					}
				}
			}
		}
	}
	
  // calculate best space proportions
  if (verbose) Rcpp::Rcout << "\tcalculating best possible space targets" << std::endl;
	std::vector<double> species_space_best_DBL(n_species_attributespace_INT);
	if (unreliable_formulation) {
		for (std::size_t a=0; a<n_species_attributespace_INT; ++a) {
			species_space_best_DBL[a]=1.0-(unreliable_space_value(species_space_weightdist_MTX[a]) / species_space_tss_DBL[a]);
		}
	} else {
		for (std::size_t a=0; a<n_species_attributespace_INT; ++a)
			species_space_best_DBL[a]=1.0-(reliable_space_value(species_space_weightdist_MTX[a],species_space_pu_probs_RDV[a],species_space_rlevel_INT[a]) / species_space_tss_DBL[a]);
	}
	
	// fix variablesSTR
	std::vector<std::string> variables_STR(variable_MAP.size());
	for (auto i : variable_MAP)
		variables_STR[i.second]=i.first;

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
			Rcpp::Named("obj") = Rcpp::wrap(obj_DBL),
			Rcpp::Named("sense") = Rcpp::wrap(sense_STR),
			Rcpp::Named("rhs") = Rcpp::wrap(rhs_DBL),
			Rcpp::Named("vtype") = Rcpp::wrap(vtype_STR),
			Rcpp::Named("lb") = Rcpp::wrap(lb_INT),
			Rcpp::Named("ub") = Rcpp::wrap(ub_INT),
			Rcpp::Named("cache") = Rcpp::List::create(
				Rcpp::Named("species_attributespace_species_INT") = Rcpp::wrap(species_attributespace_species_INT),
				Rcpp::Named("species_attributespace_space_INT") = Rcpp::wrap(species_attributespace_space_INT),
				Rcpp::Named("species_space_pu_probs_RDV") = Rcpp::wrap(species_space_pu_probs_RDV),
				Rcpp::Named("species_space_puids_RIV") = Rcpp::wrap(species_space_puids_RIV),
				Rcpp::Named("species_space_pupos_RIV") = Rcpp::wrap(species_space_pupos_RIV),
				Rcpp::Named("species_areatargets_DBL") = Rcpp::wrap(species_areatargets_DBL),
				Rcpp::Named("species_totalarea_DBL") = Rcpp::wrap(species_totalarea_DBL),
				Rcpp::Named("species_space_rawtargets_DBL") = Rcpp::wrap(species_space_rawtargets_DBL),
				Rcpp::Named("species_space_tss_DBL") = Rcpp::wrap(species_space_tss_DBL),
				Rcpp::Named("species_space_best_DBL") = Rcpp::wrap(species_space_best_DBL),
				Rcpp::Named("species_space_proptargets_DBL") = Rcpp::wrap(species_space_proptargets_DBL),
				Rcpp::Named("species_space_rlevel_INT") = Rcpp::wrap(species_space_rlevel_INT),
				Rcpp::Named("variables") = Rcpp::wrap(variables_STR),
				Rcpp::Named("wdist") = Rcpp::wrap(species_space_weightdist_MTX)
			),
			Rcpp::Named("modelsense") = "min"
		)
	);
}
