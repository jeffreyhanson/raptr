#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(BH)]]

using namespace Rcpp;


#include <vector>
#include <array>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <iomanip>
#include <iostream>
#include <RcppEigen.h>
#include "functions.h"
#include <boost/functional/hash.hpp>

// [[Rcpp::export]]
Rcpp::List rcpp_generate_model_object(Rcpp::S4 opts, bool unreliable_formulation, Rcpp::S4 data, bool verbose) {
  //// Initialization
  if (verbose) Rcpp::Rcout << "Initialization" << std::endl;
  // create variables
  std::size_t counter=0;
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
  std::vector<std::pair<std::size_t, std::size_t>> edge_id_PAIR;
  if (boundary) {
    edge_pos_INT.reserve(n_edges_INT);
    for (std::size_t i=0; i<n_edges_INT; ++i) {
      // convert to base-0 indexing
      boundary_DF_id1[i]-=1;
      boundary_DF_id2[i]-=1;
      // main processing
      if (boundary_DF_id1[i]!=boundary_DF_id2[i]) {
        /// if boundary_DF_id1[i] != boundary_DF_id2[i]
        // store quadratic variable
        edge_id_PAIR.push_back(std::pair<std::size_t,std::size_t>(boundary_DF_id1[i], boundary_DF_id2[i]));
        // cache location of quadratic variable
        edge_pos_INT.push_back(i);
      }
      // increase cost variable with boundary
      pu_DF_cost[boundary_DF_id1[i]] += (blm_DBL * boundary_DF_boundary[i]);
    }
    edge_id_PAIR.shrink_to_fit();
    n_edges2_INT=edge_id_PAIR.size();
  }

  /// attribute.space
  if (verbose) Rcpp::Rcout << "\tattribute space data" << std::endl;
  Rcpp::List attributespaces_LST=Rcpp::as<Rcpp::List>(data.slot("attribute.spaces"));
  std::size_t n_attribute_spaces_INT=attributespaces_LST.size();
  
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
    double *pcv;
    
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
        pcv = &tmp2_MTX(0,0);
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
        pcv = &tmp2_MTX(0,0);
        Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::ColMajor>> tmp3_MTX(pcv, tmp2_MTX.nrow(), tmp2_MTX.ncol());
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
          if ((ii==(target_DF_species[i]-1)) & (jj==(target_DF_target[i]-1))) {
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
  
  /// create unordered map with variable names
  if (verbose) Rcout << "\tcreating undordered_maps with variable names" << std::endl;
  std::unordered_map<std::size_t, std::size_t> pu_MAP;
  std::unordered_map<std::pair<std::size_t,std::size_t>, std::size_t, boost::hash<std::pair<std::size_t,std::size_t>>> edge_MAP;
  std::array<std::size_t,4> curr_uARRAY;
  std::array<std::size_t,5> curr_rARRAY;
  std::unordered_map<std::array<std::size_t, 4>, std::size_t, boost::hash<std::array<std::size_t,4>>> uY_MAP;
  std::unordered_map<std::array<std::size_t, 5>, std::size_t, boost::hash<std::array<std::size_t,5>>> rY_MAP;
  std::unordered_map<std::array<std::size_t, 5>, std::size_t, boost::hash<std::array<std::size_t,5>>> rP_MAP;
  std::unordered_map<std::array<std::size_t, 5>, std::size_t, boost::hash<std::array<std::size_t,5>>> rW_MAP;
  
  // pu vars
  for (std::size_t i=0; i<n_pu_INT; ++i) {
    pu_MAP[i] = counter;
    ++counter;
  }

  // pu_pu boundary vars
  if (boundary) {
    for (std::size_t i=0; i<n_edges2_INT; ++i) {
      edge_MAP[edge_id_PAIR[i]] = counter;
      ++counter;
    }
  }
  
  // space vars
  if (unreliable_formulation) {
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            // Y_var
            curr_uARRAY = {{i, j, k, l}};
            uY_MAP[curr_uARRAY] = counter;
            ++counter;
          }
        }
      }
    }
  } else {
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              // create array
              curr_rARRAY = {{i , j, k, l, r}};
              
              // Y_var
              rY_MAP[curr_rARRAY] = counter;
              ++counter;

              // P_var
              rP_MAP[curr_rARRAY] = counter;
              ++counter;

              // W_var
              rW_MAP[curr_rARRAY] = counter;
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
  std::size_t problem_size_INT;
  if (unreliable_formulation) {
    problem_size_INT = pu_MAP.size() + edge_MAP.size() + uY_MAP.size();
  } else {
    problem_size_INT = pu_MAP.size() + edge_MAP.size() + rY_MAP.size() + rP_MAP.size() + rW_MAP.size();
  }
  std::vector<double> obj_DBL(problem_size_INT);

  // cost variables
  if (verbose) Rcpp::Rcout << "\t\tcost variables" << std::endl;
  for (std::size_t i=0; i<n_pu_INT; ++i) {
    obj_DBL[pu_MAP[i]] = pu_DF_cost[i];
  }

  // boundary variables
  if (verbose) Rcpp::Rcout << "\t\tboundary variables" << std::endl;
  if (boundary) {
    for (std::size_t i=0; i<n_edges2_INT; ++i) {
      obj_DBL[edge_MAP[edge_id_PAIR[i]]] = -(blm_DBL * boundary_DF_boundary[edge_pos_INT[i]]);
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
      model_cols_INT.push_back(pu_MAP[species_pu_ids_INT[i][j]]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            curr_uARRAY={{i, j, k, l}};
            model_rows_INT.push_back(counter);
            model_cols_INT.push_back(uY_MAP[curr_uARRAY]);
            model_vals_DBL.push_back(species_space_weightdist_MTX[a](k,l));
          }
        }
        sense_STR.push_back("<=");
        rhs_DBL.push_back(species_space_rawtargets_DBL[a]);
        ++counter;
      }
    }
  } else {
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              curr_rARRAY = {{i, j, k, l, r}};
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rW_MAP[curr_rARRAY]);
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
      model_cols_INT.push_back(pu_MAP[i]);
      model_vals_DBL.push_back(1);
      sense_STR.push_back("=");
      rhs_DBL.push_back(1.0);
      ++counter;
    } else if (pu_DF_status[i]==3) {
      // locked  out
      model_rows_INT.push_back(counter);
      model_cols_INT.push_back(pu_MAP[i]);
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
      model_cols_INT.push_back(edge_MAP[edge_id_PAIR[i]]);
      model_cols_INT.push_back(pu_MAP[boundary_DF_id1[edge_pos_INT[i]]]);
      model_vals_DBL.push_back(1.0);
      model_vals_DBL.push_back(-1.0);
      sense_STR.push_back("<=");
      rhs_DBL.push_back(0.0);
      ++counter;

      // model+=boundary_DF_idpair_STR[i] + " - " + pu_DF_id_STR[boundary_DF_id2[i]] + " <= 0\n";
      model_rows_INT.push_back(counter);
      model_rows_INT.push_back(counter);
      model_cols_INT.push_back(edge_MAP[edge_id_PAIR[i]]);
      model_cols_INT.push_back(pu_MAP[boundary_DF_id2[edge_pos_INT[i]]]);
      model_vals_DBL.push_back(1.0);
      model_vals_DBL.push_back(-1.0);
      sense_STR.push_back("<=");
      rhs_DBL.push_back(0.0);
      ++counter;

      // model+=boundary_DF_idpair_STR[i] + " - " +
        // pu_DF_id_STR[boundary_DF_id1[i]] + " - " +
        // pu_DF_id_STR[boundary_DF_id2[i]] + " >= -1\n";
      
      // constraints not strictly needed since decision variables are binary
//       model_rows_INT.push_back(counter);
//       model_rows_INT.push_back(counter);
//       model_rows_INT.push_back(counter);
//       model_cols_INT.push_back(variable_MAP[boundary_DF_idpair_STR[i]]);
//       model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[boundary_DF_id1[edge_pos_INT[i]]]]);
//       model_cols_INT.push_back(variable_MAP[pu_DF_id_STR[boundary_DF_id2[edge_pos_INT[i]]]]);
//       model_vals_DBL.push_back(1);
//       model_vals_DBL.push_back(-1);
//       model_vals_DBL.push_back(-1);
//       sense_STR.push_back(">=");
//       rhs_DBL.push_back(-1.0);
//       ++counter;
    }
  }

  if (unreliable_formulation) {
    // 1b
    Rcpp::checkUserInterrupt();
    if (verbose) Rcpp::Rcout << "\t\teqn. 1b constraints" << std::endl;
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            curr_uARRAY = {{i, j, k, l}};
            model_rows_INT.push_back(counter);
            model_cols_INT.push_back(uY_MAP[curr_uARRAY]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            curr_uARRAY = {{i, j, k, l}};
            model_rows_INT.push_back(counter);
            model_cols_INT.push_back(uY_MAP[curr_uARRAY]);
            model_vals_DBL.push_back(1.0);

            model_rows_INT.push_back(counter);
            model_cols_INT.push_back(pu_MAP[species_space_puids_RIV[a][l]]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
            
            // original formulation
//               for (std::size_t l=0; l<(species_space_npu_INT(i,j)+1); ++l) {
//                 curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[l]+"_"+int_STR[r];
//                 model_rows_INT.push_back(counter);
//                 model_cols_INT.push_back(variable_MAP[curr_STR]);
//                 model_vals_DBL.push_back(1.0);
//               }
//               for (std::size_t r2=0; r2<r; ++r2) {
//                 curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[species_space_npu_INT(i,j)]+"_"+int_STR[r2];
//                 model_rows_INT.push_back(counter);
//                 model_cols_INT.push_back(variable_MAP[curr_STR]);
//                 model_vals_DBL.push_back(1.0);
//               }
//               sense_STR.push_back("=");
//               rhs_DBL.push_back(1.0);
//               ++counter;
//             }

            // force each R-level to be assigned to a pu
            for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
              curr_rARRAY = {{i, j, k, l, r}};
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rY_MAP[curr_rARRAY]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              curr_rARRAY = {{i, j, k, l, r}};
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rY_MAP[curr_rARRAY]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              curr_rARRAY = {{i, j, k, l, r}};
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rY_MAP[curr_rARRAY]);
              model_vals_DBL.push_back(1.0);
            }
            model_rows_INT.push_back(counter);
            model_cols_INT.push_back(pu_MAP[species_space_puids_RIV[a][l]]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
            
            // original formulation
  //             for (std::size_t r=0; r<(species_space_rlevel_INT(i,j)+1); ++r) {
  //               curr_STR="Y_"+int_STR[i]+"_"+int_STR[j]+"_"+int_STR[k]+"_"+int_STR[species_space_npu_INT(i,j)]+"_"+int_STR[r];
  //               model_rows_INT.push_back(counter);
  //               model_cols_INT.push_back(variable_MAP[curr_STR]);
  //               model_vals_DBL.push_back(1.0);
  //             }
  //             sense_STR.push_back("=");
  //             rhs_DBL.push_back(1.0);
  //             ++counter;
            
          // ensure that failure planning unit is assigned to last r-level
          curr_rARRAY = {{i, j, k, species_space_npu_INT[a], species_space_rlevel_INT[a]}};
          model_rows_INT.push_back(counter);
          model_cols_INT.push_back(rY_MAP[curr_rARRAY]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            // real pu
            curr_rARRAY = {{i, j, k, l, 0}};
            model_rows_INT.push_back(counter);
            model_cols_INT.push_back(rP_MAP[curr_rARRAY]);
            model_vals_DBL.push_back(1.0);

            sense_STR.push_back("=");
            rhs_DBL.push_back(species_space_pu_probs_RDV[a][l]);
            ++counter;
          }

          // failure pu
          curr_rARRAY= {{i, j, k, species_space_npu_INT[a], 0}};
          model_rows_INT.push_back(counter);
          model_cols_INT.push_back(rP_MAP[curr_rARRAY]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=1, r2=0; r<(species_space_rlevel_INT[a]+1); ++r, ++r2) {
              curr_rARRAY = {{i, j, k, l, r}};
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rP_MAP[curr_rARRAY]);
              model_vals_DBL.push_back(1.0);

              for (std::size_t l2=0; l2<species_space_npu_INT[a]; ++l2) {
                curr_rARRAY= {{i, j, k, l2, r2}};
                model_rows_INT.push_back(counter);
                model_cols_INT.push_back(rW_MAP[curr_rARRAY]);
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
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              // init
              curr_rARRAY = {{i, j, k, l, r}};
              // 2a
              //model+=currW + " - " + currP + " <= 0\n";
              model_rows_INT.push_back(counter);
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rW_MAP[curr_rARRAY]);
              model_cols_INT.push_back(rP_MAP[curr_rARRAY]);
              model_vals_DBL.push_back(1.0);
              model_vals_DBL.push_back(-1.0);
              sense_STR.push_back("<=");
              rhs_DBL.push_back(0.0);
              ++counter;
              // 2b
              //model+=currW + " - " + currY + " <= 0\n";
              model_rows_INT.push_back(counter);
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rW_MAP[curr_rARRAY]);
              model_cols_INT.push_back(rY_MAP[curr_rARRAY]);
              model_vals_DBL.push_back(1.0);
              model_vals_DBL.push_back(-1.0);
              sense_STR.push_back("<=");
              rhs_DBL.push_back(0.0);
              ++counter;
              // 2c
              //model+=currW + " >= 0\n";
              model_rows_INT.push_back(counter);
              model_cols_INT.push_back(rW_MAP[curr_rARRAY]);
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
              model_cols_INT.push_back(rW_MAP[curr_rARRAY]);
              model_cols_INT.push_back(rP_MAP[curr_rARRAY]);
              model_cols_INT.push_back(rY_MAP[curr_rARRAY]);
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
    vtype_STR[pu_MAP[i]]="B";
  }

  // boundary variables
  Rcpp::checkUserInterrupt();
  if (boundary) {
    for (std::size_t i=0; i<n_edges2_INT; ++i)
      vtype_STR[edge_MAP[edge_id_PAIR[i]]]="B";
  }

  // 1g constraints
  Rcpp::checkUserInterrupt();
  if (unreliable_formulation) {
    // 1g
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<species_space_npu_INT[a]; ++l) {
            curr_uARRAY = {{i, j, k, l}};
            vtype_STR[uY_MAP[curr_uARRAY]]="B";
          }
        }
      }
    }
  } else {
    // 1g
    Rcpp::checkUserInterrupt();
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              curr_rARRAY={{i, j, k, l, r}};
              vtype_STR[rY_MAP[curr_rARRAY]]="B";
            }
          }
        }
      }
    }
    
    ///// semi-continuous
    Rcpp::checkUserInterrupt();
    if (verbose) Rcpp::Rcout << "\t\tsemi-continuous vars" << std::endl;
    for (std::size_t a=0, i=species_attributespace_species_INT[a], j=species_attributespace_space_INT[a]; a<n_species_attributespace_INT; ++a) {
      i=species_attributespace_species_INT[a]; j=species_attributespace_space_INT[a];
      if (!std::isnan(species_space_proptargets_DBL[a])) {
        for (std::size_t k=0; k<species_space_ndp_INT[a]; ++k) {
          for (std::size_t l=0; l<(species_space_npu_INT[a]+1); ++l) {
            for (std::size_t r=0; r<(species_space_rlevel_INT[a]+1); ++r) {
              curr_rARRAY = {{i, j, k, l, r}};
              // w variables
              vtype_STR[rW_MAP[curr_rARRAY]]="S";
              // p variables
              vtype_STR[rP_MAP[curr_rARRAY]]="S";
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
  
  // dump all variables to string
  std::vector<std::string> variables_STR(problem_size_INT);
  for (auto i : pu_MAP)
    variables_STR[i.second] = i.first;
  for (auto i : edge_MAP)
    variables_STR[i.second] = "pu_" + num2str<std::size_t>(i.first.first) + "_" + num2str<std::size_t>(i.first.second);
  if (unreliable_formulation) {
    for (auto i : uY_MAP)
      variables_STR[i.second] = "Y_" + num2str<std::size_t>(i.first[0]) + "_" + num2str<std::size_t>(i.first[1]) + "_" + num2str<std::size_t>(i.first[2]) + "_" + num2str<std::size_t>(i.first[3]);
  } else {
    for (auto i : rY_MAP)
      variables_STR[i.second] = "Y_" + num2str<std::size_t>(i.first[0]) + "_" + num2str<std::size_t>(i.first[1]) + "_" + num2str<std::size_t>(i.first[2]) + "_" + num2str<std::size_t>(i.first[3]) + "_" + num2str<std::size_t>(i.first[4]);
    for (auto i : rP_MAP)
      variables_STR[i.second] = "P_" + num2str<std::size_t>(i.first[0]) + "_" + num2str<std::size_t>(i.first[1]) + "_" + num2str<std::size_t>(i.first[2]) + "_" + num2str<std::size_t>(i.first[3]) + "_" + num2str<std::size_t>(i.first[4]);
    for (auto i : rW_MAP)
      variables_STR[i.second] = "W_" + num2str<std::size_t>(i.first[0]) + "_" + num2str<std::size_t>(i.first[1]) + "_" + num2str<std::size_t>(i.first[2]) + "_" + num2str<std::size_t>(i.first[3]) + "_" + num2str<std::size_t>(i.first[4]);
  }
  
  // create pointers to store cache
  std::vector<std::size_t>* species_attributespace_species_INT_PTR = new std::vector<std::size_t>;
  std::vector<std::size_t>* species_attributespace_space_INT_PTR = new std::vector<std::size_t>;
  std::vector<Rcpp::NumericVector>* species_space_pu_probs_RDV_PTR = new std::vector<Rcpp::NumericVector>;
  std::vector<Rcpp::IntegerVector>* species_space_puids_RIV_PTR = new std::vector<Rcpp::IntegerVector>;
  std::vector<Rcpp::IntegerVector>* species_space_pupos_RIV_PTR = new std::vector<Rcpp::IntegerVector>;
  std::vector<double>* species_areatargets_DBL_PTR = new std::vector<double>;
  std::vector<double>* species_totalarea_DBL_PTR = new std::vector<double>;
  std::vector<double>* species_space_rawtargets_DBL_PTR = new std::vector<double>;
  std::vector<double>* species_space_tss_DBL_PTR = new std::vector<double>;
  std::vector<double>* species_space_best_DBL_PTR = new std::vector<double>;
  std::vector<double>* species_space_proptargets_DBL_PTR = new std::vector<double>;
  std::vector<std::size_t>* species_space_rlevel_INT_PTR = new std::vector<std::size_t>;
  std::vector<std::string>* variables_STR_PTR = new std::vector<std::string>;
  std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>* species_space_weightdist_MTX_PTR  = new std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>;
  
  // copy data for preservation
  *species_attributespace_species_INT_PTR = species_attributespace_species_INT;
  *species_attributespace_space_INT_PTR = species_attributespace_space_INT;
  *species_space_puids_RIV_PTR = species_space_puids_RIV;
  *species_space_pu_probs_RDV_PTR = species_space_pu_probs_RDV;
  *species_space_pupos_RIV_PTR = species_space_pupos_RIV;
  *species_areatargets_DBL_PTR = species_areatargets_DBL;
  *species_totalarea_DBL_PTR = species_totalarea_DBL;
  *species_space_rawtargets_DBL_PTR = species_space_rawtargets_DBL;
  *species_space_tss_DBL_PTR = species_space_tss_DBL;
  *species_space_best_DBL_PTR = species_space_best_DBL;
  *species_space_proptargets_DBL_PTR = species_space_proptargets_DBL;
  *species_space_rlevel_INT_PTR = species_space_rlevel_INT;
  *variables_STR_PTR = variables_STR;
  *species_space_weightdist_MTX_PTR = species_space_weightdist_MTX;
  
  // convert pointers to Rcpp::XPters
  Rcpp::XPtr<std::vector<std::size_t>> species_attributespace_species_INT_XPTR(species_attributespace_species_INT_PTR);
  Rcpp::XPtr<std::vector<std::size_t>> species_attributespace_space_INT_XPTR(species_attributespace_space_INT_PTR);
  Rcpp::XPtr<std::vector<Rcpp::NumericVector>> species_space_pu_probs_RDV_XPTR(species_space_pu_probs_RDV_PTR);
  Rcpp::XPtr<std::vector<Rcpp::IntegerVector>> species_space_puids_RIV_XPTR(species_space_puids_RIV_PTR);
  Rcpp::XPtr<std::vector<Rcpp::IntegerVector>> species_space_pupos_RIV_XPTR(species_space_pupos_RIV_PTR);
  Rcpp::XPtr<std::vector<double>> species_areatargets_DBL_XPTR(species_areatargets_DBL_PTR);
  Rcpp::XPtr<std::vector<double>> species_totalarea_DBL_XPTR(species_totalarea_DBL_PTR);
  Rcpp::XPtr<std::vector<double>> species_space_rawtargets_DBL_XPTR(species_space_rawtargets_DBL_PTR);
  Rcpp::XPtr<std::vector<double>> species_space_tss_DBL_XPTR(species_space_tss_DBL_PTR);
  Rcpp::XPtr<std::vector<double>> species_space_best_DBL_XPTR(species_space_best_DBL_PTR);
  Rcpp::XPtr<std::vector<double>> species_space_proptargets_DBL_XPTR(species_space_proptargets_DBL_PTR);
  Rcpp::XPtr<std::vector<std::size_t>> species_space_rlevel_INT_XPTR(species_space_rlevel_INT_PTR);
  Rcpp::XPtr<std::vector<std::string>> variables_STR_XPTR(variables_STR_PTR);
  Rcpp::XPtr<std::vector<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>> species_space_weightdist_MTX_XPTR(species_space_weightdist_MTX_PTR);
  
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
        Rcpp::Named("species_attributespace_species_INT") = species_attributespace_species_INT_XPTR,
        Rcpp::Named("species_attributespace_space_INT") = species_attributespace_space_INT_XPTR,
        Rcpp::Named("species_space_pu_probs_RDV") = species_space_pu_probs_RDV_XPTR,
        Rcpp::Named("species_space_puids_RIV") = species_space_puids_RIV_XPTR,
        Rcpp::Named("species_space_pupos_RIV") = species_space_pupos_RIV_XPTR,
        Rcpp::Named("species_areatargets_DBL") = species_areatargets_DBL_XPTR,
        Rcpp::Named("species_totalarea_DBL") = species_totalarea_DBL_XPTR,
        Rcpp::Named("species_space_rawtargets_DBL") = species_space_rawtargets_DBL_XPTR,
        Rcpp::Named("species_space_tss_DBL") = species_space_tss_DBL_XPTR,
        Rcpp::Named("species_space_best_DBL") = species_space_best_DBL_XPTR,
        Rcpp::Named("species_space_proptargets_DBL") = species_space_proptargets_DBL_XPTR,
        Rcpp::Named("species_space_rlevel_INT") = species_space_rlevel_INT_XPTR,
        Rcpp::Named("variables") = variables_STR_XPTR,
        Rcpp::Named("wdist") = species_space_weightdist_MTX_XPTR
      ),
      Rcpp::Named("modelsense") = "min"
    )
  );
}
