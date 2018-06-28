#include "functions.h"

std::vector<double> calculateConnectivity(
  std::vector<std::size_t> &id1,
  std::vector<std::size_t> &id2,
  std::vector<double> &boundary,
  IntegerMatrix &selection
) {
  std::vector<double> x(3,0.0);
  for (std::size_t i=0; i<id1.size(); ++i) {
    if (selection(0,id1[i]) + selection(0,id2[i]) == 2) {
      x[0]+=boundary[i];
    } else if (selection(0,id1[i]) + selection(0,id2[i]) == 0) {
      x[1]+=boundary[i];
    } else {
      x[2]+=boundary[i];
    }
  }
  return(x);
}

// unreliable - best space value
double unreliable_space_value(Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &weightdistMTX) {
  return(weightdistMTX.rowwise().minCoeff().sum());
}

// unreliable - space value for a prioritisation with 1 pu
double unreliable_space_value(
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &weightdistMTX,
  std::size_t pu_id
) {
  return(weightdistMTX.col(pu_id).sum());
}

// unreliable - space value for a prioritisation with n pu's
double unreliable_space_value(Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &weightdistMTX,std::vector<std::size_t> &pu_ids) {
  double value=0.0;
  double minWDist;
  for (std::size_t k=0; k<static_cast<std::size_t>(weightdistMTX.rows()); ++k) {
    // get index for planning unit closest to demand point
    minWDist=weightdistMTX(k,pu_ids[0]);
    for (std::size_t m=1; m<pu_ids.size(); ++m) {
      minWDist=std::min(minWDist, weightdistMTX(k,pu_ids[m]));
    }
    // calculate weighted distance value for closest pu
    value+=minWDist;
  }
  return(value);
}


// reliable - best space value
double reliable_space_value(
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &weightdistMTX,
  Rcpp::NumericVector &pu_probs,
  std::size_t maxrlevelINT) {
  double value=0.0;
  double curr_value;
  double currProb;
  std::vector<std::size_t> pu_ids(weightdistMTX.cols()-1);
  std::iota(pu_ids.begin(), pu_ids.end(), 0);
  for (std::size_t k=0; k<static_cast<std::size_t>(weightdistMTX.rows()); ++k) {
    // sort pus in order of distance
    std::partial_sort(
      pu_ids.begin(), pu_ids.begin()+maxrlevelINT, pu_ids.end(),
      [&](const std::size_t p1, const std::size_t p2) {
        return(weightdistMTX(k,p1) < weightdistMTX(k,p2));
      }
    );
    // calculate expected weighted distances for real pus
    currProb=1.0;
    curr_value=0.0;
    for (std::size_t r=0; r<maxrlevelINT; ++r) {
      curr_value+=(pu_probs[pu_ids[r]] * currProb * weightdistMTX(k,pu_ids[r]));
      currProb*=(1.0 - pu_probs[pu_ids[r]]);
    }
    // calculate expected weighted distance for failure PU
    curr_value+=(currProb*weightdistMTX(k, weightdistMTX.cols()-1));
    // update value
    value+=curr_value;
  }
  return(value);
}

// reliable - space value for a prioritisation containing 1 pu
double reliable_space_value(
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &weightdistMTX,
  std::size_t pu_id,
  double pu_prob,
  double sum_failure_wdists
) {
  return(
      (weightdistMTX.col(pu_id).sum() * pu_prob) +
      (sum_failure_wdists * (1.0-pu_prob))
  );
}

// reliable - space value for a prioritisation containing n pu's
double reliable_space_value(
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> &weightdistMTX,
  std::vector<std::size_t> pu_ids,
  Rcpp::NumericVector &pu_probs,
  std::size_t maxrlevelINT) {
  double value=0.0;
  double currProb;
  double curr_value;
  
  
  for (std::size_t k=0; k<static_cast<std::size_t>(weightdistMTX.rows()); ++k) {
    // sort pus in order of distance
    std::partial_sort(
      pu_ids.begin(), pu_ids.begin()+maxrlevelINT, pu_ids.end(),
      [&](const std::size_t p1, const std::size_t p2) {
        return(weightdistMTX(k,p1) < weightdistMTX(k,p2));
      }
    );
    // calculate expected weighted distances for real pus
    currProb=1.0;
    curr_value=0.0;
    for (std::size_t r=0; r<maxrlevelINT; ++r) {
      curr_value+=(pu_probs[pu_ids[r]] * currProb * weightdistMTX(k,pu_ids[r]));
      currProb*=(1.0 - pu_probs[pu_ids[r]]);
    }
    // calculate expected weighted distance for failure PU
    curr_value+=(currProb*weightdistMTX(k, weightdistMTX.cols()-1));
    // update value
    value += curr_value;
  }
  return(value);
}

// calculate euclidean distance
double distance(double x0, double y0, double x1, double y1) {
  return(sqrt(std::abs(Pow<2>(x0-x1)) + std::abs(Pow<2>(y0-y1))));
}


