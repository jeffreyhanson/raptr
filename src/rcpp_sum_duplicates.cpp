#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


#include<vector>
#include<algorithm>
#include "functions.h"


struct PairHash {
public:
  template <typename T, typename U>
  std::size_t operator()(const std::pair<T, U> &x) const {
    return std::hash<std::string>()(num2str<T>(x.first) + "_" + num2str<T>(x.second));
  }
};

struct PairEqual{
public:
    template <class T, typename U>
    bool operator()(const std::pair<T, U> &lhs, const std::pair<T, U> &rhs) const {
        return lhs.first == rhs.first && lhs.second == rhs.second;
    }
};

// [[Rcpp::export]]
Rcpp::DataFrame rcpp_sum_duplicates(std::vector<std::size_t> ids1, std::vector<std::size_t> ids2, std::vector<double> boundary) {
  /// init
  std::unordered_map<std::pair<std::size_t,std::size_t>,double,PairHash,PairEqual> bmap;
  bmap.reserve(ids1.size());
  
  /// preliminary processing
  std::pair<std::size_t,std::size_t> key;
  std::unordered_map<std::pair<std::size_t,std::size_t>,double,PairHash,PairEqual>::iterator pos;
  for (std::size_t i=0; i<ids1.size(); ++i) {
    // init
    key=std::make_pair(ids1[i], ids2[i]);
    pos=bmap.find(key);
    if (pos==bmap.end()) {
      // if key not present in map, then add it into the map
      bmap.insert({key,boundary[i]});
    } else {
      // if key is present in the map, then increase boundary value
      pos->second+=boundary[i];
    }
  }
  
  /// main processing
  std::vector<std::size_t> exp_ids1;
  exp_ids1.reserve(ids1.size());
  std::vector<std::size_t> exp_ids2;
  exp_ids2.reserve(ids1.size());
  std::vector<double> exp_boundary;
  exp_boundary.reserve(ids1.size());
  for (auto i=bmap.begin(); i!=bmap.end(); ++i) {
    exp_ids1.push_back(i->first.first);
    exp_ids2.push_back(i->first.second);
    exp_boundary.push_back(i->second);
  }
  
  /// exports
  Rcpp::IntegerVector sexp_ids1=Rcpp::wrap(exp_ids1);
  Rcpp::IntegerVector sexp_ids2=Rcpp::wrap(exp_ids2);
  Rcpp::NumericVector sexp_boundary=Rcpp::wrap(exp_boundary);

  return(
    Rcpp::DataFrame::create(
      Rcpp::Named("id1") = sexp_ids1,
      Rcpp::Named("id2") = sexp_ids2,
      Rcpp::Named("boundary") = sexp_boundary
    )
  );
  
  
}

