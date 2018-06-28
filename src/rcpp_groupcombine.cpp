#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


#include<vector>
#include<algorithm>

// [[Rcpp::export]]

NumericVector rcpp_groupcombine(std::vector<NumericVector> group_means) {
  // init
  std::vector<int> all_ids_vec;
  all_ids_vec.reserve(group_means[0].size() * group_means.size() * 10);
  std::vector<double> all_vals_vec;
  all_vals_vec.reserve(group_means[0].size() * group_means.size() * 10);
  std::vector<double> all_counts_vec;
  all_counts_vec.reserve(group_means[0].size() * group_means.size() * 10);

// preliminary processing
  IntegerVector tmp_ids;
  IntegerVector tmp_counts;
  for (std::size_t i=0; i!=group_means.size(); ++i) {
    tmp_ids=group_means[i].attr("ids");
    tmp_counts=group_means[i].attr("counts");
    for (auto j=tmp_ids.begin(); j!=tmp_ids.end(); ++j)
      all_ids_vec.push_back(*j);
    for (auto j=tmp_counts.begin(); j!=tmp_counts.end(); ++j)
      all_counts_vec.push_back(*j);
    for (auto j=group_means[i].begin(); j!=group_means[i].end(); ++j)
      all_vals_vec.push_back(*j);
  }
  tmp_ids=wrap(all_ids_vec);
  tmp_counts=wrap(all_counts_vec);

  // Main processing
  IntegerVector ids_vec=wrap(na_omit(sort_unique(tmp_ids)));
  IntegerVector levels_vec=match(tmp_ids,ids_vec)-1;

  NumericVector sum_vec(ids_vec.size());
  NumericVector count_vec(ids_vec.size());
  for (std::size_t i=0; i!=all_vals_vec.size(); ++i) {
    sum_vec[levels_vec[i]]+=(all_vals_vec[i] * all_counts_vec[i]);
    count_vec[levels_vec[i]]+=all_counts_vec[i];
  }
  NumericVector ret_vec(ids_vec.size());
  for (std::size_t i=0; i!=static_cast<std::size_t>(ids_vec.size()); ++i)
    ret_vec[i] = sum_vec[i] / count_vec[i];

  // epts
  ret_vec.attr("ids") = ids_vec;
  return(ret_vec);
}
