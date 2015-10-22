#include <Rcpp.h>

using namespace Rcpp;


#include<vector>
#include<algorithm>

// [[Rcpp::export]]

	NumericVector rcpp_groupmean(IntegerVector cat_vec, NumericVector val_vec) {
		// init
		IntegerVector ids_vec=na_omit(sort_unique(cat_vec));
		IntegerVector levels_vec=match(cat_vec,ids_vec)-1;
		NumericVector sum_vec(ids_vec.size());
		NumericVector count_vec(ids_vec.size());
		std::vector<double> cpp_mean_vec;
		cpp_mean_vec.reserve(ids_vec.size());
		std::vector<int> cpp_ids_vec;
		cpp_ids_vec.reserve(ids_vec.size());
		std::vector<double> cpp_counts_vec;
		cpp_counts_vec.reserve(ids_vec.size());

		/// main
		// calculate sums
		for (int i=0; i<cat_vec.size(); ++i) {
			if (!IntegerVector::is_na(cat_vec[i]) && !NumericVector::is_na(val_vec[i])) {
				count_vec[levels_vec[i]]+=1.0;
				sum_vec[levels_vec[i]]+=val_vec[i];
			}
		}

		// calculate means
		for (int i=0; i<sum_vec.size(); ++i) {
			if (count_vec[i]>0) {
				cpp_mean_vec.push_back(sum_vec[i]/count_vec[i]);
				cpp_ids_vec.push_back(ids_vec[i]);
				cpp_counts_vec.push_back(count_vec[i]);
			}
		}
		cpp_mean_vec.shrink_to_fit();
		cpp_ids_vec.shrink_to_fit();
		cpp_counts_vec.shrink_to_fit();

		// exports
		NumericVector ret_vec = wrap(cpp_mean_vec);
		ret_vec.attr("ids") = wrap(cpp_ids_vec);
		ret_vec.attr("counts") = wrap(cpp_counts_vec);
		return(ret_vec);
	}
