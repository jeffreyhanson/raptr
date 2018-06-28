#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


  #include <vector>

// [[Rcpp::export]]

  Rcpp::DataFrame rcpp_Polygons2PolySet(Rcpp::List polys, std::size_t n_preallocate=10000) {
    // init
    /// declare, define, preallocate main vars
    int curr_pid=0;
    int curr_sid=0;
    int curr_pos=0;
    std::vector<int> PID;
    std::vector<int> SID;
    std::vector<int> POS;
    std::vector<double> X;
    std::vector<double> Y;
    PID.reserve(n_preallocate);
    SID.reserve(n_preallocate);
    POS.reserve(n_preallocate);
    X.reserve(n_preallocate);
    Y.reserve(n_preallocate);
    
    // declare temporary vars
    Rcpp::List tmp_list;
    NumericMatrix tmp_matrix;
  
    /// main processing
    for (int i=0; i!=polys.size(); ++i) {
      // set ids
      ++curr_pid;
      curr_sid=0;
      // extract polygons
      tmp_list=Rcpp::as<Rcpp::S4>(polys[i]).slot("Polygons");
      for (int j=0; j!=tmp_list.size(); ++j) {
        // extract polygon
        curr_pos=0;
        ++curr_sid;
        tmp_matrix=Rcpp::as<Rcpp::NumericMatrix>(Rcpp::as<Rcpp::S4>(tmp_list[j]).slot("coords"));
        // store ids and coordinates
        for (int k=0; k!=tmp_matrix.nrow(); ++k) {
          ++curr_pos;
          PID.push_back(curr_pid);
          SID.push_back(curr_sid);
          POS.push_back(curr_pos);
          X.push_back(tmp_matrix(k,0));
          Y.push_back(tmp_matrix(k,1));
        }
        
      }
    }

    /// exports
    Rcpp::DataFrame df=Rcpp::DataFrame::create(
      Rcpp::Named("PID")=PID,
      Rcpp::Named("SID")=SID,
      Rcpp::Named("POS")=POS,
      Rcpp::Named("X")=X,
      Rcpp::Named("Y")=Y
    );
    df.attr("class")=CharacterVector::create("PolySet", "data.frame");
    return(df);
  }
 
