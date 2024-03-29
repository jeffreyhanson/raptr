// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_dump_numeric_object
SEXP rcpp_dump_numeric_object(SEXP x);
RcppExport SEXP _raptr_rcpp_dump_numeric_object(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_dump_numeric_object(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_dump_integer_object
SEXP rcpp_dump_integer_object(SEXP x);
RcppExport SEXP _raptr_rcpp_dump_integer_object(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_dump_integer_object(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_dump_character_object
SEXP rcpp_dump_character_object(SEXP x);
RcppExport SEXP _raptr_rcpp_dump_character_object(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_dump_character_object(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_Polygons2PolySet
Rcpp::DataFrame rcpp_Polygons2PolySet(Rcpp::List polys, std::size_t n_preallocate);
RcppExport SEXP _raptr_rcpp_Polygons2PolySet(SEXP polysSEXP, SEXP n_preallocateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type polys(polysSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n_preallocate(n_preallocateSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_Polygons2PolySet(polys, n_preallocate));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_append_model_object
Rcpp::List rcpp_append_model_object(Rcpp::List model, Rcpp::List existing_sols);
RcppExport SEXP _raptr_rcpp_append_model_object(SEXP modelSEXP, SEXP existing_solsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type model(modelSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type existing_sols(existing_solsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_append_model_object(model, existing_sols));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_calcBoundaryDF
Rcpp::List rcpp_calcBoundaryDF(Rcpp::DataFrame df, double tolerance, double lengthFactor, double edgeFactor);
RcppExport SEXP _raptr_rcpp_calcBoundaryDF(SEXP dfSEXP, SEXP toleranceSEXP, SEXP lengthFactorSEXP, SEXP edgeFactorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< double >::type lengthFactor(lengthFactorSEXP);
    Rcpp::traits::input_parameter< double >::type edgeFactor(edgeFactorSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_calcBoundaryDF(df, tolerance, lengthFactor, edgeFactor));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_extract_model_object
Rcpp::S4 rcpp_extract_model_object(Rcpp::S4 opts, bool unreliable_formulation, Rcpp::S4 data, Rcpp::List model, std::vector<std::string> logging_file, Rcpp::List solution, bool verbose);
RcppExport SEXP _raptr_rcpp_extract_model_object(SEXP optsSEXP, SEXP unreliable_formulationSEXP, SEXP dataSEXP, SEXP modelSEXP, SEXP logging_fileSEXP, SEXP solutionSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::S4 >::type opts(optsSEXP);
    Rcpp::traits::input_parameter< bool >::type unreliable_formulation(unreliable_formulationSEXP);
    Rcpp::traits::input_parameter< Rcpp::S4 >::type data(dataSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type model(modelSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type logging_file(logging_fileSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type solution(solutionSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_extract_model_object(opts, unreliable_formulation, data, model, logging_file, solution, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_generate_model_object
Rcpp::List rcpp_generate_model_object(Rcpp::S4 opts, bool unreliable_formulation, Rcpp::S4 data, bool verbose);
RcppExport SEXP _raptr_rcpp_generate_model_object(SEXP optsSEXP, SEXP unreliable_formulationSEXP, SEXP dataSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::S4 >::type opts(optsSEXP);
    Rcpp::traits::input_parameter< bool >::type unreliable_formulation(unreliable_formulationSEXP);
    Rcpp::traits::input_parameter< Rcpp::S4 >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_generate_model_object(opts, unreliable_formulation, data, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_groupcombine
NumericVector rcpp_groupcombine(std::vector<NumericVector> group_means);
RcppExport SEXP _raptr_rcpp_groupcombine(SEXP group_meansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<NumericVector> >::type group_means(group_meansSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_groupcombine(group_means));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_groupmean
NumericVector rcpp_groupmean(IntegerVector cat_vec, NumericVector val_vec);
RcppExport SEXP _raptr_rcpp_groupmean(SEXP cat_vecSEXP, SEXP val_vecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type cat_vec(cat_vecSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type val_vec(val_vecSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_groupmean(cat_vec, val_vec));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_proportion_held
double rcpp_proportion_held(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y, Rcpp::NumericVector y_weights);
RcppExport SEXP _raptr_rcpp_proportion_held(SEXP xSEXP, SEXP ySEXP, SEXP y_weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y_weights(y_weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_proportion_held(x, y, y_weights));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_rrap_proportion_held
double rcpp_rrap_proportion_held(Rcpp::NumericMatrix pu_coordinates, Rcpp::NumericVector pu_probabilities, Rcpp::NumericMatrix dp_coordinates, Rcpp::NumericVector dp_weights, double failure_distance, std::size_t maximum_r_level);
RcppExport SEXP _raptr_rcpp_rrap_proportion_held(SEXP pu_coordinatesSEXP, SEXP pu_probabilitiesSEXP, SEXP dp_coordinatesSEXP, SEXP dp_weightsSEXP, SEXP failure_distanceSEXP, SEXP maximum_r_levelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type pu_coordinates(pu_coordinatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type pu_probabilities(pu_probabilitiesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type dp_coordinates(dp_coordinatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dp_weights(dp_weightsSEXP);
    Rcpp::traits::input_parameter< double >::type failure_distance(failure_distanceSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type maximum_r_level(maximum_r_levelSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_rrap_proportion_held(pu_coordinates, pu_probabilities, dp_coordinates, dp_weights, failure_distance, maximum_r_level));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_rrap_squared_distance
std::vector<double> rcpp_rrap_squared_distance(Rcpp::NumericMatrix pu_coordinates, Rcpp::NumericVector pu_probabilities, Rcpp::NumericMatrix dp_coordinates, Rcpp::NumericVector dp_weights, double failure_distance, std::size_t maximum_r_level);
RcppExport SEXP _raptr_rcpp_rrap_squared_distance(SEXP pu_coordinatesSEXP, SEXP pu_probabilitiesSEXP, SEXP dp_coordinatesSEXP, SEXP dp_weightsSEXP, SEXP failure_distanceSEXP, SEXP maximum_r_levelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type pu_coordinates(pu_coordinatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type pu_probabilities(pu_probabilitiesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type dp_coordinates(dp_coordinatesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type dp_weights(dp_weightsSEXP);
    Rcpp::traits::input_parameter< double >::type failure_distance(failure_distanceSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type maximum_r_level(maximum_r_levelSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_rrap_squared_distance(pu_coordinates, pu_probabilities, dp_coordinates, dp_weights, failure_distance, maximum_r_level));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_squared_distance
std::vector<double> rcpp_squared_distance(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y, Rcpp::NumericVector y_weights);
RcppExport SEXP _raptr_rcpp_squared_distance(SEXP xSEXP, SEXP ySEXP, SEXP y_weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y_weights(y_weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_squared_distance(x, y, y_weights));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_sum_duplicates
Rcpp::DataFrame rcpp_sum_duplicates(std::vector<std::size_t> ids1, std::vector<std::size_t> ids2, std::vector<double> boundary);
RcppExport SEXP _raptr_rcpp_sum_duplicates(SEXP ids1SEXP, SEXP ids2SEXP, SEXP boundarySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::size_t> >::type ids1(ids1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::size_t> >::type ids2(ids2SEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type boundary(boundarySEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_sum_duplicates(ids1, ids2, boundary));
    return rcpp_result_gen;
END_RCPP
}
