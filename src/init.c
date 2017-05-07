#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP raptr_rcpp_append_model_object(SEXP, SEXP);
extern SEXP raptr_rcpp_calcBoundaryDF(SEXP, SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_dump_character_object(SEXP);
extern SEXP raptr_rcpp_dump_integer_object(SEXP);
extern SEXP raptr_rcpp_dump_numeric_object(SEXP);
extern SEXP raptr_rcpp_extract_model_object(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_generate_model_object(SEXP, SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_groupcombine(SEXP);
extern SEXP raptr_rcpp_groupmean(SEXP, SEXP);
extern SEXP raptr_rcpp_Polygons2PolySet(SEXP, SEXP);
extern SEXP raptr_rcpp_proportion_held(SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_rrap_proportion_held(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_rrap_squared_distance(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_squared_distance(SEXP, SEXP, SEXP);
extern SEXP raptr_rcpp_sum_duplicates(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"raptr_rcpp_append_model_object",   (DL_FUNC) &raptr_rcpp_append_model_object,   2},
    {"raptr_rcpp_calcBoundaryDF",        (DL_FUNC) &raptr_rcpp_calcBoundaryDF,        4},
    {"raptr_rcpp_dump_character_object", (DL_FUNC) &raptr_rcpp_dump_character_object, 1},
    {"raptr_rcpp_dump_integer_object",   (DL_FUNC) &raptr_rcpp_dump_integer_object,   1},
    {"raptr_rcpp_dump_numeric_object",   (DL_FUNC) &raptr_rcpp_dump_numeric_object,   1},
    {"raptr_rcpp_extract_model_object",  (DL_FUNC) &raptr_rcpp_extract_model_object,  7},
    {"raptr_rcpp_generate_model_object", (DL_FUNC) &raptr_rcpp_generate_model_object, 4},
    {"raptr_rcpp_groupcombine",          (DL_FUNC) &raptr_rcpp_groupcombine,          1},
    {"raptr_rcpp_groupmean",             (DL_FUNC) &raptr_rcpp_groupmean,             2},
    {"raptr_rcpp_Polygons2PolySet",      (DL_FUNC) &raptr_rcpp_Polygons2PolySet,      2},
    {"raptr_rcpp_proportion_held",       (DL_FUNC) &raptr_rcpp_proportion_held,       3},
    {"raptr_rcpp_rrap_proportion_held",  (DL_FUNC) &raptr_rcpp_rrap_proportion_held,  6},
    {"raptr_rcpp_rrap_squared_distance", (DL_FUNC) &raptr_rcpp_rrap_squared_distance, 6},
    {"raptr_rcpp_squared_distance",      (DL_FUNC) &raptr_rcpp_squared_distance,      3},
    {"raptr_rcpp_sum_duplicates",        (DL_FUNC) &raptr_rcpp_sum_duplicates,        3},
    {NULL, NULL, 0}
};

void R_init_raptr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
 
