// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "sharedObject_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// peekSharedMemory
SEXP peekSharedMemory(SEXP x);
RcppExport SEXP _sharedObject_peekSharedMemory(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(peekSharedMemory(x));
    return rcpp_result_gen;
END_RCPP
}
// C_testFunc
SEXP C_testFunc(SEXP a);
RcppExport SEXP _sharedObject_C_testFunc(SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(C_testFunc(a));
    return rcpp_result_gen;
END_RCPP
}
// C_createSharedMemory
DID C_createSharedMemory(SEXP R_x, int type, double total_size, double pid, double did);
RcppExport SEXP _sharedObject_C_createSharedMemory(SEXP R_xSEXP, SEXP typeSEXP, SEXP total_sizeSEXP, SEXP pidSEXP, SEXP didSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_x(R_xSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< double >::type total_size(total_sizeSEXP);
    Rcpp::traits::input_parameter< double >::type pid(pidSEXP);
    Rcpp::traits::input_parameter< double >::type did(didSEXP);
    rcpp_result_gen = Rcpp::wrap(C_createSharedMemory(R_x, type, total_size, pid, did));
    return rcpp_result_gen;
END_RCPP
}
// C_readSharedMemory
SEXP C_readSharedMemory(double R_DID);
RcppExport SEXP _sharedObject_C_readSharedMemory(SEXP R_DIDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type R_DID(R_DIDSEXP);
    rcpp_result_gen = Rcpp::wrap(C_readSharedMemory(R_DID));
    return rcpp_result_gen;
END_RCPP
}
// C_createAltrep
SEXP C_createAltrep(SEXP SM_obj);
RcppExport SEXP _sharedObject_C_createAltrep(SEXP SM_objSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type SM_obj(SM_objSEXP);
    rcpp_result_gen = Rcpp::wrap(C_createAltrep(SM_obj));
    return rcpp_result_gen;
END_RCPP
}
// C_clearObj
void C_clearObj(double did);
RcppExport SEXP _sharedObject_C_clearObj(SEXP didSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type did(didSEXP);
    C_clearObj(did);
    return R_NilValue;
END_RCPP
}
// C_getDataID
std::vector<double> C_getDataID();
RcppExport SEXP _sharedObject_C_getDataID() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(C_getDataID());
    return rcpp_result_gen;
END_RCPP
}
// C_getDataInfo
NumericVector C_getDataInfo(DID did);
RcppExport SEXP _sharedObject_C_getDataInfo(SEXP didSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DID >::type did(didSEXP);
    rcpp_result_gen = Rcpp::wrap(C_getDataInfo(did));
    return rcpp_result_gen;
END_RCPP
}
// C_attachAttr
SEXP C_attachAttr(SEXP R_source, SEXP R_tag, SEXP R_attr);
RcppExport SEXP _sharedObject_C_attachAttr(SEXP R_sourceSEXP, SEXP R_tagSEXP, SEXP R_attrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_source(R_sourceSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_tag(R_tagSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_attr(R_attrSEXP);
    rcpp_result_gen = Rcpp::wrap(C_attachAttr(R_source, R_tag, R_attr));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sharedObject_peekSharedMemory", (DL_FUNC) &_sharedObject_peekSharedMemory, 1},
    {"_sharedObject_C_testFunc", (DL_FUNC) &_sharedObject_C_testFunc, 1},
    {"_sharedObject_C_createSharedMemory", (DL_FUNC) &_sharedObject_C_createSharedMemory, 5},
    {"_sharedObject_C_readSharedMemory", (DL_FUNC) &_sharedObject_C_readSharedMemory, 1},
    {"_sharedObject_C_createAltrep", (DL_FUNC) &_sharedObject_C_createAltrep, 1},
    {"_sharedObject_C_clearObj", (DL_FUNC) &_sharedObject_C_clearObj, 1},
    {"_sharedObject_C_getDataID", (DL_FUNC) &_sharedObject_C_getDataID, 0},
    {"_sharedObject_C_getDataInfo", (DL_FUNC) &_sharedObject_C_getDataInfo, 1},
    {"_sharedObject_C_attachAttr", (DL_FUNC) &_sharedObject_C_attachAttr, 3},
    {NULL, NULL, 0}
};

void init_real_class(DllInfo* dll);
void init_integer_class(DllInfo* dll);
void init_logical_class(DllInfo* dll);
void init_raw_class(DllInfo* dll);
void init_str_class(DllInfo* dll);
RcppExport void R_init_sharedObject(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    init_real_class(dll);
    init_integer_class(dll);
    init_logical_class(dll);
    init_raw_class(dll);
    init_str_class(dll);
}
